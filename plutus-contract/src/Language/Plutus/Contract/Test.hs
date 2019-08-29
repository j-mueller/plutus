{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- | Testing contracts with HUnit and Tasty
module Language.Plutus.Contract.Test(
      module X
    , TracePredicate
    , Language.Plutus.Contract.Test.not
    , endpointAvailable
    , interestingAddress
    , assertResult
    , assertHooks
    , assertRecord
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , waitingForSlot
    , walletState
    , walletWatchingAddress
    , emulatorLog
    -- * Checking predicates
    , checkPredicate
    ) where

import           Control.Lens                                    (at, folded, to, view, (^.))
import           Control.Monad.Writer                            (MonadWriter (..), Writer, runWriter)
import           Data.Foldable                                   (toList, traverse_)
import           Data.Functor.Contravariant                      (Contravariant (..), Op (..))
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy(..))
import           Data.Row
import           Data.Semigroup                                  (Min)
import           Data.Sequence                                   (Seq)
import qualified Data.Sequence                                   as Seq
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           GHC.TypeLits                                    (Symbol, KnownSymbol, symbolVal)
import qualified Test.Tasty.HUnit                                as HUnit
import           Test.Tasty.Providers                            (TestTree)

import           Language.Plutus.Contract                        (Contract)
import           Language.Plutus.Contract.Record                 (Record)
import           Language.Plutus.Contract.Resumable              (ResumableError)
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Tx                     (UnbalancedTx)

import qualified Language.Plutus.Contract.Effects.AwaitSlot      as AwaitSlot
import           Language.Plutus.Contract.Effects.ExposeEndpoint (EndpointDescription)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoints
import qualified Language.Plutus.Contract.Effects.WatchAddress   as WatchAddress
import qualified Language.Plutus.Contract.Effects.WriteTx        as WriteTx

import qualified Ledger.Ada                                      as Ada
import qualified Ledger.AddressMap                               as AM
import           Ledger.Slot                                     (Slot)
import           Ledger.Tx                                       (Address)
import           Ledger.Value                                    (Value)
import qualified Ledger.Value                                    as V
import           Wallet.Emulator                                 (EmulatorAction, EmulatorEvent, Wallet)
import qualified Wallet.Emulator                                 as EM

import Language.Plutus.Contract.Events (Event(..), Hooks(..))
import           Language.Plutus.Contract.Trace                  as X

newtype PredF f a = PredF { unPredF :: a -> f Bool }
    deriving Contravariant via (Op (f Bool))

instance Applicative f => Semigroup (PredF f a) where
    l <> r = PredF $ \a -> (&&) <$> unPredF l a <*> unPredF r a

instance Applicative f => Monoid (PredF f a) where
    mappend = (<>)
    mempty = PredF $ const (pure True)

type TracePredicate i o a = PredF (Writer (Seq String)) (InitialDistribution, ContractTraceResult i o a)

hooks
    :: ( Forall o Monoid
       , Forall o Semigroup
       , AllUniqueLabels o
       )
    => Wallet
    -> ContractTraceResult i o a
    -> Hooks o
hooks w rs =
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in either (const mempty) id (State.execResumable evts con)

record 
    :: forall i o a.
       ( AllUniqueLabels o
       , Forall o Semigroup
       , Forall o Monoid
       )
    => Wallet 
    -> ContractTraceResult i o a
    -> Either ResumableError (Record (Event i))
record w rs =
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in fmap (fmap fst . fst) (State.runResumable evts con)

not :: TracePredicate i o a -> TracePredicate i o a
not = PredF . fmap (fmap Prelude.not) . unPredF

checkPredicate
    :: forall i o a. 
       String
    -> Contract i o a
    -> TracePredicate i o a
    -> ContractTrace i o EmulatorAction a ()
    -> TestTree
checkPredicate nm con predicate action =
    HUnit.testCaseSteps nm $ \step ->
        case runTrace con action of
            (Left err, _) ->
                HUnit.assertFailure $ "EmulatorAction failed. " ++ show err
            (Right (_, st), ms) -> do
                let dt = ContractTraceResult ms st
                    (result, emLog) = runWriter $ unPredF predicate (defaultDist, dt)
                if result then pure () else traverse_ step emLog
                HUnit.assertBool nm result

endpointAvailable
    :: forall (s :: Symbol) i o a.
       ( HasType s (Set EndpointDescription) o
       , KnownSymbol s
       , AllUniqueLabels o
       , Forall o Monoid
       , Forall o Semigroup
       )
    => Wallet
    -> TracePredicate i o a
endpointAvailable w = PredF $ \(_, r) -> do
    if Endpoints.isActive @s (hooks w r)
    then pure True
    else do
        tellSeq ["missing endpoint:" ++ symbolVal (Proxy :: Proxy s)]
        pure False

interestingAddress
    :: forall i o a.
       ( WatchAddress.AddressPrompt i o )
    => Wallet
    -> Address
    -> TracePredicate i o a
interestingAddress w addr = PredF $ \(_, r) -> do
    let hks = WatchAddress.addresses (hooks w r)
    if addr `Set.member` hks
    then pure True
    else do
        tellSeq ["Interesting addresses:", unlines (show <$> toList hks), "missing address:", show addr]
        pure False

tx
    :: forall i o a.
       ( HasType "tx" [UnbalancedTx] o
       , AllUniqueLabels o
       , Forall o Monoid
       , Forall o Semigroup)
    => Wallet
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate i o a
tx w flt nm = PredF $ \(_, r) -> do
    let hks = WriteTx.transactions (hooks w r)
    if any flt hks
    then pure True
    else do
        tellSeq ["Unbalanced transactions;", unlines (fmap show hks), "No transaction with '" <> nm <> "'"]
        pure False

walletState 
    :: forall i o a.
       Wallet 
    -> (EM.WalletState -> Bool) 
    -> String 
    -> TracePredicate i o a
walletState w flt nm = PredF $ \(_, r) -> do
    let ws = view (at w) $ EM._walletStates $  _ctrEmulatorState r
    case ws of
        Nothing -> do
            tellSeq ["Wallet state of '" <> show w <> "' not found"]
            pure False
        Just st ->
            if flt st
            then pure True
            else do
                tellSeq ["Wallet state of " <> show w <> ":", show st, "Fails '" <> nm <> "'"]
                pure False

walletWatchingAddress 
    :: forall i o a.
       Wallet
    -> Address
    -> TracePredicate i o a
walletWatchingAddress w addr =
    let desc = "watching address " <> show addr in
    walletState w (Map.member addr . AM.values . view EM.addressMap) desc

assertEvents 
    :: forall i o a.
       (Forall i Show)
    => Wallet
    -> ([Event i] -> Bool)
    -> String
    -> TracePredicate i o a
assertEvents w pr nm = PredF $ \(_, r) -> do
    let es = fmap toList (view (ctsEvents . at w) $ _ctrTraceState r)
    case es of
        Nothing -> do
            tellSeq ["Event log for '" <> show w <> "' not found"]
            pure False
        Just lg ->
            if pr lg
            then pure True
            else do
                tellSeq ["Event log for '" <> show w <> ":", unlines (fmap show lg), "Fails '" <> nm <> "'"]
                pure False

waitingForSlot
    :: forall i o a.
       ( HasType "slot" (Maybe (Min Slot)) o
       , AllUniqueLabels o
       , Forall o Monoid
       , Forall o Semigroup
       )
    => Wallet
    -> Slot
    -> TracePredicate i o a
waitingForSlot w sl = PredF $ \(_, r) ->
    case AwaitSlot.nextSlot (hooks w r) of
        Nothing -> do
            tellSeq [show w <> " not waiting for any slot notifications. Expected: " <>  show sl]
            pure False
        Just sl' ->
            if sl == sl'
            then pure True
            else do
                tellSeq [show w <> " waiting for " <> show sl', "Expected: " <> show sl]
                pure False

emulatorLog
    :: forall i o a. 
       ()
    => ([EmulatorEvent] -> Bool)
    -> String
    -> TracePredicate i o a
emulatorLog f nm = PredF $ \(_, r) ->
    let lg = EM._emulatorLog $ _ctrEmulatorState r in
    if f lg
    then pure True
    else do
        tellSeq ["Emulator log:", unlines (fmap show lg), "Fails '" <> nm <> "'"]
        pure False

anyTx
    :: forall i o a. 
       ( HasType "tx" [UnbalancedTx] o
       , AllUniqueLabels o
       , Forall o Monoid
       , Forall o Semigroup
       )
    => Wallet
    -> TracePredicate i o a
anyTx w = tx w (const True) "anyTx"

assertHooks 
    :: forall i o a. 
       ( AllUniqueLabels o 
       , Forall o Monoid
       , Forall o Semigroup
       , Forall o Show
       )
    => Wallet
    -> (Hooks o -> Bool)
    -> String
    -> TracePredicate i o a
assertHooks w p nm = PredF $ \(_, rs) ->
    let hks = hooks w rs in
    if p hks
    then pure True
    else do
        tellSeq ["Hooks:", show hks, "Failed '" <> nm <> "'"]
        pure False

assertRecord 
    :: forall i o a. 
       ( Forall i Show
       , Forall o Semigroup
       , Forall o Monoid
       , AllUniqueLabels o
       )
    => Wallet 
    -> (Record (Event i) -> Bool)
    -> String
    -> TracePredicate i o a
assertRecord w p nm = PredF $ \(_, rs) ->
    case record w rs of
        Right r
            | p r -> pure True
            | otherwise -> do
                tellSeq ["Record: ", show r, "Failed '" <> nm <> "'"]
                pure False
        Left err -> do
            tellSeq ["Record failed with", show err, "in '" <> nm <> "'"]
            pure False

assertResult
    :: forall i o a. 
       ( Forall i Show
       , AllUniqueLabels o
       , Forall o Semigroup
       , Forall o Monoid
       )
    => Wallet
    -> (Maybe a -> Bool)
    -> String
    -> TracePredicate i o a
assertResult w p nm = PredF $ \(_, rs) ->
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
        result = State.runResumable evts con
    in
        case fmap fst result of
            Left err
                | p Nothing -> pure True
                | otherwise -> do
                    tellSeq ["Resumable error", show err, "in '" <> nm <> "'"]
                    pure False
            Right (Left openRec)
                | p Nothing -> pure True
                | otherwise -> do
                    tellSeq ["Open record", show openRec, "in '" <> nm <> "'"]
                    pure False
            Right (Right (closedRec, a))
                | p (Just a) -> pure True
                | otherwise -> do
                    tellSeq ["Closed record", show closedRec, "failed with '" <> nm <> "'"]
                    pure False

walletFundsChange
    :: forall i o a. 
       ()
    => Wallet
    -> Value
    -> TracePredicate i o a
walletFundsChange w dlt = PredF $ \(initialDist, ContractTraceResult{_ctrEmulatorState = st}) ->
        let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in if initialValue `V.plus` dlt == finalValue
        then pure True
        else do
            tellSeq ["Expected funds to change by", show dlt, "but they changed by", show (finalValue `V.minus` initialValue)]
            pure False

tellSeq :: MonadWriter (Seq a) m => [a] -> m ()
tellSeq = tell . Seq.fromList
