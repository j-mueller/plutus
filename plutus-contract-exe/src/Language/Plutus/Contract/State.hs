{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | A version of 'Language.Plutus.Contract.Contract' that
--   writes checkpoints
module Language.Plutus.Contract.State where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Prompt              (MonadPrompt (..))
import           Control.Monad.Writer
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Types                  as Aeson
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Foldable                     (toList)

import qualified Language.Plutus.Contract.Contract as C
import           Language.Plutus.Contract.Event    as Event
import           Language.Plutus.Contract.Hooks    as Hooks
import           Language.Plutus.Contract.Record

{- Note [Handling state in contracts]

If we look at 'runContract' in 'Language.Plutus.Contract.Contract', we can see
that it takes a list of events and runs the contract as far as possible, until
either the events are depleted or the contract is done. That means we can take
the current state of the contract to be represented by the type '[Event]'. When
a new event comes in, we add it to the previous state, and apply 'runContract' 
again.

This is nice because '[Event]' is a type we can serialise (which we 
would like to do because our contract should expose a stateless interface to 
the outside world, and so we need to be able to recover the state from its 
serialised form), and at the same time we can write our contracts using the full
power of the 'ContractPrompt' interface, ie. monad, applicative, etc. In 
addition, by restricting the contract's effects to what comes down to different 
forms of `Prompt`, we get completely deterministic execution of contracts.

There is a drawback however. The list-of-events representation of the state 
means we have to replay all previous events whenever we want to process a new 
event, so 'runContract' uses time proportional to the number of events that 
have been seen.

Can we do better than '[Event]' without compromising on the expressiveness of 
the 'Contract' interface? It turns out we can, by pruning the event sequence 
whenever the contract has arrived at a state that we know how to serialise to 
something better than '[Event]'. This is the purpose of the 'StatefulContract' 
type and the functions in this module, and the 'Record' type in 
'Language.Plutus.Contract.Record'.

The main problem with serialising the contract state to anything other than '
[Event]' is that the state may contain arbitrary callbacks introduced by 
the 'Control.Monad.PromptT' type. The only time we can be certain that a 
contract has no callbacks that it is waiting for is when 'runContract' (with 'f 
= Maybe') evaluates to 'Just a'. At that point, the 'a' represents exactly the 
same state as the list of events, '[Event]', that was used to produce the 'a'. 
So if we want to serialise the state now, we can just serialise the 'a' and 
drop all the events.

This is complicated by the fact that contracts may have multiple branches that 
run at the same time (using '<*>') or one after another (with '>>='). So for a
contract 'l <*> r', if the 'r' branch is done but the 'l' branch is still 
waiting for input, we can only drop the events that have been seen by 'r', and 
we must continue to use the '[Event]' representation for the state of 'l' until 
both 'l' and 'r' are finished. 

To account for this, we store the events in a 
'Language.Plutus.Contract.Record.Record' instead of a list. The record is a 
tree that mirrors the structure of the contract, and whenever one of the 
contract's branches is done we can serialise its state and store it in the 
correct location in the record.

Finally we need to know how to serialise the result value of the (branch of a) contract when it's done. This is what the 'StatefulContract' type is for. We can
use its 'CJSONCheckpoint' constructor to inject the serialisation constraints at
arbitrary points in the contract. So we could write the previous example as
'l <*> jsonCheckpoint r' to ensure that the events consumed by the 'r' branch
are discarded as soon as 'r' produces a result.

To actually run a 'StatefulContract', we need to
* use 'initialise' to get the initial state (of type 'Record') of the contract
* for each new event, run 'insertAndUpdate' with the current state and the event
  to get the new state. 

TODO: There is a lot of boilerplate in the definitions of 'runClosed' and 
'runOpen' - the cases we have to cover are all possible combinations of the
constructors of 'StatefulContract' and 'Record'. 

I did attempt to reduce this by using 'newtype Rec i a = Rec Either (OpenRecord i) (ClosedRecord i, a)' and variations but always ran into trouble. I would be 
very grateful for suggestions on how to improve this situation.

-}

data StatefulContract a where
    CMap :: (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CAp :: StatefulContract  (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CBind :: StatefulContract  a' -> (a' -> StatefulContract  a) -> StatefulContract  a

    CContract :: C.ContractPrompt Maybe a -> StatefulContract  a
    CJSONCheckpoint :: (Aeson.FromJSON a, Aeson.ToJSON a) => StatefulContract  a -> StatefulContract  a

initialise
    :: ( MonadWriter Hooks m )
    => StatefulContract a
    -> m (Either (OpenRecord Event) (ClosedRecord Event, a))
initialise = \case
    CMap f con -> fmap (fmap f) <$> initialise con
    CAp conL conR -> do
        l' <- initialise conL
        r' <- initialise conR
        case (l', r') of
            (Left l, Left r)             -> pure $ Left (OpenBoth l r)
            (Right (l, _), Left r)       -> pure $ Left (OpenRight l r)
            (Left l, Right (r, _))       -> pure $ Left (OpenLeft l r)
            (Right (l, f), Right (r, a)) -> pure $ Right (ClosedBin l r, f a)
    CBind c f -> do
        l <- initialise c
        case l of
            Left l' -> pure $ Left (OpenBind l')
            Right (l', a) -> do
                r <- initialise (f a)
                case r of
                    Left r'       -> pure $ Left $ OpenRight l' r'
                    Right (r', b) -> pure $ Right (ClosedBin l' r', b)
    CContract con -> do
        r <- C.runConM mempty con
        case r of
            Nothing -> pure $ Left $ OpenLeaf mempty
            Just a  -> pure $ Right (ClosedLeaf (FinalEvents mempty), a)
    CJSONCheckpoint con -> do
        r <- initialise con
        case r of
            Left _       -> pure r
            Right (_, a) -> pure $ Right (jsonLeaf a, a)

checkpoint :: (Aeson.FromJSON a, Aeson.ToJSON a) => StatefulContract  a -> StatefulContract  a
checkpoint = CJSONCheckpoint

prtty :: StatefulContract  a -> String
prtty = \case
    CMap _ c -> "cmap (" ++ prtty c ++ ")"
    CAp l r -> "cap (" ++ prtty l ++ ") (" ++ prtty r ++ ")"
    CBind l _ -> "cbind (" ++ prtty l ++  ") f"
    CContract _ -> "ccontract"
    CJSONCheckpoint j -> "json(" ++ prtty j ++ ")"

instance Functor StatefulContract where
    fmap = CMap

instance Applicative StatefulContract where
    pure = CContract . pure
    (<*>) = CAp

-- TODO: Should we add an `Alt` constructor to `StatefulContract`?
instance Alternative StatefulContract where
    empty = CContract empty
    l <|> r = CContract (lower l <|> lower r)

instance Monad StatefulContract where
    (>>=) = CBind

instance MonadPrompt (Hook ()) Event StatefulContract where
    prompt = CContract . prompt

lowerM
    :: (Monad m)
    -- ^ What to do with map, ap, bind
    => (forall a'. (Aeson.FromJSON a', Aeson.ToJSON a') => m a' -> m a')
    -- ^ What to do with JSON checkpoints
    -> (forall a'. C.ContractPrompt Maybe a' -> m a')
    -- ^ What to do with the contracts
    -> StatefulContract a
    -> m a
lowerM fj fc = \case
    CMap f c' -> f <$> lowerM fj fc c'
    CAp l r -> lowerM fj fc l <*> lowerM fj fc r
    CBind c' f -> lowerM fj fc c' >>= fmap (lowerM fj fc) f
    CContract c' -> fc c'
    CJSONCheckpoint c' -> fj (lowerM fj fc c')

lower :: StatefulContract a -> C.ContractPrompt Maybe a
lower = lowerM id id

runClosed
    :: ( MonadWriter Hooks m
       , MonadError String m)
    => StatefulContract a
    -> ClosedRecord Event
    -> m a
runClosed con rc =
    case con of
        CMap f c' -> fmap f (runClosed c' rc)
        _ -> case rc of
                ClosedLeaf (FinalEvents evts) ->
                    case con of
                        CContract con' -> do
                            r <- C.runConM (toList evts) con'
                            case r of
                                Nothing -> throwError "ClosedLeaf, contract not finished"
                                Just  a -> pure a
                        _ -> throwError "ClosedLeaf, expected CContract "
                ClosedLeaf (FinalJSON vl) ->
                    case con of
                        CJSONCheckpoint _ ->
                            case Aeson.parseEither Aeson.parseJSON vl of
                                Left e    -> throwError e
                                Right vl' -> writer (vl', mempty)
                        _ -> throwError ("Expected JSON checkpoint, got " ++ prtty con)
                ClosedBin l r ->
                    case con of
                        CMap f con' -> fmap f (runClosed con' (ClosedBin l r))
                        CAp l' r'   -> runClosed l' l <*> runClosed r' l
                        CBind l' f  -> runClosed l' l >>= flip runClosed r . f
                        _           -> throwError "ClosedBin with wrong contract type"

runOpen
    :: ( MonadWriter Hooks m
       , MonadError String m)
    => StatefulContract a
    -> OpenRecord Event
    -> m (Either (OpenRecord Event) (ClosedRecord Event, a))
runOpen con opr =
    case (con, opr) of
        (CMap f con', _) -> (fmap .fmap $ fmap f) (runOpen con' opr)
        (CAp l r, OpenLeft opr' cr) -> do
            lr <- runOpen l opr'
            rr <- runClosed r cr
            case lr of
                Left opr''     -> pure (Left (OpenLeft opr'' cr))
                Right (cr', a) -> pure (Right (ClosedBin cr' cr, a rr))
        (CAp l r, OpenRight cr opr') -> do
            lr <- runClosed l cr
            rr <- runOpen r opr'
            case rr of
                Left opr''     -> pure (Left (OpenRight cr opr''))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', lr a))
        (CAp l r, OpenBoth orL orR) -> do
            lr <- runOpen l orL
            rr <- runOpen r orR
            case (lr, rr) of
                (Right (crL, a), Right (crR, b)) ->
                    pure (Right (ClosedBin crL crR, a b))
                (Right (crL, _), Left oR) ->
                    pure (Left (OpenRight crL oR))
                (Left oL, Right (cR, _)) ->
                    pure (Left (OpenLeft oL cR))
                (Left oL, Left oR) ->
                    pure (Left (OpenBoth oL oR))
        (CAp{}, OpenLeaf _) -> throwError "CAp OpenLeaf"

        (CBind c f, OpenBind bnd) -> do
            lr <- runOpen c bnd
            case lr of
                Left orL' -> pure (Left $ OpenBind orL')
                Right (crL, a) -> do
                    let con' = f a
                    orR' <- initialise con'
                    case orR' of
                        Right (crrrr, a') -> pure (Right (ClosedBin crL crrrr, a'))
                        Left orrrr -> do
                            rr <- runOpen con' orrrr
                            case rr of
                                Left orR'' ->
                                    pure (Left (OpenRight crL orR''))
                                Right (crR, a') ->
                                    pure (Right (ClosedBin crL crR, a'))

        (CBind c f, OpenRight cr opr') -> do
            lr <- runClosed c cr
            rr <- runOpen (f lr) opr'
            case rr of
                Left opr''     -> pure (Left (OpenRight cr opr''))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', a))
        (CBind{}, _) -> throwError $ "CBind " ++ show opr

        (CContract con', OpenLeaf is) -> do
                r <- C.runConM (toList is) con'
                case r of
                    Just a  -> pure (Right (ClosedLeaf (FinalEvents is), a))
                    Nothing -> pure (Left (OpenLeaf is))
        (CContract{}, _) -> throwError $ "CContract non leaf " ++ show opr

        (CJSONCheckpoint con', opr') ->
            fmap (\(_, a) -> (jsonLeaf a, a)) <$> runOpen con' opr'
        _ -> throwError "runOpen"

insertAndUpdate
    :: StatefulContract a
    -> Record Event
    -> Event
    -> Either String (Record Event, Hooks)
insertAndUpdate con rc e = updateRecord con (insert e rc)

updateRecord
    :: StatefulContract  a
    -> Record Event
    -> Either String (Record Event, Hooks)
updateRecord con rc =
    case rc of
        Right cl ->
            fmap (first $ const $ Right cl)
            $ runExcept
            $ runWriterT
            $ runClosed con cl
        Left cl  ->
            fmap (first (fmap fst))
            $ runExcept
            $ runWriterT
            $ runOpen con cl
