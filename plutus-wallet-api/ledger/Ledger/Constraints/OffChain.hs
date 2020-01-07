{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Ledger.Constraints.OffChain where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Foldable                    (traverse_)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Semigroup                   (First (..))
import qualified Data.Set                         as Set
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                     (Generic)
import           IOTS                             (IotsType)

import           Language.PlutusTx                (IsData (..))
import           Language.PlutusTx.Lattice
import qualified Language.PlutusTx.Numeric        as N

import           Ledger.Address                   (Address(..))
import qualified Ledger.Address                   as Address
import           Ledger.Constraints.TxConstraints hiding (requiredSignatories)
import           Ledger.Crypto                    (PubKeyHash)
import           Ledger.Scripts                   (DataValue (..), DataValueHash, MonetaryPolicy, MonetaryPolicyHash,
                                                   Validator, dataValueHash)
import           Ledger.Tx                        (Tx, TxOutRef, TxOutTx (..))
import qualified Ledger.Tx                        as Tx
import           Ledger.Typed.Scripts             (ScriptInstance, ScriptType (..))
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (ConnectionError)
import qualified Ledger.Typed.Tx                  as Typed
import qualified Ledger.Value                     as Value

data ScriptLookups a =
    ScriptLookups
        { slMPS            :: Map MonetaryPolicyHash MonetaryPolicy
        -- ^ Monetary policies that the script interacts with
        , slTxOutputs      :: Map TxOutRef TxOutTx
        -- ^ Unspent outputs that the script may want to spend
        , slOtherScripts   :: Map Address Validator
        -- ^ Validators of scripts other than "our script"
        , slOtherData      :: Map DataValueHash DataValue
        -- ^ Data value that we might need
        , slScriptInstance :: Maybe (ScriptInstance a)
        -- ^ The script instance with the typed validator hash & actual compiled program
        , slOwnPubkey      :: Maybe PubKeyHash
        -- ^ The contract's public key address, used for depositing tokens etc.
        }

instance Semigroup (ScriptLookups a) where
    l <> r =
        ScriptLookups
            { slMPS = slMPS l <> slMPS r
            , slTxOutputs = slTxOutputs l <> slTxOutputs r
            , slOtherScripts = slOtherScripts l <> slOtherScripts r
            , slOtherData = slOtherData l <> slOtherData r
            -- 'First' to match the semigroup instance of Map (left-biased)
            , slScriptInstance = fmap getFirst $ (First <$> slScriptInstance l) <> (First <$> slScriptInstance r)
            , slOwnPubkey = fmap getFirst $ (First <$> slOwnPubkey l) <> (First <$> slOwnPubkey r)

            }

instance Monoid (ScriptLookups a) where
    mappend = (<>)
    mempty  = ScriptLookups mempty mempty mempty mempty Nothing Nothing

scriptLookups :: ScriptInstance a -> ScriptLookups a
scriptLookups inst =
    ScriptLookups
        { slMPS = Map.singleton (Scripts.monetaryPolicyHash inst) (Scripts.monetaryPolicy inst)
        , slTxOutputs = Map.empty
        , slOtherScripts = Map.empty
        , slOtherData = Map.empty
        , slScriptInstance = Just inst
        , slOwnPubkey = Nothing
        }

-- | An unbalanced transaction. It needs to be balanced and signed before it
--   can be submitted to the ledeger.
data UnbalancedTx =
    UnbalancedTx
        { unBalancedTxTx                 :: Tx
        , unBalancedTxRequiredSignatories :: [PubKeyHash]
        }
    deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, IotsType)

makeLensesFor [("unBalancedTxTx", "tx"), ("unBalancedTxRequiredSignatories", "requiredSignatories")] ''UnbalancedTx

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx mempty []

instance Pretty UnbalancedTx where
    pretty UnbalancedTx{unBalancedTxTx, unBalancedTxRequiredSignatories} =
        vsep
        [ hang 2 $ vsep ["Tx:", pretty unBalancedTxTx]
        , hang 2 $ vsep $ "Requires signatures:" : (pretty <$> unBalancedTxRequiredSignatories)
        ]

data ConstraintProcessingState =
    ConstraintProcessingState
        { cpsUnbalancedTx       :: UnbalancedTx
        -- ^ The unbalanced transaction that we're building
        , cpsValueSpentActual   :: Value.Value
        -- ^ The total value spent by the UnbalancedTx, including
        --   any currency forged by it.
        , cpsValueSpentRequired :: Value.Value
        -- ^ The value that should be spent by the transaction
        }

initialState :: ConstraintProcessingState
initialState = ConstraintProcessingState{ cpsUnbalancedTx = emptyUnbalancedTx, cpsValueSpentActual = mempty, cpsValueSpentRequired = mempty }

makeLensesFor [("cpsUnbalancedTx", "unbalancedTx"), ("cpsValueSpentActual", "valueSpentActual"), ("cpsValueSpentRequired", "valueSpentRequired")] ''ConstraintProcessingState

-- | Turn a 'ConstraintsWithScripts a' value into a transaction that can be submitted
--   to the ledger, by adding all the right inputs and outputs
mkTx
    :: ( IsData (DataType a)
       , IsData (RedeemerType a))
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DataType a)
    -> Either MkTxError UnbalancedTx
mkTx lookups txc = fmap cpsUnbalancedTx $ runExcept $ runReaderT (execStateT go initialState) lookups where
    TxConstraints{txConstraints, txOwnInputs, txOwnOutputs} = txc
    go = do
        traverse_ processConstraint txConstraints
        traverse_ addOwnInput txOwnInputs
        traverse_ addOwnOutput txOwnOutputs
        addMissingValueSpent

addMissingValueSpent
    :: ( MonadReader (ScriptLookups a) m
       , MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
addMissingValueSpent = do
    ConstraintProcessingState{cpsValueSpentActual, cpsValueSpentRequired} <- get

    -- 'missing' is everything that's in
    -- 'cpsValueSpentRequired' but not in 'cpsValueSpentActual'
    let (_, missing) = Value.split (cpsValueSpentRequired N.- cpsValueSpentActual)

    if Value.isZero missing
        then pure ()
        else do
            -- add 'missing' to the transaction's outputs. This ensures that the
            -- wallet will add a corresponding input when balancing the
            -- transaction.
            pk <- asks slOwnPubkey >>= maybe (throwError OwnPubKeyMissing) pure
            unbalancedTx . tx . Tx.outputs %= (Tx.TxOut (PubKeyAddress pk) missing Tx.PayToPubKey :)

-- | Add a typed input, checking the type of the output it spends.
addOwnInput
    :: ( MonadReader (ScriptLookups a) m
        , MonadError MkTxError m
        , MonadState ConstraintProcessingState m
        , IsData (DataType a)
        , IsData (RedeemerType a)
        )
    => InputConstraint (RedeemerType a)
    -> m ()
addOwnInput InputConstraint{icRedeemer, icTxOutRef} = do
    ScriptLookups{slTxOutputs, slScriptInstance} <- ask
    inst <- maybe (throwError ScriptInstanceMissing) pure slScriptInstance
    typedOutRef <-
        either (throwError . TypeCheckFailed) pure
        $ runExcept @ConnectionError
        $ Typed.typeScriptTxOutRef (`Map.lookup` slTxOutputs) inst icTxOutRef
    let txIn = Typed.makeTypedScriptTxIn inst icRedeemer typedOutRef
    unbalancedTx . tx . Tx.inputs %= Set.insert (Typed.tyTxInTxIn txIn)

addOwnOutput
    :: ( MonadReader (ScriptLookups a) m
        , MonadState ConstraintProcessingState m
        , IsData (DataType a)
        , MonadError MkTxError m
        )
    => OutputConstraint (DataType a)
    -> m ()
addOwnOutput OutputConstraint{ocData, ocValue} = do
    ScriptLookups{slScriptInstance} <- ask
    inst <- maybe (throwError ScriptInstanceMissing) pure slScriptInstance
    let txOut = Typed.makeTypedScriptTxOut inst ocData ocValue
        dsV   = DataValue (toData ocData)
    unbalancedTx . tx . Tx.outputs %= (Typed.tyTxOutTxOut txOut :)
    unbalancedTx . tx . Tx.dataWitnesses . at (dataValueHash dsV) .= Just dsV

data MkTxError =
    TypeCheckFailed ConnectionError
    | TxOutRefNotFound TxOutRef
    | TxOutRefWrongType TxOutRef
    | DataValueNotFound DataValueHash
    | MonetaryPolicyNotFound MonetaryPolicyHash
    | ValidatorHashNotFound Address
    | OwnPubKeyMissing
    | ScriptInstanceMissing
    deriving (Eq, Show)

lookupTxOutRef
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m )
    => TxOutRef
    -> m TxOutTx
lookupTxOutRef outRef =
    let err = throwError (TxOutRefNotFound outRef) in
    asks slTxOutputs >>= maybe err pure . view (at outRef)

lookupDataValue
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m )
    => DataValueHash
    -> m DataValue
lookupDataValue dvh =
    let err = throwError (DataValueNotFound dvh) in
    asks slOtherData >>= maybe err pure . view (at dvh)

lookupMonetaryPolicy
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m )
    => MonetaryPolicyHash
    -> m MonetaryPolicy
lookupMonetaryPolicy mph =
    let err = throwError (MonetaryPolicyNotFound mph) in
    asks slMPS >>= maybe err pure . view (at mph)

lookupValidator
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m )
    => Address
    -> m Validator
lookupValidator addr =
    let err = throwError (ValidatorHashNotFound addr) in
    asks slOtherScripts >>= maybe err pure . view (at addr)

-- | Modify the 'UnbalancedTx' so that it satisfies the constraints, if
--   possible. Fails if a hash is missing from the lookups, or if an output
--   of the wrong type is spent.
processConstraint
  :: ( MonadReader (ScriptLookups a) m
     , MonadError MkTxError m
     , MonadState ConstraintProcessingState m )
  => TxConstraint
  -> m ()
processConstraint = \case
    MustIncludeDataValue dv ->
        let theHash = dataValueHash dv in
        unbalancedTx . tx . Tx.dataWitnesses %= set (at theHash) (Just dv)
    MustValidateIn slotRange ->
        unbalancedTx . tx . Tx.validRange %= (slotRange /\)
    MustBeSignedBy pk ->
        unbalancedTx . requiredSignatories %= (pk :)
    MustSpendValue vl ->
        valueSpentRequired <>= vl
    MustSpendPubKeyOutput txo -> do
        TxOutTx{txOutTxOut} <- lookupTxOutRef txo
        case Tx.txOutAddress txOutTxOut of
            PubKeyAddress pk -> do
                unbalancedTx . tx . Tx.inputs %= Set.insert (Tx.pubKeyTxIn txo)
                valueSpentActual <>= Tx.txOutValue txOutTxOut
                unbalancedTx . requiredSignatories %= (pk :)
            _                 -> throwError (TxOutRefWrongType txo)
    MustSpendScriptOutput txo red -> do
        txOutTx <- lookupTxOutRef txo
        case Tx.txOutType (txOutTxOut txOutTx) of
            Tx.PayToScript dvh -> do
                let addr = view Tx.outAddress (Tx.txOutTxOut txOutTx)
                validator <- lookupValidator addr

                -- first check the 'txOutTx' for the data value, then
                -- look for it in the 'slOtherData' map.
                dataValue <- maybe (lookupDataValue dvh) pure (Tx.txOutTxData txOutTx)
                -- TODO: When witnesses are properly segregated we can
                --       probably get rid of the 'slOtherData' map and of
                --       'lookupDataValue'
                let input = Tx.scriptTxIn txo validator red dataValue
                unbalancedTx . tx . Tx.inputs %= Set.insert input
                valueSpentActual <>= Tx.txOutValue (txOutTxOut txOutTx)
            _                 -> throwError (TxOutRefWrongType txo)

    MustForgeValue mpsHash tn i -> do
        monetaryPolicy <- lookupMonetaryPolicy mpsHash
        let value = Value.singleton (Value.mpsSymbol mpsHash) tn i
        unbalancedTx . tx . Tx.forgeScripts %= Set.insert monetaryPolicy
        unbalancedTx . tx . Tx.forge <>= value
        valueSpentActual <>= value
    MustPayToPubKey pk vl -> do
        unbalancedTx . tx . Tx.outputs %= (Tx.TxOut (PubKeyAddress pk) vl Tx.PayToPubKey :)
        -- we can subtract vl from 'valueSpentRequired' because
        -- we know for certain that it will be matched by an input
        -- after balancing
        valueSpentRequired <>= N.negate vl
    MustPayToOtherScript vlh dv vl -> do
        let addr = Address.scriptHashAddress vlh
            theHash = dataValueHash dv
        unbalancedTx . tx . Tx.dataWitnesses %= set (at theHash) (Just dv)
        unbalancedTx . tx . Tx.outputs %= (Tx.scriptTxOut' vl addr dv :)
        valueSpentRequired <>= N.negate vl
