-- | This is a mock of (parts of) the Plutus API
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Plutus (-- * Transactions and related types
                Address
              , PubKey
              , KeyPair
              , pubKey
              , Value
              , Tx(..)
              , TxIn(..)
              , TxOut(..)
              , TxOutRef(..)
              , standardTxFee
              , pubKeyAddress
              , txOutValue
              , txOutRedeemer
              -- * API operations
              , TxM
              , Hash
              , Redeemer
              , Validator(..)
              , PlutusTx(..)
              , BlockHeight(..)
              , mkValidator
              , PendingTx(..)
              , submitTransaction
              , assert
              , lookupMyKeyPair
              , lookupMyPubKey
              , createPayment
              , txInSign
              , Range(..)
              , EventTrigger(..)
              ) where

import           Control.Monad.State        (State)
import           Language.Haskell.TH        hiding (Range)
import           Language.Haskell.TH.Syntax hiding (Range)

-- | Cardano address
--
newtype Address = Address String
    deriving (Eq, Ord, Show, Read)

-- | Ada value
--
newtype Value = Value Integer
    deriving (Eq, Ord, Num, Show, Read)

newtype Hash = Hash String -- bytestring type in plutus core
    deriving (Eq, Ord, Show, Read)

-- | Public key
--
data PubKey

instance Lift PubKey where
  lift = undefined

-- | Public key pair (no lift instance, because we never ought to put it into a
--   transaction)
--
data KeyPair

pubKeyAddress :: KeyPair -> Address
pubKeyAddress = undefined

data TxState

-- | Transaction monad for coordination layer computations; provides access to
-- the blockchain
--
type TxM a = State TxState a

-- | Submit the given transaction to the blockchain
submitTransaction :: Tx -> TxM [TxOutRef]
submitTransaction = undefined

-- | Verify that a condition is true.
assert :: Bool -> TxM ()
assert = undefined

-- | Get the users's public key. Part of the wallet interface
lookupMyPubKey :: TxM PubKey
lookupMyPubKey = pubKey <$> lookupMyKeyPair

-- | Extract the public key from a key pair.
pubKey :: KeyPair -> PubKey
pubKey = undefined

-- | Part of the wallet interface
--   TODO: Should the Plutus client even be able to know the private key?
lookupMyKeyPair :: TxM KeyPair
lookupMyKeyPair = undefined

-- | Create an input that spends the given value (part of the wallet interface)
--
createPayment :: Value -> TxM TxIn
createPayment = undefined

-- | A UTxO transaction specification
--
data Tx = Tx
          { txInputs  :: [TxIn]
          , txOutputs :: [TxOut]
          }

-- | UTxO input
--
data TxIn = TxIn
            { txInOutRef   :: !TxOutRef
            , txInRedeemer :: !Hash
            }

-- | Construct an input that can spend the given output (assuming it was payed
--   to an address in our wallet.) Part of the wallet interface
--
txInSign :: TxOutRef -> KeyPair -> TxIn
txInSign = undefined

-- | Reference to an unspent output
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#extension-to-transaction-outputs
--
data TxOutRef =
  TxOutRef
  {
     txOutRefValue          :: !Value -- We assume this is added by the library. TODO: In cardano-sl this is a "ValueDistribution" (map of keys to values)
   , txOutRefValidatorHash  :: !Hash -- Hash of validator script. The validator script has to be submitted by the consumer of the outputs referenced by this TxOutRef.
   , txOutRefDataScriptHash :: !Hash -- Hash of data script used by the creator of the transaction.
  }

type  Redeemer = String

type DataScript = PlutusTx ()

newtype BlockHeight = BlockHeight Integer
    deriving (Eq, Ord)

-- | Information about a pending transaction used by validator scripts.
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#blockchain-state-available-to-validator-scripts
data PendingTx = PendingTx {
      pendingTxBlockHeight :: !BlockHeight -- ^ Block height exl. current transaction
    , pendingTxHash        :: !Hash -- ^ Hash of the transaction that is being validated
    , pendingTxTransaction :: !Tx
    }

-- | UTxO output
--
data TxOut = TxOutPubKey !Value !Address
           | TxOutScript  !Value !(PlutusTx (Validator ())) !(PlutusTx ())     -- FIXME: it is a shame this is weakly typed

txOutValue :: TxOut -> Value
txOutValue = \case
    TxOutPubKey v _ -> v
    TxOutScript v _ _ -> v


txOutRedeemer :: TxOut -> Maybe (PlutusTx ())
txOutRedeemer = \case
    TxOutScript _ _ r -> Just r
    _ -> Nothing

-- | PlutusTx code (for now just a plain Template Haskell expression AST)
--
newtype PlutusTx a = PlutusTx ExpQ

-- | Some sort of transaction fee (we need to determine that more dynamically)
--
standardTxFee :: Value
standardTxFee = undefined

data Range a =
    Interval a a -- inclusive-exclusive
    | GEQ a
    | LT a

-- | Event triggers the Plutus client can register with the wallet.
data EventTrigger =
    BlockHeightRange !(Range Int) -- ^ True when the block height is within the range
    | FundsAtAddress !Address !(Range Value) -- ^ True when the funds at an address are within the range; TODO: [Address] to add up funds from multiple addresses
    | And EventTrigger EventTrigger -- ^ True when both triggers are true
    | Or EventTrigger EventTrigger -- ^ True when at least one trigger is true
    | PAlways -- ^ Always true
    | PNever -- ^ Never true

-- | Validator scripts expect two scripts and information about the current
--   txn. In the future this will be written in Plutus (with the help of TH)
--   and its return type will be `a` instead of `Maybe a`.
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#extension-to-validator-scripts
--
--  TODO: What happens to the output if it is `Just a` ?
newtype Validator a = Validator { runValidator :: Redeemer -> DataScript -> PendingTx -> Maybe a }

instance Lift a => Lift (Validator a) where
    lift = undefined

mkValidator :: Lift a => Validator a -> PlutusTx (Validator a)
mkValidator f = PlutusTx [| f |]

{- Note [Transaction Templates]

Transaction templates are currently missing from this mock API and will be
added in the future.

Transaction templates differ from transactions in at least two ways:

1) They do not include a transaction fee (that is, the sum of their input
   values equals the sum of their output values)
2) Part of their input value is not attributed to an address

To turn a template into a transaction, the wallet
1) Adjusts either the input values or the output value to ensure that the
   difference between inputs and outputs matches the transaction fee.
2) Expands the inputs to account for the missing funds (via coin selection).

These two steps depend on each other because the transaction fee is a
function of the size of the transaction including its
inputs.

-}
