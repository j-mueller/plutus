-- | This is a mock of (parts of) the Plutus API
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
              -- * API operations
              , TxM
              , Witness
              , Validator
              , PlutusTx
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
type Address = String

-- | Ada value
--
newtype Value = Value Integer
    deriving (Eq, Ord, Num, Show, Read)

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
            { txInOutRef  :: TxOutRef
            , txInWitness :: Witness
            }

-- | Construct an input that can spend the given output (assuming it was payed
--   to an address in our wallet.) Part of the wallet interface
--
txInSign :: TxOutRef -> KeyPair -> TxIn
txInSign = undefined

-- | Reference to an unspent output
--
newtype TxOutRef =
  TxOutRef
  { -- FIXME: missing is the actual reference
   txOutRefValue :: Value  -- we assume, this is added by the library
  }

data Witness

-- | UTxO output
--
data TxOut = TxOutPubKey Value Address
           | TxOutScript  Value PlutusTx PlutusTx     -- FIXME: it is a shame this is weakly typed

-- | PlutusTx code (for now just a plain Template Haskell expression AST)
--
type PlutusTx = ExpQ

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
    BlockHeight (Range Int) -- ^ True when the block height is within the range
    | FundsAtAddress Address (Range Value) -- ^ True when the funds at an address are within the range; TODO: [Address] to add up funds from multiple addresses
    | And EventTrigger EventTrigger -- ^ True when both triggers are true
    | Or EventTrigger EventTrigger -- ^ True when at least one trigger is true
    | PAlways -- ^ Always true
    | PNever -- ^ Never true

type Validator = PlutusTx
