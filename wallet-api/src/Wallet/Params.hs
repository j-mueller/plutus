{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Wallet.Params where

import           Data.Aeson            (FromJSON (..), ToJSON (..))
import           Data.Functor.Identity
import qualified Data.Text             as Text
import           GHC.Generics          (Generic)
import           Wallet.API            (WalletAPI (..))
import qualified Wallet.UTXO           as UTXO

-- | Types that are used in validator scripts. Because this compiles to
--   PLC, the primitives are mostly integers and hashes.
--   We distinguish a number of different types here so that the appropriate 
--   form can be displayed in HTML. For example, a `Wallet` value can be
--   selected from the list of all known wallets in the current playground.
data PrimType =
      Value
    | BlockHeight
    | Wallet
    | PubKey
    | TxRef -- Reference to a transaction output (select one in the UI)
    | ScriptHash
    deriving (Show, Generic)

instance ToJSON PrimType
instance FromJSON PrimType

-- | These are types for which we can make a nice form in Purescript. 
data ContractParamType =
    NamedParam Text.Text ContractParamType -- ^ Named type
    | Prim PrimType -- ^ Primitive
    | Sum ContractParamType ContractParamType -- ^ (Either)
    | Product ContractParamType ContractParamType -- ^ (,)
    | ParamList ContractParamType -- ^ []
    deriving (Show, Generic)

instance ToJSON ContractParamType
instance FromJSON ContractParamType

class ContractParamTypeable a where
    tp :: ContractParamType

instance ContractParamTypeable UTXO.Value where
    tp = Prim Value

instance ContractParamTypeable UTXO.TxOutRef' where
    tp = Prim TxRef

-- TODO: other instances for types in UTXO module

-- | ContractAPI is the class of functions whose parameters
--   `ContractParamTypeable`.
class ContractAPI t where
    params :: [ContractParamType]

-- NB: There are probably better ways to do this.
--     The goal is to be able to have something like
--     `instance ContractAPI (WalletAPI m => Vesting -> UTXO.Value -> m ())`
--     (which is the signature of a real contract interaction)
--     But that doesn't work because of the way `m` is quantified. So here I am
--     just using `Identity ()`.
instance ContractAPI (Identity ()) where
    params = []

instance (ContractParamTypeable a, ContractAPI t) => ContractAPI (a -> t) where
    params = tp @a : params @t

-- | Tranche of a vesting scheme.
data VestingTranche = VestingTranche {
    vestingTrancheDate   :: UTXO.Height,
    vestingTrancheAmount :: UTXO.Value
    } deriving (Show, Generic)

instance ContractParamTypeable VestingTranche where
    tp = Product
            (NamedParam "vestingTrancheDate" $ Prim BlockHeight)
            (NamedParam "vestingTrancheAmount" $ Prim Value)

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (block height) after which an additional amount of money can be spent.
data Vesting = Vesting {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner    :: UTXO.PubKey
    } deriving Generic

instance ContractParamTypeable Vesting where
    tp = Product
            (Product
                (NamedParam "vestingTranche1" $ tp @VestingTranche)
                (NamedParam "vestingTranche2" $ tp @VestingTranche))
            (NamedParam "vestingOwner" $ Prim PubKey)

-- | Data script for vesting utxo
data VestingData = VestingData {
    vestingDataHash    :: Int, -- ^ Hash of the validator script (TODO: Digest SHA256)
    vestingDataPaidOut :: UTXO.Value -- ^ How much of the vested value has already been retrieved
    } deriving (Generic, Eq)

instance ContractParamTypeable VestingData where
    tp = Product
            (NamedParam "vestingDataHash" $ Prim ScriptHash)
            (NamedParam "vestingDataPaidOut" $ Prim Value)
-- EXAMPLE


vestFunds :: WalletAPI m => Vesting -> UTXO.Value -> m ()
vestFunds = undefined -- See Language.Plutus.Coordination.Contracts.Vesting

retrieveFunds :: WalletAPI m => Vesting -> VestingData -> UTXO.TxOutRef' -> m ()
retrieveFunds = undefined

-- >>> vestFundsParams
-- >>> [Product (Product (NamedParam "vestingTranche1" (Product (NamedParam "vestingTrancheDate" (Prim BlockHeight)) (NamedParam "vestingTrancheAmount" (Prim Value)))) (NamedParam "vestingTranche2" (Product (NamedParam "vestingTrancheDate" (Prim BlockHeight)) (NamedParam "vestingTrancheAmount" (Prim Value))))) (NamedParam "vestingOwner" (Prim PubKey)),Prim Value]
vestFundsParams :: [ContractParamType]
vestFundsParams = params @(Vesting -> UTXO.Value -> Identity ())

retrieveFundsParams :: [ContractParamType]
retrieveFundsParams = params @(Vesting -> VestingData -> UTXO.TxOutRef' -> Identity ())
