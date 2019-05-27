{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Prevent unboxing, which the plugin can't deal with
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Functions for working with 'Value'.
module Ledger.Value(
    -- ** Currency symbols
      CurrencySymbol(..)
    , currencySymbol
    -- ** Token names
    , TokenName(..)
    , tokenName
    , toString
    -- ** Value
    , Value(..)
    , singleton
    , valueOf
    , scale
    , symbols
      -- * Constants
    , zero
      -- * Num operations
    , plus
    , minus
    , multiply
    , negate
    , geq
    , gt
    , leq
    , lt
      -- * Etc.
    , isZero
    , split
    ) where

import qualified Prelude                      as Haskell

import           Codec.Serialise.Class        (Serialise)
import           Control.Lens                 (set,(.~))
import           Data.Aeson                   (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:))
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Extras            as JSON
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as C8
import           Data.Hashable                (Hashable)
import qualified Data.Swagger.Internal        as S
import           Data.Swagger.Schema          (ToSchema(declareNamedSchema))
import qualified Data.Swagger.Lens            as S
import           Data.Swagger                 (SwaggerType(SwaggerObject), NamedSchema(NamedSchema), declareSchemaRef)
import           Data.Proxy                   (Proxy(Proxy))
import           Data.String                  (IsString(fromString))
import qualified Data.Text                    as Text
import           GHC.Generics                 (Generic)
import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Lift       (makeLift)
import           Language.PlutusTx.Prelude    hiding (plus, minus, negate, multiply)
import qualified Language.PlutusTx.Prelude    as P
import qualified Language.PlutusTx.AssocMap   as Map
import           Language.PlutusTx.These
import           LedgerBytes                  (LedgerBytes(LedgerBytes))
import           Data.Function                ((&))

hexSchema :: S.Schema
hexSchema = Haskell.mempty & set S.type_ S.SwaggerString & set S.format (Just "hex")

stringSchema :: S.Schema
stringSchema = Haskell.mempty & set S.type_ S.SwaggerString

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
    deriving (IsString, Show, ToJSONKey, FromJSONKey, Serialise) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord)
    deriving anyclass (Hashable)

instance ToSchema CurrencySymbol where
  declareNamedSchema _ = pure $ S.NamedSchema (Just "CurrencySymbol") hexSchema

instance ToJSON CurrencySymbol where
  toJSON currencySymbol =
    JSON.object
      [ ( "unCurrencySymbol"
        , JSON.String .
          JSON.encodeByteString .
          BSL.toStrict . unCurrencySymbol $
          currencySymbol)
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      bytes <- JSON.decodeByteString raw
      pure . CurrencySymbol . BSL.fromStrict $ bytes

makeLift ''CurrencySymbol

{-# INLINABLE currencySymbol #-}
currencySymbol :: ByteString -> CurrencySymbol
currencySymbol = CurrencySymbol

newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
    deriving (Serialise) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord)
    deriving anyclass (Hashable)

instance IsString TokenName where
  fromString = TokenName . C8.pack

toString :: TokenName -> String
toString = C8.unpack . unTokenName

instance Show TokenName where
  show = toString

instance ToSchema TokenName where
    declareNamedSchema _ = pure $ S.NamedSchema (Just "TokenName") stringSchema

instance ToJSON TokenName where
    toJSON tokenName =
        JSON.object
        [ ( "unTokenName", JSON.toJSON $ toString tokenName)]

instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object .: "unTokenName"
        pure . fromString . Text.unpack $ raw

makeLift ''TokenName

{-# INLINABLE tokenName #-}
tokenName :: ByteString -> TokenName
tokenName = TokenName

-- | A cryptocurrency value. This is a map from 'CurrencySymbol's to a
-- quantity of that currency.
--
-- Operations on currencies are usually implemented /pointwise/. That is,
-- we apply the operation to the quantities for each currency in turn. So
-- when we add two 'Value's the resulting 'Value' has, for each currency,
-- the sum of the quantities of /that particular/ currency in the argument
-- 'Value'. The effect of this is that the currencies in the 'Value' are "independent",
-- and are operated on separately.
--
-- Whenever we need to get the quantity of a currency in a 'Value' where there
-- is no explicit quantity of that currency in the 'Value', then the quantity is
-- taken to be zero.
--
-- See note [Currencies] for more details.
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable)
    deriving newtype (Serialise)

-- Orphan instances for 'Map' to make this work
instance (ToJSON v, ToJSON k) => ToJSON (Map.Map k v) where
    toJSON = JSON.toJSON . Map.toList

instance (FromJSON v, FromJSON k) => FromJSON (Map.Map k v) where
    parseJSON v = Map.fromList Haskell.<$> JSON.parseJSON v

deriving anyclass instance (Hashable k, Hashable v) => Hashable (Map.Map k v)
deriving anyclass instance (Serialise k, Serialise v) => Serialise (Map.Map k v)
deriving anyclass instance (ToSchema k, ToSchema v) => ToSchema (Map.Map k v)

makeLift ''Value

instance Haskell.Eq Value where
    (==) = eq

instance Eq Value where
    {-# INLINABLE (==) #-}
    (==) = eq

-- No 'Ord Value' instance since 'Value' is only a partial order, so 'compare' can't
-- do the right thing in some cases.

instance Haskell.Semigroup Value where
    (<>) = plus

instance Semigroup Value where
    (<>) = plus

instance Haskell.Monoid Value where
    mempty = zero

instance Monoid Value where
    mempty = zero

-- 'InnerMap' exists only to trick swagger's generic deriving mechanism
-- into not looping indefinitely when it encounters a nested map.
newtype InnerMap = InnerMap { unMap :: [(TokenName, Integer)] }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance ToSchema Value where
    declareNamedSchema _ = do
        mapSchema <- declareSchemaRef (Proxy @(Map.Map CurrencySymbol InnerMap))
        return $
                NamedSchema (Just "Value") $ Haskell.mempty
                    & S.type_ .~ SwaggerObject
                    & S.properties .~ [ ("getValue", mapSchema) ]
                    & S.required .~ [ "getValue" ]

{- note [Currencies]

The 'Value' type represents a collection of amounts of different currencies.

We can think of 'Value' as a vector space whose dimensions are
currencies. At the moment there is only a single currency (Ada), so 'Value'
contains one-dimensional vectors. When currency-creating transactions are
implemented, this will change and the definition of 'Value' will change to a
'Map Currency Int', effectively a vector with infinitely many dimensions whose
non-zero values are recorded in the map.

To create a value of 'Value', we need to specifiy a currency. This can be done
using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we use
'Ledger.Ada.fromValue'. Plutus contract authors will be able to define modules
similar to 'Ledger.Ada' for their own currencies.

-}

{-# INLINABLE valueOf #-}
-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf (Value mp) cur tn =
    case Map.lookup cur mp of
        Nothing -> 0 :: Integer
        Just i  -> case Map.lookup tn i of
            Nothing -> 0
            Just v  -> v

{-# INLINABLE symbols #-}
-- | The list of 'CurrencySymbol's of a 'Value'.
symbols :: Value -> [CurrencySymbol]
symbols (Value mp) = Map.keys mp

{-# INLINABLE singleton #-}
-- | Make a 'Value' containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleton c tn i = Value (Map.singleton c (Map.singleton tn i))

{-# INLINABLE unionVal #-}
-- | Combine two 'Value' maps
unionVal :: Value -> Value -> Map.Map CurrencySymbol (Map.Map TokenName (These Integer Integer))
unionVal (Value l) (Value r) =
    let
        combined = Map.union l r
        unThese k = case k of
            This a    -> This <$> a
            That b    -> That <$> b
            These a b -> Map.union a b
    in unThese <$> combined

{-# INLINABLE unionWith #-}
unionWith :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
unionWith f ls rs =
    let
        combined = unionVal ls rs
        unThese k' = case k' of
            This a -> f a 0
            That b -> f 0 b
            These a b -> f a b
    in Value (fmap (fmap unThese) combined)

{-# INLINABLE scale #-}
-- | Multiply all the quantities in the 'Value' by the given scale factor.
scale :: Integer -> Value -> Value
scale i (Value xs) = Value (fmap (fmap (\i' -> P.multiply i i')) xs)

-- Num operations

{-# INLINABLE plus #-}
-- | Add two 'Value's together. See 'Value' for an explanation of how operations on 'Value's work.
plus :: Value -> Value -> Value
plus = unionWith P.plus

{-# INLINABLE negate #-}
-- | Negate a 'Value's. See 'Value' for an explanation of how operations on 'Value's work.
negate :: Value -> Value
negate = scale (-1)

{-# INLINABLE minus #-}
-- | Subtract one 'Value' from another. See 'Value' for an explanation of how operations on 'Value's work.
minus :: Value -> Value -> Value
minus = unionWith P.minus

{-# INLINABLE multiply #-}
-- | Multiply two 'Value's together. See 'Value' for an explanation of how operations on 'Value's work.
multiply :: Value -> Value -> Value
multiply = unionWith P.multiply

{-# INLINABLE zero #-}
-- | The empty 'Value'.
zero :: Value
zero = Value (Map.empty ())

{-# INLINABLE isZero #-}
-- | Check whether a 'Value' is zero.
isZero :: Value -> Bool
isZero (Value xs) = Map.all (Map.all (\i -> 0 == i)) xs

{-# INLINABLE checkPred #-}
checkPred :: (These Integer Integer -> Bool) -> Value -> Value -> Bool
checkPred f l r =
    let
      inner :: Map.Map TokenName (These Integer Integer) -> Bool
      inner = Map.all f
    in
      Map.all inner (unionVal l r)

{-# INLINABLE checkBinRel #-}
-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
--   supplying 0 where a key is only present in one of them.
checkBinRel :: (Integer -> Integer -> Bool) -> Value -> Value -> Bool
checkBinRel f l r =
    let
        unThese k' = case k' of
            This a    -> f a 0
            That b    -> f 0 b
            These a b -> f a b
    in checkPred unThese l r

{-# INLINABLE geq #-}
-- | Check whether one 'Value' is greater than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
geq :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

{-# INLINABLE gt #-}
-- | Check whether one 'Value' is strictly greater than another. See 'Value' for an explanation of how operations on 'Value's work.
gt :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

{-# INLINABLE leq #-}
-- | Check whether one 'Value' is less than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
leq :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

{-# INLINABLE lt #-}
-- | Check whether one 'Value' is strictly less than another. See 'Value' for an explanation of how operations on 'Value's work.
lt :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

{-# INLINABLE eq #-}
-- | Check whether one 'Value' is equal to another. See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

{-# INLINABLE split #-}
-- | Split a value into its positive and negative parts. The first element of 
--   the tuple contains the negative parts of the value, the second element
--   contains the positive parts.
--
--   @negate (fst (split a)) `plus` (snd (split a)) == a@
--
split :: Value -> (Value, Value)
split (Value mp) = (Value pos, Value neg) where
  (pos, neg) = Map.mapThese splitIntl mp

    splitIntl :: Map.
    splitIntl 
