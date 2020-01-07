{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ledger.Typed.TypeUtils where

import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Proxy         (Proxy (..))

import           Data.Kind
import           Data.Type.Equality
import           GHC.TypeLits

-- | A heterogeneous list where every element is wrapped with the given functor.
data HListF (f :: Type -> Type) (l :: [Type]) where
    HNilF  :: HListF f '[]
    HConsF :: f e -> HListF f l -> HListF f (e ': l)

-- | Turn a 'HListF' into a homogeneous list. Requires a very polymorphic function, likely something like 'coerce'.
hfOut :: forall o f (ts :: [Type]) . (forall a . f a -> o) -> HListF f ts -> [o]
hfOut _ HNilF         = []
hfOut f (HConsF e es) = f e : hfOut f es

-- | The first element of a heterogeneous list.
hfHead :: forall f t (ts :: [Type]) . HListF f (t ': ts) -> f t
hfHead (HConsF e _) = e

data LengthList (n :: Nat) a where
  LLNil :: LengthList 0 a
  LLCons :: a -> LengthList n a -> LengthList (n+1) a

instance (Eq a) => Eq (LengthList n a) where
  l == r = llList l == llList r

instance ToJSON a => ToJSON (LengthList n a) where
  toJSON = toJSON . llList

instance (KnownNat n, FromJSON a) => FromJSON (LengthList n a) where
  parseJSON v = parseJSON v >>= maybe (fail "Wrong length") pure . fromList' @n

data SomeLengthList a =
  forall n. KnownNat n => SomeLengthList (LengthList n a)

slCons :: a -> SomeLengthList a -> SomeLengthList a
slCons a (SomeLengthList l) = case l of
    LLNil       -> SomeLengthList (LLCons a LLNil)
    LLCons x xs -> SomeLengthList (LLCons x xs)

slNil :: SomeLengthList a
slNil = SomeLengthList LLNil

llList :: LengthList n a -> [a]
llList = \case
  LLNil -> []
  LLCons x xs -> x : llList xs

fromList' :: forall n a. KnownNat n => [a] -> Maybe (LengthList n a)
fromList' xs = case fromList xs of
  SomeLengthList (p2 :: LengthList n2 a) ->
    case sameNat (Proxy @n) (Proxy @n2) of
      Nothing   -> Nothing
      Just Refl -> Just p2

fromList :: [a] -> SomeLengthList a
fromList = foldl (flip slCons) slNil

instance Eq a => Eq (SomeLengthList a) where
  (SomeLengthList xs) == (SomeLengthList ys) =
    llList xs == llList ys

instance ToJSON a => ToJSON (SomeLengthList a) where
  toJSON = \case { SomeLengthList ll -> toJSON (llList ll) }

instance FromJSON a => FromJSON (SomeLengthList a) where
  parseJSON = fmap fromList . parseJSON
