{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Plutus.Contract.Balanced where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

class Semigroup m => InverseSemigroup m where
  inv :: m -> m

-- 'Balanced' keeps track of how many things (parens, endpoint calls, etc.) 
-- are required on either side.
--
-- From https://www.youtube.com/watch?v=HGi5AxmQUwU
data Balanced = Balanced !Int !Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Balanced where
    Balanced a b <> Balanced c d
        | b <= c = Balanced (a + c - b) d
        | otherwise = Balanced a (d + b - c)

instance InverseSemigroup Balanced where
    inv (Balanced a b) = Balanced b a

open :: Balanced
open = Balanced 0 1

close :: Balanced
close = Balanced 1 0

instance Monoid Balanced where
    mempty = Balanced 0 0

isBalanced :: Balanced -> Bool
isBalanced = (==) mempty

isOpen :: Balanced -> Bool
isOpen (Balanced _ o) = o > 0
