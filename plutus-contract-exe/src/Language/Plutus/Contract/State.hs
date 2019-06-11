{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A version of 'Language.Plutus.Contract.Contract' that
--   writes checkpoints
module Language.Plutus.Contract.State where

import           Control.Monad.State
import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Foldable                     (toList)
import           Data.Profunctor                   (Profunctor (..))
import           Data.Sequence                     (Seq, (|>))
import qualified Data.Sequence                     as Seq
import           GHC.Generics                      (Generic)

import           Language.Plutus.Contract.Class
import qualified Language.Plutus.Contract.Contract as C

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data BinPath a = L !(BinPath a) | R !(BinPath a) | H !a
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data StatefulContract s i o a where
    CContract :: C.Contract i o a -> StatefulContract s i o a
    CFinished :: a -> StatefulContract s i o a
    CAp :: StatefulContract s i o (a' -> a) -> StatefulContract s i o a' -> StatefulContract s i o a
    CBind :: StatefulContract s i o a' -> (a' -> StatefulContract s i o a) -> StatefulContract s i o a

instance Functor (StatefulContract s i o) where
    fmap f = \case
        CFinished a -> CFinished (f a)
        CAp l r     -> CAp (fmap (fmap f) l) r
        CBind m f'  -> CBind m (fmap f . f')

instance Applicative (StatefulContract s i o) where
    (<*>) = CAp

instance Monad (StatefulContract s i o) where
    (>>=) = CBind

toCon :: StatefulContract s i o a -> C.Contract i o a
toCon = \case
    CContract c -> c
    CFinished a -> pure a
    CAp l r -> toCon l <*> toCon r
    CBind l r -> toCon l >>= (\i' -> toCon (r i'))

instance MonadContract i o (StatefulContract s i o) where
    emit = CContract . emit
    waiting = CContract waiting
    select l r = CContract (toCon l `select` toCon r)
    offer i c = CContract (offer i (toCon c))
