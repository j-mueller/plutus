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

import           Control.Applicative               (Alternative (..))
import           Control.Lens
import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Types                  as Aeson
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Foldable                     (toList)
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Profunctor                   (Profunctor (..))
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as Seq
import           GHC.Generics                      (Generic)

import           Language.Plutus.Contract.Class
import qualified Language.Plutus.Contract.Contract as C

data StatefulContract i o a where
    CEmit :: o -> StatefulContract i o a -> StatefulContract i o a
    CWaiting :: (i -> StatefulContract i o a) -> StatefulContract i o a

    CMap :: (a' -> a) -> StatefulContract i o a' -> StatefulContract i o a
    CAp :: StatefulContract i o (a' -> a) -> StatefulContract i o a' -> StatefulContract i o a
    CBind :: StatefulContract i o a' -> (a' -> StatefulContract i o a) -> StatefulContract i o a
    CSelect :: StatefulContract i o a -> StatefulContract i o a -> StatefulContract i o a

    CFinished :: a -> StatefulContract i o a
    CJSONCheckpoint :: (Aeson.FromJSON a, Aeson.ToJSON a, Aeson.FromJSON o, Aeson.ToJSON o) => StatefulContract i o a -> StatefulContract i o a


instance Functor (StatefulContract i o) where
    fmap f = \case
        CEmit o e -> CEmit o (fmap f e)
        CWaiting f' -> CWaiting $ \i -> fmap f (f' i)

        CMap f' c -> CMap (f . f') c
        CAp l r     -> CAp (fmap (fmap f) l) r
        CBind m f'  -> CBind m (fmap f . f')
        CSelect l r  -> CSelect (fmap f l) (fmap f r)

        CFinished a -> CFinished (f a)
        CJSONCheckpoint c -> CMap f (CJSONCheckpoint c)

instance Applicative (StatefulContract i o) where
    pure = CFinished
    (<*>) = CAp

instance Monad (StatefulContract i o) where
    (>>=) = CBind

instance MonadContract i o (StatefulContract i o) where
    emit o = CEmit o (CFinished ())
    waiting = CWaiting pure
    select = CSelect

data BinTree a = Node (BinTree a) a (BinTree a) | Leaf a
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

append_ :: Semigroup a => a -> BinTree a -> BinTree a
append_ a (Node l m r) = Node l (m <> a) r
append_ a (Leaf a')    = Leaf (a' <> a)

appendMaybe :: Semigroup a => Maybe a -> BinTree a -> BinTree a
appendMaybe Nothing  = id
appendMaybe (Just a) = append_ a

data FinalValue i o = FinalJSON Value o | FinalEvents (Seq i)
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data ClosedRecord i o =
      ClosedLeaf (FinalValue i o)
    | ClosedBin  (ClosedRecord i o) (ClosedRecord i o)
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data OpenRecord i o =
      OpenLeaf i (OpenRecord i o)
    | OpenLeft (OpenRecord i o) (ClosedRecord i o)
    | OpenRight (ClosedRecord i o) (OpenRecord i o)
    | OpenBoth (OpenRecord i o) (OpenRecord i o)
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

type Record i o = Either (OpenRecord i o) (ClosedRecord i o)

jsonLeaf :: (Aeson.ToJSON a) => a -> o -> ClosedRecord i o
jsonLeaf a o = ClosedLeaf (FinalJSON (Aeson.toJSON a) o)

insert :: i -> Record i o -> Record i o
insert i = undefined

offerRec
    :: Monoid o
    => StatefulContract i o a
    -> Record i o
    -> i
    -> Either String ((o, Record i o), Maybe a)
offerRec con r i = updateRecord con (insert i r)

offer :: i -> StatefulContract i o a -> StatefulContract i o a
offer i = \case
    CEmit o c' -> CEmit o (offer i c')
    CWaiting f -> f i

    CMap f c' -> CMap f (offer i c')
    CAp l r -> CAp (offer i l) (offer i r)
    CBind l f -> CBind (offer i l) f -- ?
    CSelect l r -> CSelect (offer i l) (offer i r)

    CFinished a -> CFinished a
    CJSONCheckpoint c -> CJSONCheckpoint (offer i c)

applyInputs is c = foldr offer c is

drain :: Monoid o => StatefulContract i o a -> (o, Maybe a)
drain = \case
    CEmit o c' -> let (o', r) = drain c' in (o <> o', r)
    CWaiting _ -> (mempty, Nothing)

    CMap f c' -> let (o, r) = drain c' in (o, fmap f r)
    CAp l r -> 
        let (ol, rl) = drain l
            (or, rr) = drain r
        in (ol <> or, rl <*> rr)
    CBind l f ->
        let (o, r) = drain l
        in case r of
            Nothing -> (o, Nothing)
            Just a  -> let (o', r') = drain (f a) in (o <> o', r')
    CSelect l r ->
        let (ol, rl) = drain l
            (or, rr) = drain r
        in case (rl, rr) of
            (Just a, _) -> (ol <> or, Just a) -- TODO: all of rr's hooks (ie, 'or') should be disabled?
            (_, Just a) -> (ol <> or, Just a)
            _ -> (ol <> or, Nothing)
    CFinished a -> (mempty, Just a)
    CJSONCheckpoint c' -> drain c'

runClosed 
    :: Monoid o
    => StatefulContract i o a
    -> ClosedRecord i o
    -> Either String (o, Maybe a)
runClosed con = \case
    ClosedLeaf (FinalEvents is) ->
        let con' = applyInputs (toList is) con
            (o, r) = drain con'
        in pure (o, r)
    ClosedLeaf (FinalJSON vl o) -> 
        case con of
            CJSONCheckpoint _ -> do
                vl' <- Aeson.parseEither Aeson.parseJSON vl
                pure (o, Just vl')
            _ -> Left "Expected JSON checkpoint"
    ClosedBin l r ->
        case con of
            CAp l' r' -> do
                (lo, a) <- runClosed l' l
                (ro, b) <- runClosed r' l
                pure (lo <> ro, a <*> b)
            CBind l' f -> do
                (lo, a') <- runClosed l' l
                a <- maybe (Left "CBind") Right a'
                (ro, b) <- runClosed (f a) r
                pure (lo <> ro, b)
            CSelect l' r' -> do
                (lo, a) <- runClosed l' l
                (ro, b) <- runClosed r' r
                case (a, b) of
                    (Just a', _) -> pure (lo <> ro, Just a')
                    (_, Just b') -> pure (lo <> ro, Just b')
                    _ -> pure (lo <> ro, Nothing) -- error?`
            _ -> Left "Right ClosedBin with wrong contract type"        

runOpen
    :: Monoid o
    => StatefulContract i o a
    -> OpenRecord i o
    -> Either String (o, Either (OpenRecord i o) (ClosedRecord i o, a))
runOpen con or =
    case (con, or) of
        (CEmit o con', _) -> do
            (o', r) <- runOpen con' or
            pure (o <> o', r)
        (CWaiting f, OpenLeaf i or') -> 
            runOpen (f i) or'
            

updateRecord
    :: Monoid o
    => StatefulContract i o a
    -> Record i o
    -> Either String ((o, Record i o), Maybe a)
updateRecord con rc = 
    case rc of
        Right cl -> do
            (o, r) <- runClosed con cl
            pure ((o, Right cl), r)
        Left cl -> do
            (o, result) <- runOpen con cl
            case result of
                Left r' -> pure ((o, Left r'), Nothing)
                Right (r', a) -> pure ((o, Right r'), Just a)
