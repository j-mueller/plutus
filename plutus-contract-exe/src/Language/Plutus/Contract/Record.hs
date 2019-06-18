{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
module Language.Plutus.Contract.Record where

import           Control.Lens
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Sequence                     (Seq)
import           GHC.Generics                      (Generic)

import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson

data FinalValue i o = FinalJSON Value o | FinalEvents (Seq i)
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data ClosedRecord i o =
    ClosedLeaf (FinalValue i o)
  | ClosedBin  (ClosedRecord i o) (ClosedRecord i o)
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data OpenRecord i o =
    OpenLeaf (Seq i)
  | OpenBind (OpenRecord i o)
  | OpenLeft (OpenRecord i o) (ClosedRecord i o)
  | OpenRight (ClosedRecord i o) (OpenRecord i o)
  | OpenBoth (OpenRecord i o) (OpenRecord i o)
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

openSubRecords :: Traversal' (OpenRecord i o) (OpenRecord i o)
openSubRecords f = \case
    v@OpenLeaf{} -> pure v
    OpenBind b -> OpenBind <$> f b
    OpenLeft l r -> OpenLeft <$> f l <*> pure r
    OpenRight l r -> OpenRight l <$> f r
    OpenBoth l r -> OpenBoth <$> f l <*> f r

closedSubRecords :: Traversal' (ClosedRecord i o) (ClosedRecord i o)
closedSubRecords f = \case
    v@ClosedLeaf{} -> pure v
    ClosedBin l r -> ClosedBin <$> f l <*> f r

type Record i o = Either (OpenRecord i o) (ClosedRecord i o)

newtype Rec i o a = Rec { unRec :: Either (OpenRecord i o) (ClosedRecord i o, a) }
    deriving (Functor, Foldable, Traversable)

getRecord :: Rec i o a -> Record i o
getRecord (Rec e) = fmap fst e

instance Applicative (Rec i o) where
    pure a = Rec (Right (ClosedLeaf (FinalEvents mempty), a))
    Rec l <*> Rec r = Rec $ case (l, r) of
        (Left l', Right (r', _))  -> Left (OpenLeft l' r')
        (Left l', Left r')   -> Left (OpenBoth l' r')
        (Right (l', _), Left r')  -> Left (OpenRight l' r')
        (Right (l', f), Right (r', a)) -> Right (ClosedBin l' r', f a)

instance Monad (Rec i o) where
    Rec l >>= _ = case l of
        Left l' -> Rec (Left (OpenBind l'))
        Right (l', _) -> Rec (Left (OpenRight l' (OpenLeaf mempty)))

fromPair :: Record i o -> Record i o -> Record i o
fromPair l r = case (l, r) of
  (Left l', Right r')  -> Left (OpenLeft l' r')
  (Left l', Left r')   -> Left (OpenBoth l' r')
  (Right l', Left r')  -> Left (OpenRight l' r')
  (Right l', Right r') -> Right (ClosedBin l' r')

jsonLeaf :: (Aeson.ToJSON a) => a -> o -> ClosedRecord i o
jsonLeaf a o = ClosedLeaf (FinalJSON (Aeson.toJSON a) o)

insert :: i -> Record i o -> Record i o
insert i = bimap go id  where
  go = \case
      OpenLeaf s -> OpenLeaf (s |> i)
      OpenLeft or' cr -> OpenLeft (go or') cr
      OpenRight cr or' -> OpenRight cr (go or')
      OpenBoth or' or'' -> OpenBoth (go or') (go or'')
      OpenBind or' -> OpenBind (go or')
