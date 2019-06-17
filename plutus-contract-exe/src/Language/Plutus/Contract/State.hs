{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A version of 'Language.Plutus.Contract.Contract' that
--   writes checkpoints
module Language.Plutus.Contract.State where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Types                  as Aeson
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Foldable                     (toList)
import           Data.Sequence                     (Seq)
import           GHC.Generics                      (Generic)

import           Language.Plutus.Contract.Class
import qualified Language.Plutus.Contract.Contract as C

data StatefulContract i o a where
    CMap :: (a' -> a) -> StatefulContract i o a' -> StatefulContract i o a
    CAp :: StatefulContract i o (a' -> a) -> StatefulContract i o a' -> StatefulContract i o a
    CBind :: StatefulContract i o a' -> (a' -> StatefulContract i o a) -> StatefulContract i o a

    CContract :: C.Contract i o a -> StatefulContract i o a
    CJSONCheckpoint :: (Aeson.FromJSON a, Aeson.ToJSON a, Aeson.FromJSON o, Aeson.ToJSON o) => StatefulContract i o a -> StatefulContract i o a

initialise :: Monoid o => StatefulContract i o a -> Record i o
initialise = \case
        CMap _ c' -> initialise c'
        CAp l r -> fromPair (initialise l) (initialise r)
        CBind l f ->
            case initialise l of
                Left r -> Left (OpenBind r)
                Right _ ->
                    case snd (result l) of
                        Just a -> fromPair (initialise l) (initialise $ f a)
        CContract c ->
            case snd (result (CContract c)) of
                Nothing -> Left (OpenLeaf mempty)
                Just _  -> Right (ClosedLeaf (FinalEvents mempty))
        CJSONCheckpoint c' ->
            case result c' of
                (_, Nothing) -> initialise c'
                (o, Just a)  -> Right $ jsonLeaf a o

checkpoint :: (Aeson.FromJSON a, Aeson.ToJSON a, Aeson.FromJSON o, Aeson.ToJSON o) => StatefulContract i o a -> StatefulContract i o a
checkpoint = CJSONCheckpoint

prtty :: StatefulContract i o a -> String
prtty = \case
    CMap _ c -> "cmap (" ++ prtty c ++ ")"
    CAp l r -> "cap (" ++ prtty l ++ ") (" ++ prtty r ++ ")"
    CBind l _ -> "cbind (" ++ prtty l ++  ") f"
    CContract _ -> "ccontract"
    CJSONCheckpoint j -> "json(" ++ prtty j ++ ")"

result :: Monoid o => StatefulContract i o a -> (o, Maybe a)
result c = let (o, c') = C.drain (lower c) in
            case c' of
                C.Pure a -> (o, Just a)
                _        -> (o, Nothing)

instance Functor (StatefulContract i o) where
    fmap f = \case
        CMap f' c -> CMap (f . f') c
        CAp l r     -> CAp (fmap (fmap f) l) r
        CBind m f'  -> CBind m (fmap f . f')

        CContract con -> CContract (fmap f con)
        CJSONCheckpoint c -> CMap f (CJSONCheckpoint c)

lower :: StatefulContract i o a -> C.Contract i o a
lower = \case
    CMap f c' -> f <$> lower c'
    CAp l r -> lower l <*> lower r
    CBind c' f -> lower c' >>= fmap lower f
    CContract c' -> c'
    CJSONCheckpoint c' -> lower c'

instance Applicative (StatefulContract i o) where
    pure = CContract . C.Pure
    (<*>) = CAp

instance Monad (StatefulContract i o) where
    (>>=) = CBind

instance MonadContract i o (StatefulContract i o) where
    emit o = CContract (emit o)
    waiting = CContract waiting
    select l r = CContract (select (lower l) (lower r))

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
      OpenLeaf (Seq i)
    | OpenBind (OpenRecord i o)
    | OpenLeft (OpenRecord i o) (ClosedRecord i o)
    | OpenRight (ClosedRecord i o) (OpenRecord i o)
    | OpenBoth (OpenRecord i o) (OpenRecord i o)
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

type Record i o = Either (OpenRecord i o) (ClosedRecord i o)

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
        OpenLeft or cr -> OpenLeft (go or) cr
        OpenRight cr or -> OpenRight cr (go or)
        OpenBoth or or' -> OpenBoth (go or) (go or')
        OpenBind or -> OpenBind (go or)

offer :: Monoid o => i -> StatefulContract i o a -> StatefulContract i o a
offer i = \case
    CMap f c' -> CMap f (offer i c')
    CAp l r -> CAp (offer i l) (offer i r)
    CBind l f ->
        case snd (result l) of
            Just _  -> CBind l (offer i . f)
            Nothing -> CBind (offer i l) f

    CContract c -> CContract (C.offer i c)
    CJSONCheckpoint c -> CJSONCheckpoint (offer i c)

applyInputs is c = foldr offer c is

runClosed
    :: ( MonadWriter o m
       , MonadError String m)
    => StatefulContract i o a
    -> ClosedRecord i o
    -> m a
runClosed con = \case
    ClosedLeaf (FinalEvents is) ->
        let con' = C.applyInputs (toList is) (lower con)
            (o, r) = C.drain con'
        in case C.result r of
            Left _  -> throwError "Closed contract not finished"
            Right a -> writer (a, o)
    ClosedLeaf (FinalJSON vl o) ->
        case con of
            CJSONCheckpoint _ ->
                case Aeson.parseEither Aeson.parseJSON vl of
                    Left e    -> throwError e
                    Right vl' -> writer (vl', o)
            _ -> throwError "Expected JSON checkpoint"
    ClosedBin l r ->
        case con of
            CMap f con' -> fmap f (runClosed con' (ClosedBin l r))
            CAp l' r'   -> runClosed l' l <*> runClosed r' l
            CBind l' f  -> runClosed l' l >>= flip runClosed r . f
            _           -> throwError "ClosedBin with wrong contract type"

runOpen
    :: ( Show i
       , Show o
       , Monoid o
       , MonadWriter o m
       , MonadError String m)
    => StatefulContract i o a
    -> OpenRecord i o
    -> m (Either (OpenRecord i o) (ClosedRecord i o, a))
runOpen con or =
    case (con, or) of
        (CMap f con', _) -> (fmap .fmap $ fmap f) (runOpen con' or)
        (CAp l r, OpenLeft or cr) -> do
            lr <- runOpen l or
            rr <- runClosed r cr
            case lr of
                Left or'       -> pure (Left (OpenLeft or' cr))
                Right (cr', a) -> pure (Right (ClosedBin cr' cr, a rr))
        (CAp l r, OpenRight cr or) -> do
            lr <- runClosed l cr
            rr <- runOpen r or
            case rr of
                Left or'       -> pure (Left (OpenRight cr or'))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', lr a))
        (CAp l r, OpenBoth orL orR) -> do
            lr <- runOpen l orL
            rr <- runOpen r orR
            case (lr, rr) of
                (Right (crL, a), Right (crR, b)) ->
                    pure (Right (ClosedBin crL crR, a b))
                (Right (crL, a), Left oR) ->
                    pure (Left (OpenRight crL oR))
                (Left oL, Right (cR, a)) ->
                    pure (Left (OpenLeft oL cR))
                (Left oL, Left oR) ->
                    pure (Left (OpenBoth oL oR))
        (CAp{}, OpenLeaf _) -> throwError "CAp OpenLeaf"

        (CBind c f, OpenBind bnd) -> do
            lr <- runOpen c bnd
            case lr of
                Left orL' -> pure (Left orL')
                Right (crL, a) -> do
                    let con' = f a
                        orR' = initialise con'
                    case orR' of
                        Right crrrr -> do
                            a <- runClosed con' crrrr
                            pure (Right (ClosedBin crL crrrr, a))
                        Left orrrr -> do
                            rr <- runOpen con' orrrr
                            case rr of
                                Left orR'' ->
                                    pure (Left (OpenRight crL orR''))
                                Right (crR, a) ->
                                    pure (Right (ClosedBin crL crR, a))

        (CBind c f, OpenRight cr or) -> do
            lr <- runClosed c cr
            rr <- runOpen (f lr) or
            case rr of
                Left or'       -> pure (Left (OpenRight cr or'))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', a))
        (CBind{}, _) -> throwError $ "CBind " ++ show or

        (CContract con, OpenLeaf is) ->
            case C.drain (C.applyInputs (toList is) con) of
                (o, C.Pure a) -> writer (Right (ClosedLeaf (FinalEvents is), a), o)
                (o, _)        -> writer (Left (OpenLeaf is), o)
        (CContract{}, _) -> throwError $ "CContract non leaf " ++ show or

        (CJSONCheckpoint con, or) -> do
            (r, o) <- listen (runOpen con or)
            pure $ fmap (\(cr, a) -> (jsonLeaf a o, a)) r
        _ -> throwError "runOpen"

insertAndUpdate
    :: (Show i, Show o, Monoid o)
    => StatefulContract i o a
    -> Record i o
    -> i
    -> Either String (Record i o, o)
insertAndUpdate con rc i =
    updateRecord con (insert i rc)

updateRecord
    :: (Show i, Show o, Monoid o)
    => StatefulContract i o a
    -> Record i o
    -> Either String (Record i o, o)
updateRecord con rc =
    case rc of
        Right cl -> fmap (first $ const $ Right cl) $ runExcept $ runWriterT $ runClosed con cl
        Left cl -> fmap (first (fmap fst)) $ runExcept $ runWriterT $ runOpen con cl
