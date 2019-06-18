{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | A version of 'Language.Plutus.Contract.Contract' that
--   writes checkpoints
module Language.Plutus.Contract.State where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Aeson                         as Aeson
import qualified Data.Aeson.Types                   as Aeson
import           Data.Bifunctor                     (Bifunctor (..))
import           Data.Foldable                      (toList)
import qualified Data.Map                           as Map

import qualified Language.Plutus.Contract.Contract  as C
import           Language.Plutus.Contract.Event     as Event
import           Language.Plutus.Contract.Hooks     as Hooks
import           Language.Plutus.Contract.Record
import           Language.Plutus.Contract.RequestId

data StatefulContract a where
    CMap :: (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CAp :: StatefulContract  (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CBind :: StatefulContract  a' -> (a' -> StatefulContract  a) -> StatefulContract  a

    CContract :: C.ContractPrompt (Either Hooks) a -> StatefulContract  a
    CJSONCheckpoint :: (Aeson.FromJSON a, Aeson.ToJSON a) => StatefulContract  a -> StatefulContract  a

initialise
    :: RequestId
    -> StatefulContract a
    -> (Rec Event Hooks a, RequestId)
initialise = undefined --  lower _ _ c

checkpoint :: (Aeson.FromJSON a, Aeson.ToJSON a) => StatefulContract  a -> StatefulContract  a
checkpoint = CJSONCheckpoint

prtty :: StatefulContract  a -> String
prtty = \case
    CMap _ c -> "cmap (" ++ prtty c ++ ")"
    CAp l r -> "cap (" ++ prtty l ++ ") (" ++ prtty r ++ ")"
    CBind l _ -> "cbind (" ++ prtty l ++  ") f"
    CContract _ -> "ccontract"
    CJSONCheckpoint j -> "json(" ++ prtty j ++ ")"

instance Functor StatefulContract where
    fmap f = \case
        CMap f' c -> CMap (f . f') c
        CAp l r     -> CAp (fmap (fmap f) l) r
        CBind m f'  -> CBind m (fmap f . f')

        CContract con -> CContract (fmap f con)
        CJSONCheckpoint c -> CMap f (CJSONCheckpoint c)

lower
    :: (Monad m)
    -- ^ What to do with map, ap, bind
    => (forall a'. (Aeson.FromJSON a', Aeson.ToJSON a') => m a' -> m a')
    -- ^ What to do with JSON checkpoints
    -> (forall a'. C.ContractPrompt (Either Hooks) a' -> m a')
    -- ^ What to do with the contracts
    -> StatefulContract a
    -> m a
lower fj fc = \case
    CMap f c' -> f <$> lower fj fc c'
    CAp l r -> lower fj fc l <*> lower fj fc r
    CBind c' f -> lower fj fc c' >>= fmap (lower fj fc) f
    CContract c' -> fc c'
    CJSONCheckpoint c' -> fj (lower fj fc c')

instance Applicative StatefulContract where
    pure = CContract . pure
    (<*>) = CAp

instance Monad StatefulContract where
    (>>=) = CBind

runCon :: C.ContractPrompt (Either Hooks) a -> Map.Map RequestId Event -> RequestId -> (Maybe a, RequestId, Hooks)
runCon con es i =
    let (r, i') = C.runContract con (C.InstanceState es i)
    in case r of
        Left h  -> (Nothing, i', h)
        Right a -> (Just a, i', mempty)

runConM
    :: MonadState RequestId m
    => C.ContractPrompt (Either Hooks) a
    -> [Event]
    -> m (Maybe a, Hooks)
runConM con es = do
    s <- get
    let (r, s', h) = runCon con es s
    put s'
    return (r, h)

runClosed
    :: ( MonadWriter Hooks m
       , MonadState RequestId m
       , MonadError String m)
    => StatefulContract  a
    -> ClosedRecord Event Hooks
    -> m a
runClosed con = \case
    ClosedLeaf (FinalEvents is) ->
        case con of
            CContract con' -> do
                (r, h) <- runConM con' (toList is)
                case r of
                    Nothing -> throwError "ClosedLeaf, contract not finished"
                    Just  a -> writer (a, h)
            _ -> throwError "ClosedLeaf, expected CContract "
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
    :: ( MonadWriter Hooks m
       , MonadState RequestId m
       , MonadError String m)
    => StatefulContract  a
    -> OpenRecord Event Hooks
    -> m (Either (OpenRecord Event Hooks) (ClosedRecord Event Hooks, a))
runOpen con opr =
    case (con, opr) of
        (CMap f con', _) -> (fmap .fmap $ fmap f) (runOpen con' opr)
        (CAp l r, OpenLeft opr' cr) -> do
            lr <- runOpen l opr'
            rr <- runClosed r cr
            case lr of
                Left opr''     -> pure (Left (OpenLeft opr'' cr))
                Right (cr', a) -> pure (Right (ClosedBin cr' cr, a rr))
        (CAp l r, OpenRight cr opr') -> do
            lr <- runClosed l cr
            rr <- runOpen r opr'
            case rr of
                Left opr''     -> pure (Left (OpenRight cr opr''))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', lr a))
        (CAp l r, OpenBoth orL orR) -> do
            lr <- runOpen l orL
            rr <- runOpen r orR
            case (lr, rr) of
                (Right (crL, a), Right (crR, b)) ->
                    pure (Right (ClosedBin crL crR, a b))
                (Right (crL, _), Left oR) ->
                    pure (Left (OpenRight crL oR))
                (Left oL, Right (cR, _)) ->
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
                    i <- get
                    let (orR', i') = initialise i con'
                    put i'
                    case unRec orR' of
                        Right (crrrr, a') -> pure (Right (ClosedBin crL crrrr, a'))
                        Left orrrr -> do
                            rr <- runOpen con' orrrr
                            case rr of
                                Left orR'' ->
                                    pure (Left (OpenRight crL orR''))
                                Right (crR, a') ->
                                    pure (Right (ClosedBin crL crR, a'))

        (CBind c f, OpenRight cr opr') -> do
            lr <- runClosed c cr
            rr <- runOpen (f lr) opr'
            case rr of
                Left opr''     -> pure (Left (OpenRight cr opr''))
                Right (cr', a) -> pure (Right (ClosedBin cr cr', a))
        (CBind{}, _) -> throwError $ "CBind " ++ show opr

        (CContract con', OpenLeaf is) -> do
                (r, h) <- runConM con' (toList is)
                case r of
                    Just a  -> writer (Right (ClosedLeaf (FinalEvents is), a), h)
                    Nothing -> writer (Left (OpenLeaf is), h)
        (CContract{}, _) -> throwError $ "CContract non leaf " ++ show opr

        (CJSONCheckpoint con', opr') -> do
            (r, o) <- listen (runOpen con' opr')
            pure $ fmap (\(_, a) -> (jsonLeaf a o, a)) r
        _ -> throwError "runOpen"

insertAndUpdate
    :: StatefulContract  a
    -> Record Event Hooks
    -> Event
    -> Either String (Record Event Hooks, Hooks)
insertAndUpdate con rc i = updateRecord con (insert i rc)

updateRecord
    :: StatefulContract  a
    -> Record Event Hooks
    -> Either String (Record Event Hooks, Hooks)
updateRecord con rc =
    case rc of
        Right cl -> fmap (first $ const $ Right cl) $ runExcept $ flip evalStateT 0 $ runWriterT $ runClosed con cl
        Left cl  -> fmap (first (fmap fst)) $ runExcept $ flip evalStateT 0 $ runWriterT $ runOpen con cl
