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

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Prompt              (MonadPrompt (..))
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Types                  as Aeson
import           Data.Bifunctor                    (Bifunctor (..))
import           Data.Foldable                     (toList)

import qualified Language.Plutus.Contract.Contract as C
import           Language.Plutus.Contract.Event    as Event
import           Language.Plutus.Contract.Hooks    as Hooks
import           Language.Plutus.Contract.Record

data StatefulContract a where
    CMap :: (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CAp :: StatefulContract  (a' -> a) -> StatefulContract  a' -> StatefulContract  a
    CBind :: StatefulContract  a' -> (a' -> StatefulContract  a) -> StatefulContract  a

    CContract :: C.ContractPrompt Maybe a -> StatefulContract  a
    CJSONCheckpoint :: (Aeson.FromJSON a, Aeson.ToJSON a) => StatefulContract  a -> StatefulContract  a

initialise
    :: ( MonadWriter Hooks m )
    => StatefulContract a
    -> m (Either (OpenRecord Event) (ClosedRecord Event, a))
initialise = \case
    CMap f con -> fmap (fmap f) <$> initialise con
    CAp conL conR -> do
        l' <- initialise conL
        r' <- initialise conR
        case (l', r') of
            (Left l, Left r)             -> pure $ Left (OpenBoth l r)
            (Right (l, _), Left r)       -> pure $ Left (OpenRight l r)
            (Left l, Right (r, _))       -> pure $ Left (OpenLeft l r)
            (Right (l, f), Right (r, a)) -> pure $ Right (ClosedBin l r, f a)
    CBind c f -> do
        l <- initialise c
        case l of
            Left l' -> pure $ Left (OpenBind l')
            Right (l', a) -> do
                r <- initialise (f a)
                case r of
                    Left r'       -> pure $ Left $ OpenRight l' r'
                    Right (r', b) -> pure $ Right (ClosedBin l' r', b)
    CContract con -> do
        r <- runConM mempty con
        case r of
            Nothing -> pure $ Left $ OpenLeaf mempty
            Just a  -> pure $ Right (ClosedLeaf (FinalEvents mempty), a)
    CJSONCheckpoint con -> do
        r <- initialise con
        case r of
            Left _       -> pure r
            Right (_, a) -> pure $ Right (jsonLeaf a, a)

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

instance Applicative StatefulContract where
    pure = CContract . pure
    (<*>) = CAp

-- TODO: Should we add an `Alt` constructor to `StatefulContract`?
instance Alternative StatefulContract where
    empty = CContract empty
    l <|> r = CContract (lower l <|> lower r)

instance Monad StatefulContract where
    (>>=) = CBind

instance MonadPrompt (Hook ()) Event StatefulContract where
    prompt = CContract . prompt

lowerM
    :: (Monad m)
    -- ^ What to do with map, ap, bind
    => (forall a'. (Aeson.FromJSON a', Aeson.ToJSON a') => m a' -> m a')
    -- ^ What to do with JSON checkpoints
    -> (forall a'. C.ContractPrompt Maybe a' -> m a')
    -- ^ What to do with the contracts
    -> StatefulContract a
    -> m a
lowerM fj fc = \case
    CMap f c' -> f <$> lowerM fj fc c'
    CAp l r -> lowerM fj fc l <*> lowerM fj fc r
    CBind c' f -> lowerM fj fc c' >>= fmap (lowerM fj fc) f
    CContract c' -> fc c'
    CJSONCheckpoint c' -> fj (lowerM fj fc c')

lower :: StatefulContract a -> C.ContractPrompt Maybe a
lower = lowerM id id

runConM
    :: ( MonadWriter Hooks m )
    => [Event]
    -> C.ContractPrompt Maybe a
    -> m (Maybe a)
runConM evts con = evalStateT (C.runContract con) evts

runClosed
    :: ( MonadWriter Hooks m
       , MonadError String m)
    => StatefulContract a
    -> ClosedRecord Event
    -> m a
runClosed con rc =
    case con of
        CMap f c' -> fmap f (runClosed c' rc)
        _ -> case rc of
                ClosedLeaf (FinalEvents evts) ->
                    case con of
                        CContract con' -> do
                            r <- runConM (toList evts) con'
                            case r of
                                Nothing -> throwError "ClosedLeaf, contract not finished"
                                Just  a -> pure a
                        _ -> throwError "ClosedLeaf, expected CContract "
                ClosedLeaf (FinalJSON vl) ->
                    case con of
                        CJSONCheckpoint _ ->
                            case Aeson.parseEither Aeson.parseJSON vl of
                                Left e    -> throwError e
                                Right vl' -> writer (vl', mempty)
                        _ -> throwError ("Expected JSON checkpoint, got " ++ prtty con)
                ClosedBin l r ->
                    case con of
                        CMap f con' -> fmap f (runClosed con' (ClosedBin l r))
                        CAp l' r'   -> runClosed l' l <*> runClosed r' l
                        CBind l' f  -> runClosed l' l >>= flip runClosed r . f
                        _           -> throwError "ClosedBin with wrong contract type"

runOpen
    :: ( MonadWriter Hooks m
       , MonadError String m)
    => StatefulContract a
    -> OpenRecord Event
    -> m (Either (OpenRecord Event) (ClosedRecord Event, a))
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
                Left orL' -> pure (Left $ OpenBind orL')
                Right (crL, a) -> do
                    let con' = f a
                    orR' <- initialise con'
                    case orR' of
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
                r <- runConM (toList is) con'
                case r of
                    Just a  -> pure (Right (ClosedLeaf (FinalEvents is), a))
                    Nothing -> pure (Left (OpenLeaf is))
        (CContract{}, _) -> throwError $ "CContract non leaf " ++ show opr

        (CJSONCheckpoint con', opr') ->
            fmap (\(_, a) -> (jsonLeaf a, a)) <$> runOpen con' opr'
        _ -> throwError "runOpen"

insertAndUpdate
    :: StatefulContract a
    -> Record Event
    -> Event
    -> Either String (Record Event, Hooks)
insertAndUpdate con rc e = updateRecord con (insert e rc)

updateRecord
    :: StatefulContract  a
    -> Record Event
    -> Either String (Record Event, Hooks)
updateRecord con rc =
    case rc of
        Right cl ->
            fmap (first $ const $ Right cl)
            $ runExcept
            $ runWriterT
            $ runClosed con cl
        Left cl  ->
            fmap (first (fmap fst))
            $ runExcept
            $ runWriterT
            $ runOpen con cl
