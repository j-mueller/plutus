{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Language.Plutus.Contract.Class where

import           Control.Applicative            (liftA2)

class Monad m => MonadContract i o m | m -> i, m -> o where
    emit :: o -> m ()
    waiting :: m i

    -- | Run both contracts and return the first one that finishes
    --   @select (pure a) _ = pure a@
    --   @fmap f (select a b) == select (fmap f a) (fmap f b)@
    select :: m a -> m a -> m a

-- | A monadic version of 'loop', where the predicate returns 'Left' as a seed 
--   for the next loop or 'Right' to abort the loop.
--
-- https://hackage.haskell.org/package/extra-1.6.15/docs/src/Control.Monad.Extra.html#loopM
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x' -> loopM act x'
        Right v -> return v

-- | Repeatedly evaluate the action until it yields 'Nothing',
--   then return the aggregated result.
foldMaybe
    :: Monad m
    => (a -> b -> b)
    -> b
    -> m (Maybe a)
    -> m b
foldMaybe f b con = loopM go b where
    go b' = maybe (Left b') (Right . flip f b') <$> con

await :: MonadContract i o m => o -> (i -> Maybe a) -> m a
await a f = do
    emit a
    i <- waiting
    case f i of
        Nothing -> await a f
        Just i' -> pure i'

both :: MonadContract i o m => m a -> m b -> m (a, b)
both = liftA2 (,)

selectEither :: MonadContract i o m => m a -> m b -> m (Either a b)
selectEither l r = select (Left <$> l) (Right <$> r)
