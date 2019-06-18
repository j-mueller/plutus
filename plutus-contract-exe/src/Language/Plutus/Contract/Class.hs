{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Plutus.Contract.Class where

import           Control.Applicative  (Alternative (..), liftA2)
import           Control.Monad.Prompt (MonadPrompt (..))

type MonadContract i o m = MonadPrompt o i m

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

await :: (Alternative m, Monad m, MonadContract i o m) => o -> (i -> Maybe a) -> m a
await i f = do
    o <- prompt i
    maybe empty pure (f o)

both :: (MonadContract i o f) => f a -> f b -> f (a, b)
both = liftA2 (,)

selectEither :: (Alternative f) => f a -> f b -> f (Either a b)
selectEither l r = (Left <$> l) <|> (Right <$> r)
