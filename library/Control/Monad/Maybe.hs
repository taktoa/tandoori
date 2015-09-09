--{-# OPTIONS_GHC -fglasgow-exts  -fallow-undecidable-instances #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A monad transformer for Maybe
module Control.Monad.Maybe ( MaybeT
                           , runMaybeT
                           , module Control.Monad
                           , module Control.Monad.Trans
                           ) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad        (mplus, mzero)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Writer

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
  fmap f x = MaybeT $ fmap f <$> runMaybeT x

instance Monad m => Applicative (MaybeT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Alternative (MaybeT m) where
  (<|>) = mplus
  empty = mzero

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _  = MaybeT $ return Nothing

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT $ return Nothing
  mplus x y = MaybeT $ do
    mx <- runMaybeT x
    case mx of
      Nothing -> runMaybeT y
      Just _  -> return mx

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (MaybeT m) where
  ask = lift ask
  local f = MaybeT . local f . runMaybeT

instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell

  listen m = MaybeT $ do
    (mv, w) <- listen (runMaybeT m)
    case mv of
      Nothing -> return Nothing
      Just v  -> return $ Just (v, w)

  pass m = MaybeT $ do
    mvf <- runMaybeT m
    case mvf of
      Nothing    -> return Nothing
      Just (v,f) -> pass $ return (Just v, f)
