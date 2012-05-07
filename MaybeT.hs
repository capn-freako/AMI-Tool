-- The following definition of `MaybeT' was taken directly from `Real World Haskell'.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module MaybeT where

import Control.Monad
import Control.Monad.Trans

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
  }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do unwrapped <- runMaybeT x
                           case unwrapped of
                               Nothing -> return Nothing
                               Just y  -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=)  = bindMT
    fail   = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)
