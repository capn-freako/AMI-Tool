{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
  #-}

module EitherT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

newtype EitherT e m a = EitherT {
    runEitherT :: m (Either e a)
  }

bindET :: (Monad m) => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
x `bindET` f = EitherT $ do
    unwrapped <- runEitherT x
    case unwrapped of
        Left err -> return $ Left err
        Right y  -> runEitherT (f y)

returnET :: (Monad m) => a -> EitherT e m a
returnET a = EitherT $ return (Right a)

failET :: (Monad m) => t -> EitherT e m a
failET _ = EitherT $ return (Left (error "sth"))
 
instance (Monad m) => Monad (EitherT e m) where
    return = returnET
    (>>=)  = bindET
    fail   = failET

instance MonadTrans (EitherT e) where
    lift m = EitherT (Right `liftM` m)

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (EitherT e m) where
    get   = lift get
    put k = lift (put k)

