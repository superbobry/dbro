{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Data.Bro.Monad
  ( Bro
  , runBro
  , runBro_
  ) where

import Control.Applicative (Applicative)
import Control.Monad (void)

import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Error (MonadError, Error, ErrorT, runErrorT)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Resource (ResourceT, MonadResource, MonadThrow,
                                     MonadUnsafeIO, runResourceT)

newtype Bro e s a = Bro { unBro :: StateT s (ErrorT e (ResourceT IO)) a }
    deriving (Functor, Applicative, Monad, MonadIO,
              (MonadState s), (MonadError e),
              MonadResource, MonadThrow, MonadUnsafeIO)


runBro :: Error e => Bro e s a -> s -> IO (Either e a)
runBro (Bro m) = runResourceT . runErrorT . evalStateT m

runBro_ :: Error e => Bro e s a -> s -> IO ()
runBro_ m = void . runBro m
