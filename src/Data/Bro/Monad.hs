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

newtype Bro e s a = Bro { unBro :: StateT s (ErrorT e IO) a }
    deriving (Functor, Applicative, Monad, MonadIO,
              (MonadState s), (MonadError e))


runBro :: Error e => Bro e s a -> s -> IO (Either e a)
runBro (Bro m) = runErrorT . evalStateT m

runBro_ :: Error e => Bro e s a -> s -> IO ()
runBro_ m = void . runBro m
