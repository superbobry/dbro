{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.Bro.Monad
  ( Bro
  , runBro
  , runBro_
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, void)

import Control.Monad.Base (MonadBase(..))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Error (MonadError, Error, ErrorT, runErrorT)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (ResourceT, MonadActive, MonadResource,
                                     MonadThrow, MonadUnsafeIO, runResourceT)

newtype Bro e s a = Bro { unBro :: StateT s (ErrorT e (ResourceT IO)) a }
    deriving (Functor, Applicative, Monad, MonadIO,
              (MonadState s), (MonadError e),
              MonadActive, MonadResource, MonadThrow, MonadUnsafeIO)

instance Error e => MonadBase IO (Bro e s) where
    liftBase = Bro . liftBase

instance Error e => MonadBaseControl IO (Bro e s) where
    newtype StM (Bro e s) a = StBro { unStBro :: StM (StateT s (ErrorT e (ResourceT IO))) a }

    liftBaseWith f = Bro . liftBaseWith $ \run -> f $ liftM StBro . run . unBro

    restoreM = Bro . restoreM . unStBro


runBro :: Error e => Bro e s a -> s -> IO (Either e a)
runBro (Bro m) = runResourceT . runErrorT . evalStateT m

runBro_ :: Error e => Bro e s a -> s -> IO ()
runBro_ m = void . runBro m
