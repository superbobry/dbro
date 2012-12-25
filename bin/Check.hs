{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((<=<), replicateM_, void)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)

import Control.Monad.Trans (liftIO)

import Data.Bro.Backend.Disk (DiskBackend, makeDiskBackend)
import Data.Bro.Backend.Error (BackendError)
import Data.Bro.Backend.Result (BackendResult(Selected))
import Data.Bro.Monad (Bro, runBro_)
import Data.Bro.Types (ColumnType(..), ColumnValue(..), Statement(..),
                       Projection(..))
import qualified Data.Bro.Backend as Backend


main :: IO ()
main = withSystemTempDirectory "dbro" $ runBro_ go <=< makeDiskBackend
  where
    n :: Int
    n = 10000

    go :: Bro BackendError DiskBackend ()
    go = do
        liftIO . putStrLn $ printf "Inserting %i values ..." n
        void . Backend.exec $ CreateTable "test"
            [("a", IntegerColumn), ("b", DoubleColumn), ("c", VarcharColumn 255)]
        replicateM_ n $ Backend.exec $ InsertInto "test" []
            [IntegerValue 42, DoubleValue 42.0, VarcharValue "foobar"]
        liftIO . putStrLn $ printf "Selecting %i values ..." n
        Selected rows <-Backend.exec $ Select "test" (Projection []) Nothing
        mapM_ (liftIO . print) rows
