{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad ( replicateM_, void)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as S

import Control.Monad.Trans (liftIO)

import Data.Bro.Backend.Disk (DiskBackend, makeDiskBackend)
import Data.Bro.Backend.Error (BackendError)
import Data.Bro.Backend.Result (BackendResult(Selected))
import Data.Bro.Monad (Bro, runBro_)
import Data.Bro.Types (ColumnType(..), ColumnValue(..), Statement(..),
                       Projection(..), TableName, TableSchema)
import Data.Bro.Backend.Util (rowSize)
import qualified Data.Bro.Backend as Backend


main :: IO ()
main = do
    removeDirectoryRecursive root
    createDirectoryIfMissing True root
    runBro_ go =<< makeDiskBackend root
  where
    n :: Int
    n = 10000

    root :: FilePath
    root = "check"

    name :: TableName
    name = "test"

    schema :: TableSchema
    schema = [("a", IntegerColumn), ("b", DoubleColumn), ("c", VarcharColumn 255)]

    go :: Bro BackendError DiskBackend ()
    go = do
        liftIO $ do
            putStrLn $ printf "Row size: %i bytes" (rowSize schema)
            putStrLn $ printf "Inserting %i values ..." n
        void . Backend.exec $ CreateTable name schema
        liftIO $ do
            size <- S.length <$> S.readFile (root </> S.unpack name)
            putStrLn $ printf "Table size: %i" size
        replicateM_ n $ Backend.exec $ InsertInto name []
            [IntegerValue 42, DoubleValue 42.0, VarcharValue "foobar"]
        liftIO . putStrLn $ printf "Selecting %i values ..." n
        Selected rows <-Backend.exec $ Select name (Projection []) Nothing
        mapM_ (liftIO . print) rows
