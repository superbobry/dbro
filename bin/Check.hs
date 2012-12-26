{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (replicateM_, void, when)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive,
                         doesDirectoryExist)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as S
import qualified System.IO as IO

import Control.Monad.Trans (liftIO)
import Data.Conduit (($$), ($=))
import Data.Conduit.Binary (sinkHandle)
import qualified Data.Conduit.List as CL

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
    exists <- doesDirectoryExist root
    when exists $ removeDirectoryRecursive root
    createDirectoryIfMissing True root
    runBro_ go =<< makeDiskBackend root
  where
    n :: Int
    n = 200000

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
        replicateM_ n $ Backend.exec $ InsertInto name []
            [IntegerValue 42, DoubleValue 42.0, VarcharValue "foobar"]
        liftIO $ do
            size <- S.length <$> S.readFile (root </> S.unpack name)
            putStrLn $ printf "Table size: %i" size
        liftIO . putStrLn $ printf "Selecting %i values ..." n
        Selected sourceRow <- Backend.exec $ Select name (Projection []) Nothing
        sourceRow $= CL.map (S.pack . show) $$ sinkHandle IO.stdout
