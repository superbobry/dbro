{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Main where

import Control.Monad (forever, unless)
import Text.Printf (printf)
import qualified System.IO as IO

import Control.Monad.Error (throwError, catchError, strMsg)
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Csv (ToField(..), ToRecord(..))
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Data.Bro.Backend (exec)
import Data.Bro.Backend.Class (Backend(..), Query)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (BackendResult(..))
import Data.Bro.Backend.Disk (makeDiskBackend)
import Data.Bro.Monad (Bro, runBro_)
import Data.Bro.Parser (statement)
import Data.Bro.Types (Row(..), ColumnValue(..))

instance ToField ColumnValue where
    toField (IntegerValue i) = toField i
    toField (DoubleValue d) = toField d
    toField (VarcharValue s) = toField s
    {-# INLINE toField #-}

instance ToRecord Row where
    toRecord = toRecord . rowData
    {-# INLINE toRecord #-}

main :: IO ()
main = runBro_ (forever process) =<< makeDiskBackend "." where
  process :: (Backend b, Query b) => Bro BackendError b ()
  process = flip catchError handleError $ do
      l   <- liftIO S.getLine
      case parseOnly statement l of
          Left err -> throwError (strMsg err)
          Right s  -> exec s >>= liftIO . putStrLnLn . formatResult

  putStrLnLn :: L.ByteString -> IO ()
  putStrLnLn s = do
      unless (L.null s) $ L.putStrLn s
      IO.putChar '\n'

  handleError :: Backend b => BackendError -> Bro BackendError b ()
  handleError = liftIO . putStrLnLn . formatError

  formatError :: BackendError -> L.ByteString
  formatError = L.pack . show  -- FIXME(Sergei): descriptive error messages.

  formatResult :: BackendResult -> L.ByteString
  formatResult Created = "OK"
  formatResult (Updated nUpd) = L.pack $ printf "OK %i" nUpd
  formatResult (Deleted nDel) = L.pack $ printf "OK %i" nDel
  formatResult (Inserted rowId0) = L.pack $ printf "OK %i" rowId0
  formatResult (Selected rows) = let s = Csv.encode $ V.fromList rows in
      if L.null s then s else L.init s
