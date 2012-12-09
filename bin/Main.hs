{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Main where

import Control.Monad (forever, void)
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
import Data.Bro.Monad (Bro, runBro)
import Data.Bro.Parser (statement)
import Data.Bro.Types (Row(..), ColumnType(..), ColumnValue(..), Table(..))

instance ToField ColumnValue where
    toField (IntegerValue i) = toField i
    toField (DoubleValue d) = toField d
    toField (VarcharValue s) = toField s
    {-# INLINE toField #-}

instance ToRecord Row where
    toRecord = toRecord . rowData
    {-# INLINE toRecord #-}

main :: IO ()
main = void $ runBro (forever process) =<< makeDiskBackend "." where
  process :: (Backend b, Query b) => Bro BackendError b ()
  process = flip catchError handleError $ do
      l   <- liftIO S.getLine
      case parseOnly statement l of
          Left err -> throwError (strMsg err)
          Right s  -> exec s >>= liftIO . putStrLnLn . formatResult

  putStrLnLn :: L.ByteString -> IO ()
  putStrLnLn s = L.putStrLn s >> IO.putChar '\n'

  handleError :: Backend b => BackendError -> Bro BackendError b ()
  handleError = liftIO . putStrLnLn . formatError

  formatError :: BackendError -> L.ByteString
  formatError = L.pack . show  -- FIXME(Sergei): descriptive error messages.

  formatResult :: BackendResult -> L.ByteString
  formatResult Created = "OK"
  formatResult (Inserted rowId0) = L.pack $ printf "OK %i" rowId0
  formatResult (Selected (Table { tabSchema = (schema, _) }) rows) =
      -- FIXME(Sergei): switch to 'Data.Vector' for [Row]?
      L.concat $ filter (not . L.null) [header, body]
    where
      header :: L.ByteString
      header = Csv.encode $ V.singleton $ do
          (name, t) <- schema
          return $! S.append name $ case t of
              IntegerColumn   -> "(int)"
              DoubleColumn    -> "(double)"
              VarcharColumn _ -> "(string)"

      body :: L.ByteString
      body = Csv.encode $ V.fromList rows
