{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import Control.Monad (forever, unless, forM_)
import Text.Printf (printf)
import qualified System.IO as IO

import Control.Monad.Error (throwError, catchError, strMsg)
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Data.Bro.Backend (exec)
import Data.Bro.Backend.Class (Backend(..), Query)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (BackendResult(..))
import Data.Bro.Backend.Disk (makeDiskBackend)
import Data.Bro.Monad (Bro, runBro_)
import Data.Bro.Parser (statement)
import Data.Bro.Types (Row(..))

putBSLn :: Backend b => S.ByteString -> Bro BackendError b ()
putBSLn s = liftIO $ unless (S.null s) $ S.putStrLn s

putBSLnLn :: Backend b => S.ByteString -> Bro BackendError b ()
putBSLnLn s = putBSLn s >> liftIO (IO.putChar '\n')

main :: IO ()
main = runBro_ (forever loop) =<< makeDiskBackend "." where
  loop :: (Backend b, Query b) => Bro BackendError b ()
  loop = flip catchError formatError $ do
      l   <- liftIO S.getLine
      case parseOnly statement l of
          Left err -> throwError (strMsg err)
          Right s  -> exec s >>= formatResult >> liftIO (IO.putChar '\n')

  formatError :: Backend b => BackendError -> Bro BackendError b ()
  formatError =
      -- FIXME(Sergei): descriptive error messages.
      putBSLnLn . S.pack . show

  formatResult :: (Backend b, Query b) => BackendResult -> Bro BackendError b ()
  formatResult r = case r of
      Created   -> liftIO $ putStrLn "OK"
      Updated n -> liftIO . putStrLn $ printf "OK %i" n
      Deleted n -> liftIO . putStrLn $ printf "OK %i" n
      Inserted rowId -> liftIO . putStrLn $ printf "OK %i" rowId
      Selected rows  -> forM_ rows $ \(Row { rowData }) ->
          liftIO . S.putStrLn $ S.intercalate "," (map (S.pack . show) rowData)
