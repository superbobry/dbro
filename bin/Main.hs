{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import Control.Monad (forever, unless)
import Text.Printf (printf)
import qualified System.IO as IO

import Control.Monad.Error (throwError, catchError, strMsg)
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Conduit (($$), ($=))
import Data.Conduit.Binary (sinkHandle)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit.List as CL

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

  formatResult :: (Backend b, Query b) => BackendResult b -> Bro BackendError b ()
  formatResult r = case r of
      Created   -> liftIO $ putStrLn "OK"
      CreatedIndex -> liftIO $ putStrLn "OK"
      Updated n -> liftIO . putStrLn $ printf "OK %i" n
      Deleted n -> liftIO . putStrLn $ printf "OK %i" n
      Inserted rowId -> liftIO . putStrLn $ printf "OK %i" rowId
      Selected sourceRow ->
          sourceRow $= CL.map f $= CL.map newLine $$ sinkHandle IO.stdout
        where
          f (Row { rowData }) = S.intercalate "," $ map (S.pack . show) rowData
          --FIXME(Misha): O(n) complexity here
          newLine l = l `S.append` (S.pack "\n")
