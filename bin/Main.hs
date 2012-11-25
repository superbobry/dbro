module Main where

import Control.Monad (forever, void)

import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Data.Bro.Parser (statement)
import Data.Bro.Backend (exec)
import Data.Bro.Backend.Class (Backend(..), Query)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (Result(..), format)
import Data.Bro.Backend.Disk (makeDiskBackend)
import Data.Bro.Monad (Bro, runBro)

main :: IO ()
main = void $ runBro (forever process) =<< makeDiskBackend "." where
  process :: (Backend b, Query b) => Bro BackendError b ()
  process = do
      l   <- liftIO S.getLine
      res <- case parseOnly statement l of
          Left err -> return . S.pack $ "ParseError: " ++ err
          Right s  -> do
              Result res <- exec s `catchError` (return . Result)
              return $! format res
      liftIO $! S.putStrLn res