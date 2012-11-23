module Main where

import Control.Applicative ((<$>))
import Control.Monad (forever, void)

import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Data.Bro.Parser (statement)
import Data.Bro.Backend (Backend(..), BackendError, exec)
import Data.Bro.Backend.Memory (makeMemoryBackend)
import Data.Bro.Monad (Bro, runBro)

main :: IO ()
main = void $ runBro (forever process) makeMemoryBackend where
  process :: Backend b => Bro BackendError b ()
  process = do
      l   <- liftIO S.getLine
      res <- case parseOnly statement l of
          Left err -> return . S.pack $ "ParseError: " ++ err
          Right s  -> S.pack . show <$> exec s
      liftIO $! S.putStrLn res