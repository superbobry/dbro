module Main where

import Prelude hiding (getLine, putStrLn)

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Data.Text.IO (getLine, putStrLn)
import qualified Data.Text as T

import Data.Bro.Parser (statement)
import Data.Bro.Backend (Backend(..), exec)
import Data.Bro.Backend.Memory (makeMemoryBackend)


main :: IO ()
main = do
    bref <- newIORef makeMemoryBackend
    forever $ do
        l <- getLine
        b <- readIORef bref
        let (b', result) = process b l
        putStrLn result
        writeIORef bref b'
  where
    process :: Backend b => b -> Text -> (b, Text)
    process b l = case parseOnly statement l of
        Left err -> (b, T.pack $ "ParseError: " ++ err)
        Right s  ->
            case exec b s of
                Left err -> (b, T.pack $ "Error: " ++ show err)
                Right (b', result) -> (b', T.pack $ show result)
