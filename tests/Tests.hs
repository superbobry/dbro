module Main where

import Test.Framework (defaultMain)

import qualified Data.Bro.Parser.Tests

main :: IO ()
main = defaultMain
       [ Data.Bro.Parser.Tests.tests
       ]
