module Main where

import Test.Framework (defaultMain)

import qualified Data.Bro.Parser.Tests
import qualified Data.Bro.Backend.Memory.Tests

main :: IO ()
main = defaultMain
       [ Data.Bro.Parser.Tests.tests
       , Data.Bro.Backend.Memory.Tests.tests
       ]
