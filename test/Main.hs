module Main where

import Test.Tasty
import TestParse (parseUnitTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "haskell-bencode Tests"
    [ parseUnitTests
    ]
