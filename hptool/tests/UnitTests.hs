module Main
    ( main
    ) where

import Test.Tasty

import qualified UnitTests.CLArgs


tests :: IO TestTree
tests = do
    clArgs <- UnitTests.CLArgs.tests
    return $ testGroup "Unit Tests" [ clArgs ]

main :: IO ()
main = tests >>= defaultMain
