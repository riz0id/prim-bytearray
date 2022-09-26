{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Test.Tasty (defaultMain)

import Test.ByteArray qualified
import Test.Compat (TestTree, testGroup)
import Test.MutByteArray qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.ByteArray.testTree
    , Test.MutByteArray.testTree
    ]