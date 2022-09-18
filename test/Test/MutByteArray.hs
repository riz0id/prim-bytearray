module Test.MutByteArray
  ( testTree,
  )
where

import Test.Tasty ()

import Test.Compat (TestTree, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "MutByteArray"
    []
