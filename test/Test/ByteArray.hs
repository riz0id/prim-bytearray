module Test.ByteArray
  ( testTree,
  )
where

import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty ()

import Data.ByteArray.Prim (ByteArray#)

import Test.ByteArray.Conversion qualified
import Test.ByteArray.Copy qualified
import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "ByteArray"
    [ Test.ByteArray.Conversion.testTree
    , Test.ByteArray.Copy.testTree
    ]