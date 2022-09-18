module Test.ByteArray.Conversion
  ( testTree,
  )
where

import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty ()

import Data.ByteArray.Prim (ByteArray#, unpack#, pack#)

import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Conversion"
    [ testProp "String" $ property do
        str <- forAll Gen.list'word8 
        unpack# (pack# str) === str
    ]
