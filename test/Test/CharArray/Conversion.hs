module Test.CharArray.Conversion
  ( testTree,
  )
where

import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty ()

import Data.CharArray.Prim (CharArray#, unpack#, pack#)

import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Conversion"
    [ testProp "String" $ property do
        str <- forAll Gen.list'char
        unpack# (pack# str) === str
    ]
