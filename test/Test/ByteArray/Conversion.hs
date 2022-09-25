module Test.ByteArray.Conversion
  ( testTree,
  )
where

import Hedgehog (forAll, property, (===))
import Test.Tasty ()

import Data.ByteArray.Prim (unpack#, pack#)

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
