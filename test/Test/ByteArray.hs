module Test.ByteArray
  ( testTree,
  )
where

import Hedgehog (forAll, property, (===))
import Test.Tasty ()

import Data.ByteArray.Prim (size#)
import Data.Primitive (ByteArray (ByteArray), sizeofByteArray)

import Test.ByteArray.Conversion qualified
import Test.ByteArray.Copy qualified
import Test.ByteArray.Ord qualified
import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen
import GHC.Exts (Int(I#))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "ByteArray#"
    [ Test.ByteArray.Conversion.testTree
    , Test.ByteArray.Copy.testTree
    , Test.ByteArray.Ord.testTree
    , testProp "size#" $ property do 
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        I# (size# xs#) === sizeofByteArray xs
    ]