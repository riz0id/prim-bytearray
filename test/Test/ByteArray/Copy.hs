module Test.ByteArray.Copy
  ( testTree,
  )
where

import Control.Monad.Primitive (primitive)

import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.ByteArray.Prim (clone#, pack#, slice#, unpack#)

import GHC.Exts (Int (I#))

import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen
import Test.Tasty ()

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Copy"
    [ testProp "slice#" $ property do
        elts <- forAll Gen.list'word8
        i0@(I# i0#) <- forAll (Gen.int $ Range.constant 0 $ length elts)
        i1@(I# i1#) <- forAll (Gen.int $ Range.constant i0 $ length elts)
        sub <- primitive \st0# ->
          case slice# (pack# elts) i0# i1# st0# of
            (# st1#, xs# #) -> (# st1#, unpack# xs# #)
        sub === take (i1 - i0) (drop i0 elts)
    , testProp "clone#" $ property do
        elts <- forAll Gen.list'word8
        cpy <- primitive \st0# ->
          case clone# (pack# elts) st0# of
            (# st1#, xs# #) -> (# st1#, unpack# xs# #)
        cpy === elts
    ]
