module Test.ByteArray.Ord
  ( testTree,
  )
where

import Data.Bool.Prim qualified as Bool
import Data.ByteArray.Prim (unpack#)
import Data.Ord.Prim (Eq# (..), Ord# (..))
import Data.Ord.Prim qualified as Ord
import Data.Primitive (ByteArray (ByteArray))

import Hedgehog (forAll, property, (===))

import Test.Compat (TestTree, testGroup, testProp)
import Test.Gen qualified as Gen
import Test.Tasty ()

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Ord"
    [ testProp "(==#)" $ property do
        (ByteArray xs#) <- forAll Gen.bytearray
        (ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# ==# ys#) === (unpack# xs# == unpack# ys#)
    , testProp "(/=#)" $ property do
        (ByteArray xs#) <- forAll Gen.bytearray
        (ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# /=# ys#) === (unpack# xs# /= unpack# ys#)
    , testProp "compare#" $ property do
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        ys@(ByteArray ys#) <- forAll Gen.bytearray
        Ord.toOrdering (compare# xs# ys#) === (compare xs ys)
    , testProp "(>#)" $ property do
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        ys@(ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# ># ys#) === (xs > ys)
    , testProp "(>=#)" $ property do
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        ys@(ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# >=# ys#) === (xs >= ys)
    , testProp "(<#)" $ property do
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        ys@(ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# <# ys#) === (xs < ys)
    , testProp "(<=#)" $ property do
        xs@(ByteArray xs#) <- forAll Gen.bytearray
        ys@(ByteArray ys#) <- forAll Gen.bytearray
        Bool.toBool (xs# <=# ys#) === (xs <= ys)
    ]