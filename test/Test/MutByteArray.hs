
module Test.MutByteArray 
  ( tests
  ) 
where 

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty ()

import Data.ByteArray.Prim (ByteArray#)

import Test.Compat (TestTree, testGroup, testProp)

--------------------------------------------------------------------------------

tests :: TestTree 
tests =
  testGroup 
    "ByteArray"
    [
    ]

