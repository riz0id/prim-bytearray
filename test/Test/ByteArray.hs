
module Test.ByteArray 
  ( testTree
  ) 
where 

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty ()

import Data.ByteArray.Prim (ByteArray#)

import Test.Compat (TestTree, testGroup, testProp)
import Test.ByteArray.Conversion qualified
import Test.ByteArray.Copy qualified

--------------------------------------------------------------------------------

testTree :: TestTree 
testTree =
  testGroup 
    "ByteArray"
    [ Test.ByteArray.Conversion.testTree
    , Test.ByteArray.Copy.testTree
    ]