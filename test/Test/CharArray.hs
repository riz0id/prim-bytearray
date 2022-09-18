
module Test.CharArray 
  ( testTree
  ) 
where 

import Test.Tasty ()

import Test.Compat (TestTree, testGroup)
import Test.CharArray.Conversion qualified
import Test.CharArray.Copy qualified

--------------------------------------------------------------------------------

testTree :: TestTree 
testTree =
  testGroup 
    "CharArray"
    [ Test.CharArray.Conversion.testTree
    , Test.CharArray.Copy.testTree
    ]