
module Test.Gen 
  ( list'char,
    list'word8,
  ) 
where 

import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Word (Word8)

--------------------------------------------------------------------------------

list'char :: Gen [Char]
list'char = Gen.sized \size -> 
  let range :: Range Int 
      range = Range.constant 0 (fromIntegral size)
   in Gen.list range Gen.latin1

list'word8 :: Gen [Word8]
list'word8 = Gen.sized \size -> 
  let range :: Range Int 
      range = Range.constant 0 (fromIntegral size)
   in Gen.list range (Gen.word8 Range.constantBounded)
