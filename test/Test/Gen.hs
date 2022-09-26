
module Test.Gen 
  ( -- * List Generators 
    sized'list,
    list'char,
    list'word8,

    -- * ByteArray Generators
    bytearray,
  ) 
where 

import Data.Word (Word8)
import Data.Primitive (ByteArray)

import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import GHC.Exts (fromList)

-- List Generators -------------------------------------------------------------

sized'list :: Gen a -> Gen [a]
sized'list gen = 
  Gen.sized \size -> 
    let range :: Range Int 
        range = Range.constant 0 (fromIntegral size)
     in Gen.list range gen

list'char :: Gen [Char]
list'char = sized'list Gen.latin1

list'word8 :: Gen [Word8]
list'word8 = sized'list (Gen.word8 Range.constantBounded)

-- ByteArray Generators --------------------------------------------------------

bytearray :: Gen ByteArray
bytearray = fmap fromList list'word8