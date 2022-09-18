{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.CharArray.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.CharArray.Prim
  ( CharArray# (CharArray#),
    pack#,
    unpack#,

    -- * Construction

    -- new#,
    -- pin#,
    -- aligned#,

    -- * Comparison
    eq#,
    same#,
    compare#,

    -- * Copy
    slice#,
    clone#,

    -- * Thaw
    thaw#,

    -- * Query
    address#,
    size#,
    null#,
    pinned#,

    -- * Index
    index#,

    -- ** Unsafe
    unsafeIndex#,

    -- * Write

    -- * Folds
    foldl#,
    foldr#,
    foldMap#,

    -- ** Strict
    foldl'#,
    foldr'#,
    foldMap'#,

    -- * Indexed Folds
    ifoldl#,
    ifoldr#,
    ifoldMap#,

    -- ** Strict
    ifoldl'#,
    ifoldr'#,
    ifoldMap'#,
  )
where

import Data.Bool.Prim (Bool# (False#, True#))
import Data.Bool.Prim qualified as Bool
import Data.Coerce (coerce)
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int

import GHC.Exts (Char#, State#, Addr#, Char (C#), RealWorld, Int (I#))
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Data.CharArray.Prim.Core (CharArray# (CharArray#))
import Data.CharArray.Prim.Unsafe (unsafeIndex#, unsafeThaw#)

import Data.ByteArray.Prim qualified as ByteArray

import Data.MutCharArray.Prim.Core (MutCharArray#)
import qualified Data.MutCharArray.Prim as MutCharArray

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pack# :: [Char] -> CharArray# 
pack# xs = GHC.runRW# \st0# -> 
  let !(I# len#) = length xs
      !(# st1#, dst# #) = MutCharArray.new# len# st0#

      loop# :: Int# -> [Char] -> State# RealWorld -> State# RealWorld
      loop# _ [] st# = st#
      loop# i# (C# x# : xs') st# = 
        let !st'# = MutCharArray.write# dst# i# x# st# 
         in loop# (Int.addInt# 1# i#) xs' st'#
   in case MutCharArray.unsafeFreeze# dst# (loop# 0# xs st1#) of 
        (# _, xs# #) -> xs# 
{-# INLINE pack# #-}

-- | TODO
--
-- @since 1.0.0
unpack# :: CharArray# -> [Char] 
unpack# = foldr'# (\x# xs -> C# x# : xs) []
{-# INLINE unpack# #-}

-- Comparison ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
eq# :: CharArray# -> CharArray# -> Bool#
eq# xs# ys# = Int.eqInt# 0# (compare# xs# ys#)

-- | TODO
--
-- @since 1.0.0
same# :: CharArray# -> CharArray# -> Bool#
same# xs# ys# = Bool.unsafeFromInt# (GHC.eqAddr# (address# xs#) (address# ys#))

-- | TODO
--
-- @since 1.0.0
compare# :: CharArray# -> CharArray# -> Int# 
compare# = coerce ByteArray.compare# 

-- Copy ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
slice# :: CharArray# -> Int# -> Int# -> State# s -> (# State# s, CharArray# #)
slice# src# i0# i1# =
  let !i0'# = Int.mulInt# 4# i0#
      !i1'# = Int.mulInt# 4# i1#
   in coerce ByteArray.slice# src# i0'# i1'#

-- | TODO
--
-- @since 1.0.0
clone# :: CharArray# -> State# s -> (# State# s, CharArray# #)
clone# = coerce ByteArray.clone#

-- Thaw ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
thaw# :: CharArray# -> State# s -> (# State# s, MutCharArray# s #)
thaw# src# st0# =
  let !(# st1#, dst# #) = clone# src# st0#
   in unsafeThaw# dst# st1#

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
address# :: CharArray# -> Addr#
address# = coerce ByteArray.address# 

-- | TODO
--
-- @since 1.0.0
size# :: CharArray# -> Int#
size# xs# = GHC.quotInt# (coerce GHC.sizeofByteArray# xs#) 4#

-- | TODO
--
-- @since 1.0.0
null# :: CharArray# -> Bool#
null# xs# = Int.eqInt# 0# (coerce GHC.sizeofByteArray# xs#)

-- | TODO
--
-- @since 1.0.0
pinned# :: CharArray# -> Bool#
pinned# xs# = Bool.unsafeFromInt# (coerce GHC.isByteArrayPinned# xs#)

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index# :: CharArray# -> Int# -> Char#
index# xs# i# =
  let lower# = Int.leInt# 0# i#
      upper# = Int.ltInt# i# (size# xs#)
   in case Bool.and# lower# upper# of
        True# -> unsafeIndex# xs# i#
        False# -> '\NUL'#

-- Write -----------------------------------------------------------------------

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl# :: (a -> Char# -> a) -> a -> CharArray# -> a
foldl# con = ifoldl# \_ -> con
{-# INLINE foldl# #-}

-- | TODO
--
-- @since 1.0.0
foldr# :: (Char# -> a -> a) -> a -> CharArray# -> a
foldr# con = ifoldr# \_ -> con
{-# INLINE foldr# #-}

-- | TODO
--
-- @since 1.0.0
foldMap# :: Monoid a => (Char# -> a) -> CharArray# -> a
foldMap# f = foldr# (\x# xs -> f x# <> xs) mempty
{-# INLINE foldMap# #-}

-- Folds - Strict --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl'# :: (a -> Char# -> a) -> a -> CharArray# -> a
foldl'# con = ifoldl'# \_ -> con
{-# INLINE foldl'# #-}

-- | TODO
--
-- @since 1.0.0
foldr'# :: (Char# -> a -> a) -> a -> CharArray# -> a
foldr'# con = ifoldr'# \_ -> con
{-# INLINE foldr'# #-}

-- | TODO
--
-- @since 1.0.0
foldMap'# :: Monoid a => (Char# -> a) -> CharArray# -> a
foldMap'# f = foldl'# (\xs x# -> xs <> f x#) mempty
{-# INLINE foldMap'# #-}

-- Indexed Folds ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl# :: forall a. (Int# -> a -> Char# -> a) -> a -> CharArray# -> a
ifoldl# con nil xs# = loop# (size# xs# GHC.-# 1#)
  where
    loop# :: Int# -> a
    loop# i# =
      case 0# GHC.<=# i# of
        1# -> con i# (loop# (i# GHC.-# 1#)) (unsafeIndex# xs# i#)
        _ -> nil
{-# INLINE ifoldl# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr# :: forall a. (Int# -> Char# -> a -> a) -> a -> CharArray# -> a
ifoldr# con nil xs# = loop# 0#
  where
    loop# :: Int# -> a
    loop# i# =
      case i# GHC.<# size# xs# of
        1# -> con i# (unsafeIndex# xs# i#) (loop# (1# GHC.+# i#))
        _ -> nil
{-# INLINE ifoldr# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap# :: Monoid a => (Int# -> Char# -> a) -> CharArray# -> a
ifoldMap# f = ifoldr# (\i# x# xs -> f i# x# <> xs) mempty
{-# INLINE ifoldMap# #-}

-- Indexed Folds - Strict ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl'# :: forall a. (Int# -> a -> Char# -> a) -> a -> CharArray# -> a
ifoldl'# con nil s# = ifoldr# con' id s# nil
  where
    con' :: Int# -> Char# -> (a -> a) -> (a -> a)
    con' i# x# k = GHC.oneShot \xs -> xs `seq` k (con i# xs x#)
{-# INLINE ifoldl'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr'# :: forall a. (Int# -> Char# -> a -> a) -> a -> CharArray# -> a
ifoldr'# con nil s# = ifoldl# con' id s# nil
  where
    con' :: Int# -> (a -> a) -> Char# -> (a -> a)
    con' i# k x# = GHC.oneShot \xs -> xs `seq` k (con i# x# xs)
{-# INLINE ifoldr'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap'# :: Monoid a => (Int# -> Char# -> a) -> CharArray# -> a
ifoldMap'# f = ifoldl'# (\i# xs x# -> xs <> f i# x#) mempty
{-# INLINE ifoldMap'# #-}