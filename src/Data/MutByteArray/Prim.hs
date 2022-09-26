{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutByteArray.Prim
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
module Data.MutByteArray.Prim
  ( MutByteArray#,
    MutableByteArray#,

    -- * Construction
    new#,
    pack#,
    -- pin#,
    -- aligned#,

    -- * Copy
    copy#,
    slice#,
    clone#,

    -- * Freeze
    freeze#,
    unsafeFreeze#,

    -- * Query
    address#,
    size#,
    null#,
    pinned#,

    -- * Index
    index#,

    -- * Write
    write#,

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

import Control.Exception.IndexError (IndexError (IndexError))

import Data.Bool.Prim (Bool# (False#, True#))
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int
import Data.Word (Word8)

import GHC.Exts (Addr#, ByteArray#, Int (I#), MutableByteArray#, State#, TYPE, Word8#)
import GHC.Exts qualified as GHC
import GHC.Word (Word8 (W8#))

--------------------------------------------------------------------------------

import Control.Exception (toException)

import Data.MutByteArray.Prim.Unsafe
  ( unsafeFreeze#,
    unsafeIndex#,
    unsafeWrite#,
  )

raiseIndexError# :: forall r (a :: TYPE r) s. MutByteArray# s -> Int# -> a
raiseIndexError# xs# i# =
  let exn :: IndexError
      exn = IndexError 0 (toInteger (I# (GHC.sizeofMutableByteArray# xs#))) (toInteger (I# i#))
   in GHC.raise# (toException exn)

-- | TODO
--
-- @since 1.0.0
type MutByteArray# = MutableByteArray#

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
new# :: Int# -> State# s -> (# State# s, MutByteArray# s #)
new# len# = GHC.newPinnedByteArray# len# -- TODO: unpack as safe/unsafe

-- | TODO
--
-- @since 1.0.0
pack# :: forall s. [Word8] -> State# s -> (# State# s, MutByteArray# s #)
pack# xs st0# =
  let !(I# len#) = length xs
      !(# st1#, dst# #) = new# len# st0#

      loop# :: Int# -> [Word8] -> State# s -> State# s
      loop# _ [] st# = st#
      loop# i# (W8# x# : xs') st# =
        loop# (Int.addInt# 1# i#) xs' (write# dst# i# x# st#)
   in (# loop# 0# xs st1#, dst# #)
{-# INLINE pack# #-}

-- Copy ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
copy# :: MutByteArray# s -> Int# -> MutByteArray# s -> Int# -> Int# -> State# s -> State# s
copy# = GHC.copyMutableByteArray# -- TODO: unpack as safe/unsafe

-- | TODO
--
-- @since 1.0.0
slice# :: MutByteArray# s -> Int# -> Int# -> State# s -> (# State# s, MutByteArray# s #)
slice# src# i0# i1# st0# =
  -- TODO: unpack as safe/unsafe
  let !(# st1#, dst# #) = new# (Int.subInt# i1# i0#) st0#
   in (# copy# dst# 0# src# i0# (Int.subInt# i1# i0#) st1#, dst# #)

-- | TODO
--
-- @since 1.0.0
clone# :: MutByteArray# s -> State# s -> (# State# s, MutByteArray# s #)
clone# src# st0# =
  let !(# st1#, len# #) = size# src# st0#
      !(# st2#, dst# #) = new# len# st1#
   in (# copy# src# 0# dst# 0# len# st2#, dst# #)

-- Copy - Unsafe ---------------------------------------------------------------

-- Freeze ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
freeze# :: MutByteArray# s -> State# s -> (# State# s, ByteArray# #)
freeze# src# st0# =
  let !(# st1#, dst# #) = clone# src# st0#
   in unsafeFreeze# dst# st1#

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
address# :: MutByteArray# s -> Addr#
address# = GHC.mutableByteArrayContents#

-- | TODO
--
-- @since 1.0.0
size# :: MutByteArray# s -> State# s -> (# State# s, Int# #)
size# = GHC.getSizeofMutableByteArray#

-- | TODO
--
-- @since 1.0.0
null# :: MutByteArray# s -> State# s -> (# State# s, Bool# #)
null# xs# st0# =
  let !(# st1#, len# #) = size# xs# st0#
   in (# st1#, Int.eqInt# 0# len# #)

-- | TODO
--
-- @since 1.0.0
pinned# :: MutByteArray# s -> Bool#
pinned# xs# = Bool.unsafeFromInt# (GHC.isMutableByteArrayPinned# xs#)

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index# :: MutByteArray# s -> Int# -> State# s -> (# State# s, Word8# #)
index# xs# i# st0# =
  let !(# st1#, len# #) = size# xs# st0#
      lower# = Int.gtInt# 0# i#
      upper# = Int.ltInt# i# len#
   in case Bool.and# lower# upper# of
        True# -> unsafeIndex# xs# i# st1#
        False# -> raiseIndexError# xs# i#

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
write# :: MutByteArray# s -> Int# -> Word8# -> State# s -> State# s
write# xs# i# x# st0# =
  let !(# st1#, len# #) = size# xs# st0#
      lower# = Int.leInt# 0# i#
      upper# = Int.ltInt# i# len#
   in case Bool.and# lower# upper# of
        True# -> unsafeWrite# xs# i# x# st1#
        False# -> st1#

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl# :: (a -> Word8# -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
foldl# con = ifoldl# \_ -> con
{-# INLINE foldl# #-}

-- | TODO
--
-- @since 1.0.0
foldr# :: (Word8# -> a -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
foldr# con = ifoldr# \_ -> con
{-# INLINE foldr# #-}

-- | TODO
--
-- @since 1.0.0
foldMap# :: Monoid a => (Word8# -> a) -> MutByteArray# s -> State# s -> (# State# s, a #)
foldMap# f = foldr# (\x# xs -> f x# <> xs) mempty
{-# INLINE foldMap# #-}

-- Folds - Strict --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl'# :: (a -> Word8# -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
foldl'# con = ifoldl'# \_ -> con
{-# INLINE foldl'# #-}

-- | TODO
--
-- @since 1.0.0
foldr'# :: (Word8# -> a -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
foldr'# con = ifoldr'# \_ -> con
{-# INLINE foldr'# #-}

-- | TODO
--
-- @since 1.0.0
foldMap'# :: Monoid a => (Word8# -> a) -> MutByteArray# s -> State# s -> (# State# s, a #)
foldMap'# f = foldl'# (\xs x# -> xs <> f x#) mempty
{-# INLINE foldMap'# #-}

-- Indexed Folds ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl# :: forall s a. (Int# -> a -> Word8# -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldl# con nil xs# st# =
  let !(# st'#, len# #) = size# xs# st#

      loop# :: Int# -> State# s -> (# State# s, a #)
      loop# i# st0# =
        case Int.ltInt# i# len# of
          False# -> (# st0#, nil #)
          True# ->
            let !(# st1#, a# #) = loop# (1# GHC.+# i#) st0#
                !(# st2#, b# #) = unsafeIndex# xs# i# st1#
             in (# st2#, con i# a# b# #)
   in loop# 0# st'#
{-# INLINE ifoldl# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr# :: forall s a. (Int# -> Word8# -> a -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldr# con nil xs# st# =
  let !(# st'#, len# #) = size# xs# st#

      loop# :: Int# -> State# s -> (# State# s, a #)
      loop# i# st0# =
        case Int.ltInt# i# len# of
          False# -> (# st0#, nil #)
          True# ->
            let !(# st1#, a# #) = unsafeIndex# xs# i# st0#
                !(# st2#, b# #) = loop# (1# GHC.+# i#) st1#
             in (# st2#, con i# a# b# #)
   in loop# 0# st'#
{-# INLINE ifoldr# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap# :: Monoid a => (Int# -> Word8# -> a) -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldMap# f = ifoldr# (\i# x# xs -> f i# x# <> xs) mempty
{-# INLINE ifoldMap# #-}

-- Indexed Folds - Strict ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl'# :: (Int# -> a -> Word8# -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldl'# con nil xs# st0# =
  let !(# st1#, k #) = ifoldr# con' id xs# st0#
   in (# st1#, k nil #)
  where
    con' i# x# k = GHC.oneShot \xs -> xs `seq` k (con i# xs x#)
{-# INLINE ifoldl'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr'# :: (Int# -> Word8# -> a -> a) -> a -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldr'# con nil xs# st0# =
  let !(# st1#, k #) = ifoldl# con' id xs# st0#
   in (# st1#, k nil #)
  where
    con' i# k x# = GHC.oneShot \xs -> xs `seq` k (con i# x# xs)
{-# INLINE ifoldr'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap'# :: Monoid a => (Int# -> Word8# -> a) -> MutByteArray# s -> State# s -> (# State# s, a #)
ifoldMap'# f = ifoldl'# (\i# xs x# -> xs <> f i# x#) mempty
{-# INLINE ifoldMap'# #-}
