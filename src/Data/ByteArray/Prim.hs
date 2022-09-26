{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.ByteArray.Prim
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
module Data.ByteArray.Prim
  ( ByteArray#,
    pack#,
    unpack#,

    -- * Construction

    -- new#,
    -- pin#,
    -- aligned#,

    -- * Comparison
    equiv#,

    -- * Copy
    slice#,
    clone#,

    -- * Thaw
    thaw#,

    -- * Query
    address#,
    size#,

    -- * Predicates
    isEmpty#,
    isPinned#,

    -- * Index
    index#,

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

import Control.Exception (toException)

import Data.Bool.Prim (Bool# (F#, T#))
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int
import Data.Ord.Prim (Eq# (..), Ord# (..), Ordering# (EQ#))
import Data.Word (Word8)

import GHC.Exts (Addr#, ByteArray#, Int (I#), State#, TYPE, Word8#)
import GHC.Exts qualified as GHC
import GHC.Word (Word8 (W8#))

--------------------------------------------------------------------------------

import Control.Exception.IndexError (IndexError (IndexError))

import Data.ByteArray.Prim.Unsafe (unsafeIndex#, unsafeThaw#)
import Data.MutByteArray.Prim (MutByteArray#)
import Data.MutByteArray.Prim qualified as MutByteArray

--------------------------------------------------------------------------------

raiseIndexError# :: forall r (a :: TYPE r). ByteArray# -> Int# -> a
raiseIndexError# xs# i# =
  let exn :: IndexError
      exn = IndexError 0 (toInteger (I# (size# xs#))) (toInteger (I# i#))
   in GHC.raise# (toException exn)

-- Eq# - ByteArray# ------------------------------------------------------------

-- | @since 1.0.0
instance Eq# ByteArray# where
  xs# ==# ys# =
    case equiv# xs# ys# of
      T# -> T#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) ==# 0#
        F# -> F#
  {-# INLINE (==#) #-}

  xs# /=# ys# =
    case equiv# xs# ys# of
      T# -> F#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) /=# 0#
        F# -> T#
  {-# INLINE (/=#) #-}

-- Ord# - ByteArray# -----------------------------------------------------------

-- | @since 1.0.0
instance Ord# ByteArray# where
  compare# xs# ys# =
    case equiv# xs# ys# of
      T# -> EQ#
      F# -> case size# xs# ==# size# ys# of
        T# -> compare# (GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#)) 0#
        F# -> compare# (size# xs#) (size# ys#)
  {-# INLINE compare# #-}

  xs# ># ys# =
    case equiv# xs# ys# of
      T# -> F#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) ># 0#
        F# -> size# xs# ># size# ys#
  {-# INLINE (>#) #-}

  xs# >=# ys# =
    case equiv# xs# ys# of
      T# -> F#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) >=# 0#
        F# -> size# xs# >=# size# ys#
  {-# INLINE (>=#) #-}

  xs# <# ys# =
    case equiv# xs# ys# of
      T# -> F#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) <# 0#
        F# -> size# xs# <# size# ys#
  {-# INLINE (<#) #-}

  xs# <=# ys# =
    case equiv# xs# ys# of
      T# -> F#
      F# -> case size# xs# ==# size# ys# of
        T# -> GHC.compareByteArrays# xs# 0# ys# 0# (size# xs#) <=# 0#
        F# -> size# xs# <=# size# ys#
  {-# INLINE (<=#) #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pack# :: [Word8] -> ByteArray#
pack# xs =
  GHC.runRW# \st0# ->
    let !(# st1#, dst# #) = MutByteArray.pack# xs st0#
        !(# _, bxs# #) = MutByteArray.unsafeFreeze# dst# st1#
     in bxs#
{-# INLINE pack# #-}

-- | TODO
--
-- @since 1.0.0
unpack# :: ByteArray# -> [Word8]
unpack# = foldr'# (\x# xs -> W8# x# : xs) []
{-# INLINE unpack# #-}

-- Comparison ------------------------------------------------------------------

-- | Compares the 'address#' of two 'ByteArray#' values.
--
-- @since 1.0.0
equiv# :: ByteArray# -> ByteArray# -> Bool#
equiv# xs# ys# = Bool.unsafeFromInt# (GHC.eqAddr# (address# xs#) (address# ys#))

-- Copy ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
slice# :: ByteArray# -> Int# -> Int# -> State# s -> (# State# s, ByteArray# #)
slice# src# i0# i1# st0# =
  let !len# = Int.subInt# i1# i0#
      !(# st1#, dst# #) = MutByteArray.new# len# st0#
      !st2# = GHC.copyByteArray# src# i0# dst# 0# len# st1#
   in MutByteArray.unsafeFreeze# dst# st2#

-- | TODO
--
-- @since 1.0.0
clone# :: ByteArray# -> State# s -> (# State# s, ByteArray# #)
clone# src# st0# =
  let !len# = size# src#
      !(# st1#, dst# #) = MutByteArray.new# len# st0#
      !st2# = GHC.copyByteArray# src# 0# dst# 0# len# st1#
   in MutByteArray.unsafeFreeze# dst# st2#

-- Thaw ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
thaw# :: ByteArray# -> State# s -> (# State# s, MutByteArray# s #)
thaw# src# st0# =
  let !(# st1#, dst# #) = clone# src# st0#
   in unsafeThaw# dst# st1#

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
address# :: ByteArray# -> Addr#
address# = GHC.byteArrayContents#

-- | TODO
--
-- @since 1.0.0
size# :: ByteArray# -> Int#
size# = GHC.sizeofByteArray#

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isEmpty# :: ByteArray# -> Bool#
isEmpty# xs# = Int.eqInt# 0# (size# xs#)

-- | TODO
--
-- @since 1.0.0
isPinned# :: ByteArray# -> Bool#
isPinned# xs# = Bool.unsafeFromInt# (GHC.isByteArrayPinned# xs#)

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index# :: ByteArray# -> Int# -> Word8#
index# xs# i# =
  let lower# = Int.leInt# 0# i#
      upper# = Int.ltInt# i# (size# xs#)
   in case Bool.and# lower# upper# of
        T# -> unsafeIndex# xs# i#
        F# -> raiseIndexError# xs# i#

-- Write -----------------------------------------------------------------------

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl# :: forall a. (a -> Word8# -> a) -> a -> ByteArray# -> a
foldl# con = ifoldl# \_ -> con
{-# INLINE foldl# #-}

-- | TODO
--
-- @since 1.0.0
foldr# :: forall a. (Word8# -> a -> a) -> a -> ByteArray# -> a
foldr# con = ifoldr# \_ -> con
{-# INLINE foldr# #-}

-- | TODO
--
-- @since 1.0.0
foldMap# :: Monoid a => (Word8# -> a) -> ByteArray# -> a
foldMap# f = foldr# (\x# xs -> f x# <> xs) mempty
{-# INLINE foldMap# #-}

-- Folds - Strict --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl'# :: forall a. (a -> Word8# -> a) -> a -> ByteArray# -> a
foldl'# con = ifoldl'# \_ -> con
{-# INLINE foldl'# #-}

-- | TODO
--
-- @since 1.0.0
foldr'# :: forall a. (Word8# -> a -> a) -> a -> ByteArray# -> a
foldr'# con = ifoldr'# \_ -> con
{-# INLINE foldr'# #-}

-- | TODO
--
-- @since 1.0.0
foldMap'# :: Monoid a => (Word8# -> a) -> ByteArray# -> a
foldMap'# f = foldl'# (\xs x# -> xs <> f x#) mempty
{-# INLINE foldMap'# #-}

-- Indexed Folds ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl# :: forall a. (Int# -> a -> Word8# -> a) -> a -> ByteArray# -> a
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
ifoldr# :: forall a. (Int# -> Word8# -> a -> a) -> a -> ByteArray# -> a
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
ifoldMap# :: Monoid a => (Int# -> Word8# -> a) -> ByteArray# -> a
ifoldMap# f = ifoldr# (\i# x# xs -> f i# x# <> xs) mempty
{-# INLINE ifoldMap# #-}

-- Indexed Folds - Strict ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl'# :: forall a. (Int# -> a -> Word8# -> a) -> a -> ByteArray# -> a
ifoldl'# con nil s# = ifoldr# con' id s# nil
  where
    con' :: Int# -> Word8# -> (a -> a) -> (a -> a)
    con' i# x# k = GHC.oneShot \xs -> xs `seq` k (con i# xs x#)
{-# INLINE ifoldl'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr'# :: forall a. (Int# -> Word8# -> a -> a) -> a -> ByteArray# -> a
ifoldr'# con nil s# = ifoldl# con' id s# nil
  where
    con' :: Int# -> (a -> a) -> Word8# -> (a -> a)
    con' i# k x# = GHC.oneShot \xs -> xs `seq` k (con i# x# xs)
{-# INLINE ifoldr'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap'# :: Monoid a => (Int# -> Word8# -> a) -> ByteArray# -> a
ifoldMap'# f = ifoldl'# (\i# xs x# -> xs <> f i# x#) mempty
{-# INLINE ifoldMap'# #-}
