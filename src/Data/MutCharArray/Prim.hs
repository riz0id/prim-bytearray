{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutCharArray.Prim
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
module Data.MutCharArray.Prim
  ( MutCharArray# (MutCharArray#),

    -- * Construction
    new#,
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

import Data.Bool.Prim (Bool# (False#, True#))
import Data.Bool.Prim qualified as Bool
import Data.Coerce (coerce)
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int

import GHC.Exts (ByteArray#, Char#, State#, Addr#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Data.MutByteArray.Prim qualified as MutByteArray

import Data.MutCharArray.Prim.Core (MutCharArray# (MutCharArray#))
import Data.MutCharArray.Prim.Unsafe
  ( unsafeFreeze#,
    unsafeIndex#,
    unsafeWrite#,
  )

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
new# :: Int# -> State# s -> (# State# s, MutCharArray# s #)
new# len# = coerce GHC.newByteArray# (Int.mulInt# 4# len#) -- TODO: unpack as safe/unsafe

-- Copy ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
copy# :: MutCharArray# s -> Int# -> MutCharArray# s -> Int# -> Int# -> State# s -> State# s
copy# src# i0# dst# i1# len# =
  -- TODO: unpack as safe/unsafe
  let !i0'# = Int.mulInt# 4# i0#
      !i1'# = Int.mulInt# 4# i1#
      !len'# = Int.mulInt# 4# len#
   in coerce MutByteArray.copy# src# i0'# dst# i1'# len'#

-- | TODO
--
-- @since 1.0.0
slice# :: MutCharArray# s -> Int# -> Int# -> State# s -> (# State# s, MutCharArray# s #)
slice# src# i0# i1# =
  -- TODO: unpack as safe/unsafe
  let !i0'# = Int.mulInt# 4# i0#
      !i1'# = Int.mulInt# 4# i1#
   in coerce MutByteArray.slice# src# i0'# i1'#

-- | TODO
--
-- @since 1.0.0
clone# :: MutCharArray# s -> State# s -> (# State# s, MutCharArray# s #)
clone# = coerce MutByteArray.clone#

-- Copy - Unsafe ---------------------------------------------------------------

-- Freeze ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
freeze# :: MutCharArray# s -> State# s -> (# State# s, ByteArray# #)
freeze# = coerce MutByteArray.freeze#

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
address# :: MutCharArray# s -> Addr#
address# = coerce GHC.mutableByteArrayContents# 

-- | TODO
--
-- @since 1.0.0
size# :: MutCharArray# s -> State# s -> (# State# s, Int# #)
size# xs# st0# =
  let !(# st1#, len# #) = coerce MutByteArray.size# xs# st0#
   in (# st1#, GHC.quotInt# len# 4# #)

-- | TODO
--
-- @since 1.0.0
null# :: MutCharArray# s -> State# s -> (# State# s, Bool# #)
null# = coerce MutByteArray.null#

-- | TODO
--
-- @since 1.0.0
pinned# :: MutCharArray# s -> Bool#
pinned# = coerce MutByteArray.pinned#

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index# :: MutCharArray# s -> Int# -> State# s -> (# State# s, Char# #)
index# xs# i# st0# =
  let !(# st1#, len# #) = size# xs# st0#
      lower# = Int.gtInt# 0# i#
      upper# = Int.ltInt# i# len#
   in case Bool.and# lower# upper# of
        True# -> unsafeIndex# xs# i# st1#
        False# -> (# st1#, '\NUL'# #)

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
write# :: MutCharArray# s -> Int# -> Char# -> State# s -> State# s
write# xs# i# x# st0# =
  let !(# st1#, len# #) = size# xs# st0#
      lower# = Int.gtInt# 0# i#
      upper# = Int.ltInt# i# len#
   in case Bool.and# lower# upper# of
        True# -> unsafeWrite# xs# i# x# st1#
        False# -> st1#

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl# :: (a -> Char# -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
foldl# con = ifoldl# \_ -> con
{-# INLINE foldl# #-}

-- | TODO
--
-- @since 1.0.0
foldr# :: (Char# -> a -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
foldr# con = ifoldr# \_ -> con
{-# INLINE foldr# #-}

-- | TODO
--
-- @since 1.0.0
foldMap# :: Monoid a => (Char# -> a) -> MutCharArray# s -> State# s -> (# State# s, a #)
foldMap# f = foldr# (\x# xs -> f x# <> xs) mempty
{-# INLINE foldMap# #-}

-- Folds - Strict --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foldl'# :: (a -> Char# -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
foldl'# con = ifoldl'# \_ -> con
{-# INLINE foldl'# #-}

-- | TODO
--
-- @since 1.0.0
foldr'# :: (Char# -> a -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
foldr'# con = ifoldr'# \_ -> con
{-# INLINE foldr'# #-}

-- | TODO
--
-- @since 1.0.0
foldMap'# :: Monoid a => (Char# -> a) -> MutCharArray# s -> State# s -> (# State# s, a #)
foldMap'# f = foldl'# (\xs x# -> xs <> f x#) mempty
{-# INLINE foldMap'# #-}

-- Indexed Folds ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl# :: forall s a. (Int# -> a -> Char# -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
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
ifoldr# :: forall s a. (Int# -> Char# -> a -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
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
ifoldMap# :: Monoid a => (Int# -> Char# -> a) -> MutCharArray# s -> State# s -> (# State# s, a #)
ifoldMap# f = ifoldr# (\i# x# xs -> f i# x# <> xs) mempty
{-# INLINE ifoldMap# #-}

-- Indexed Folds - Strict ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ifoldl'# :: (Int# -> a -> Char# -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
ifoldl'# con nil xs# st0# =
  let !(# st1#, k #) = ifoldr# con' id xs# st0#
   in (# st1#, k nil #)
  where
    con' i# x# k = GHC.oneShot \xs -> xs `seq` k (con i# xs x#)
{-# INLINE ifoldl'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldr'# :: (Int# -> Char# -> a -> a) -> a -> MutCharArray# s -> State# s -> (# State# s, a #)
ifoldr'# con nil xs# st0# =
  let !(# st1#, k #) = ifoldl# con' id xs# st0#
   in (# st1#, k nil #)
  where
    con' i# k x# = GHC.oneShot \xs -> xs `seq` k (con i# x# xs)
{-# INLINE ifoldr'# #-}

-- | TODO
--
-- @since 1.0.0
ifoldMap'# :: Monoid a => (Int# -> Char# -> a) -> MutCharArray# s -> State# s -> (# State# s, a #)
ifoldMap'# f = ifoldl'# (\i# xs x# -> xs <> f i# x#) mempty
{-# INLINE ifoldMap'# #-}
