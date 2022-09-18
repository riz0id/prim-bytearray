{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.ByteArray.Prim.Unsafe
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
module Data.MutByteArray.Prim.Unsafe
  ( -- * Freeze
    unsafeFreeze#,

    -- * Index
    unsafeIndex#,

    -- * Write
    unsafeWrite#,
  )
where

import Data.Int.Prim (Int#)

import GHC.Exts (ByteArray#, MutableByteArray#, Word8#, State#)
import GHC.Exts qualified as GHC

-- Freeze ----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
unsafeFreeze# :: MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
unsafeFreeze# = GHC.unsafeFreezeByteArray#

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word8# #)
unsafeIndex# = GHC.readWord8Array#

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeWrite# :: MutableByteArray# s -> Int# -> Word8# -> State# s -> State# s
unsafeWrite# = GHC.writeWord8Array#

