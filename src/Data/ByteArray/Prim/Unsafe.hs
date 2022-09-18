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
module Data.ByteArray.Prim.Unsafe
  ( -- * Thaw
    unsafeThaw#,

    -- * Index
    unsafeIndex#,
  )
where

import Data.Int.Prim (Int#)

import GHC.Exts (ByteArray#, Word8#, State#, MutableByteArray#)
import GHC.Exts qualified as GHC

-- Thaw ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeThaw# :: ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
unsafeThaw# = GHC.unsafeCoerce#

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex# :: ByteArray# -> Int# -> Word8#
unsafeIndex# = GHC.indexWord8Array#