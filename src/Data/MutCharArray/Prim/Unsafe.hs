{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutCharArray.Prim.Unsafe
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
module Data.MutCharArray.Prim.Unsafe
  ( -- * Freeze
    unsafeFreeze#,

    -- * Index
    unsafeIndex#,

    -- * Write
    unsafeWrite#,
  )
where

import Data.Coerce (coerce)
import Data.Int.Prim (Int#)

import GHC.Exts (State#, Char#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Data.CharArray.Prim.Core (CharArray# (CharArray#))
import Data.MutCharArray.Prim.Core (MutCharArray# (MutCharArray#))

-- Freeze ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeFreeze# :: MutCharArray# s -> State# s -> (# State# s, CharArray# #)
unsafeFreeze# = coerce GHC.unsafeFreezeByteArray#

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex# :: MutCharArray# s -> Int# -> State# s -> (# State# s, Char# #)
unsafeIndex# = coerce GHC.readWideCharArray# 

-- Write -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeWrite# :: MutCharArray# s -> Int# -> Char# -> State# s -> State# s
unsafeWrite# = coerce GHC.writeWideCharArray#
