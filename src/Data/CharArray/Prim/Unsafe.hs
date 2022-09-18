{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.CharArray.Prim.Unsafe
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
module Data.CharArray.Prim.Unsafe
  ( -- * Thaw
    unsafeThaw#,

    -- * Index
    unsafeIndex#,
  )
where

import Data.Int.Prim (Int#)
import Data.Coerce (coerce)

import GHC.Exts (Char#, State#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Data.CharArray.Prim.Core (CharArray# (CharArray#))
import Data.MutCharArray.Prim.Core (MutCharArray#)

-- Thaw ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeThaw# :: CharArray# -> State# s -> (# State# s, MutCharArray# s #)
unsafeThaw# = GHC.unsafeCoerce#

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
unsafeIndex# :: CharArray# -> Int# -> Char#
unsafeIndex# = coerce GHC.indexWideCharArray#
