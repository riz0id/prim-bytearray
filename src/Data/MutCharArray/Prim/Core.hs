{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutCharArray.Prim.Core
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
module Data.MutCharArray.Prim.Core
  ( MutCharArray# (MutCharArray#),
  )
where

import GHC.Exts (MutableByteArray#)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype MutCharArray# s = MutCharArray# (MutableByteArray# s)