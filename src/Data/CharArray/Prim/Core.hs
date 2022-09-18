{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.CharArray.Prim.Core
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
module Data.CharArray.Prim.Core
  ( CharArray# (CharArray#),
  )
where

import GHC.Exts (ByteArray#)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype CharArray# = CharArray# ByteArray#