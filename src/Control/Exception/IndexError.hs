{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Exception.IndexError
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
module Control.Exception.IndexError
  ( -- * TODO
    IndexError (IndexError, lower, upper, index)
  )
where

import Control.Exception (Exception, displayException)

import Data.Data (Data)
import qualified Text.Printf as Text

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data IndexError = IndexError
  { lower :: Integer
  , upper :: Integer 
  , index :: Integer
  }
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show IndexError where 
  show (IndexError low high i) = 
    Text.printf "IndexError: index %v out of bounds [%v, %v)" i low high
  {-# INLINE show #-}

-- | @since 1.0.0
instance Exception IndexError where 
  displayException = show 
  {-# INLINE displayException #-}
