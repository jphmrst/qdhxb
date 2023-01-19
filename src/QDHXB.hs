
-- | Quick-and-Dirty Haskell/XSD bindings.
--
-- This module simply re-exports bindings made elsewhere: the
-- top-level QDHXB calls, configuration options, and definitions used
-- in the code to which the macros expand.
module QDHXB (
  -- * Invoking QDHXB
  qdhxb, qdhxb',

  -- * Options
  --
  -- | These options for the `qdhxb` function are re-exported from the
  -- @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType,

  -- * Other re-exported utilities
  --
  -- | These values are used in the code generated by `qdhxb` and
  -- `qdhxb'`, and therefore are re-exported here for convenience.
  Content,
  ZeroOneMany(Zero, One, Many),
  pullContentFrom, zomToList, loadElement
  ) where

import Text.XML.Light.Types (Content)
import QDHXB.Internal.Utils.XMLLight (
  ZeroOneMany(Zero, One, Many), pullContentFrom, zomToList, loadElement
  )
import QDHXB.API
import QDHXB.Options
