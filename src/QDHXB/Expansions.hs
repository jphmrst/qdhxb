
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Re-exports of values used in generated TH code.
module QDHXB.Expansions (
  readMaybe, mapM,
  simpleTypeDecoder,
  Content,
  ZeroOneMany(Zero, One, Many), zomToList, zommapM,
  Except, throwError, catchError, runExcept, runExceptT,
  spaceSep, pullAttrFrom,
  Day, TimeOfDay, DiffTime, ZonedTime,
  __loadElement, __loadContent, pullContentFrom, pullCRefContent,
  HXBErr(..), HXBExcept,
  bpp, QName, contextfreeStringToQName
  )
where

import Control.Monad.Except (
  Except, throwError, runExcept, runExceptT, catchError)
import Text.Read (readMaybe)
import Text.XML.Light.Types (Content, QName)
import Data.Time.Calendar.OrdinalDate (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Clock (DiffTime)
import QDHXB.Errs
import QDHXB.Utils.ZeroOneMany
import QDHXB.Utils.XMLLight (
  __loadElement, __loadContent,
  pullContentFrom, pullAttrFrom, pullCRefContent, contextfreeStringToQName)
import QDHXB.Utils.Misc (spaceSep, ZonedTime)
import QDHXB.Utils.BPP (bpp)
import QDHXB.Internal.Generate (simpleTypeDecoder)

instance Eq Content where
  _ == _ = False
