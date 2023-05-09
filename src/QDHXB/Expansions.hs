
-- | Re-exports of values used in generated TH code.
module QDHXB.Expansions (
  readMaybe, mapM,
  simpleTypeDecoder,
  Content,
  ZeroOneMany(Zero, One, Many), zomToList, __loadElement, pullContentFrom,
  Except, throwError, catchError, runExcept,
  spaceSep, pullAttrFrom,
  Day, TimeOfDay, DiffTime, ZonedTime,
  HXBErr(..), HXBExcept,
  bpp, QName
  )
where

import Control.Monad.Except (Except, throwError, runExcept, catchError)
import Text.Read (readMaybe)
import Text.XML.Light.Types (Content, QName)
import Data.Time.Calendar.OrdinalDate (Day)
import Data.Time.LocalTime (TimeOfDay, ZonedTime)
import Data.Time.Clock (DiffTime)
import QDHXB.Errs
import QDHXB.Internal.Utils.ZeroOneMany
import QDHXB.Internal.Utils.XMLLight (
  __loadElement, pullContentFrom, pullAttrFrom)
import QDHXB.Internal.Utils.Misc (spaceSep)
import QDHXB.Internal.Utils.BPP (bpp)
import QDHXB.Internal.Generate (simpleTypeDecoder)
