
-- | Options for QDHXB calls.
module QDHXB.Expansions (
  readMaybe,
  simpleTypeDecoder,
  Content,
  ZeroOneMany(Zero, One, Many), zomToList, __loadElement, pullContentFrom,
  Except, throwError, catchError, runExcept
  )
where

import Control.Monad.Except (Except, throwError, runExcept, catchError)
import Text.Read (readMaybe)
import Text.XML.Light.Types (Content)
import QDHXB.Internal.Utils.XMLLight (
  ZeroOneMany(Zero, One, Many), zomToList, __loadElement, pullContentFrom
  )
import QDHXB.Internal.Generate (simpleTypeDecoder)
