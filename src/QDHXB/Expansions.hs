
-- | Re-exports of values used in generated TH code.
module QDHXB.Expansions (
  readMaybe, mapM,
  simpleTypeDecoder,
  Content,
  ZeroOneMany(Zero, One, Many), zomToList, __loadElement, pullContentFrom,
  Except, throwError, catchError, runExcept,
  spaceSep
  )
where

import Control.Monad.Except (Except, throwError, runExcept, catchError)
import Text.Read (readMaybe)
import Text.XML.Light.Types (Content)
import QDHXB.Internal.Utils.XMLLight (
  ZeroOneMany(Zero, One, Many), zomToList, __loadElement, pullContentFrom
  )
import QDHXB.Internal.Utils.Misc (spaceSep)
import QDHXB.Internal.Generate (simpleTypeDecoder)
