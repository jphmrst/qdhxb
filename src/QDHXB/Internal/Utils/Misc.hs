
-- | Template Haskell definitions
module QDHXB.Internal.Utils.Misc (
  compressMaybe, qFirstToUpper, qTransformName
  ) where
import Text.XML.Light.Types
import QDHXB.Internal.Utils.TH


-- | Compress nested `Maybe` types into a single one.
compressMaybe :: Maybe (Maybe a) -> Maybe a
compressMaybe Nothing = Nothing
compressMaybe (Just v) = v

-- | Return a new `QName` with the first letter of the core name
-- capitalized.
qFirstToUpper :: QName -> QName
qFirstToUpper = qTransformName firstToUpper

-- | Return a new `QName` applying the given transformation to the
-- core name.
qTransformName :: (String -> String) -> QName -> QName
qTransformName f (QName n u p) = QName (f n) u p
