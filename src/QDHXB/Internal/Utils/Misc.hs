
-- | Template Haskell definitions
module QDHXB.Internal.Utils.Misc (
  compressMaybe, qFirstToUpper, qTransformName, spaceSep
  ) where
import Data.Char (isSpace)
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

-- | Divide a string into space-separated elements, allowing extra
-- spaces.
spaceSep :: String -> [String]
spaceSep "" = []
spaceSep (x:xs) | isSpace x = spaceSep xs
spaceSep (x:xs) = spaceSep' [x] xs
  where spaceSep' :: String -> String -> [String]
        spaceSep' acc [] = [reverse acc]
        spaceSep' acc (z:zs) | isSpace z = reverse acc : spaceSep zs
        spaceSep' acc (z:zs) = spaceSep' (z:acc) zs
