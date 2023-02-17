
-- | Template Haskell definitions
module QDHXB.Internal.Utils.Misc (
  compressMaybe, qFirstToUpper, qTransformName, spaceSep, chomp, pickOrCombine
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

-- |Remove leading and trailing spaces from a `String`.
chomp :: String -> String
chomp e@"" = e
chomp (c:cs) | isSpace c = chomp cs
chomp (c:cs) = c : chomp' cs
  where chomp' [] = []
        chomp' [d] | isSpace d = []
        chomp' (d:ds) | isSpace d = case chomp' ds of
                                      r@"" -> r
                                      ds' -> d:ds'
        chomp' (d:ds) = d : chomp' ds

-- |Combine to possible `String`s, appending them with a line
-- separator if both are present.
pickOrCombine :: Maybe String -> Maybe String -> Maybe String
pickOrCombine Nothing x = x
pickOrCombine x Nothing = x
pickOrCombine (Just y) (Just z) = Just $ y ++ "\n\n" ++ z
