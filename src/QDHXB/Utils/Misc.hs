
-- | Template Haskell definitions
module QDHXB.Utils.Misc (
  applyFst, applySnd,
  compressMaybe, qFirstToUpper, qTransformName, spaceSep, chomp, pickOrCombine,
  ifAtLine, maybeToList, ZonedTime(..)
  ) where
import Data.Char (isSpace)
import Text.XML.Light.Types
import Data.Time.LocalTime (zonedTimeToUTC)
import qualified Data.Time.LocalTime as LT
import QDHXB.Utils.TH

-- | Apply a function to the first element of a pair, and return a
-- pair with the original second element.
applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)

-- | Apply a function to the second element of a pair, and return a
-- pair with the original first element.
applySnd :: (a -> b) -> (c, a) -> (c, b)
applySnd f (x,y) = (x, f y)

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

-- | If there is one, return a reference to a line number in an XSD
-- file (and an empty string otherwise).
ifAtLine :: Maybe Line -> String
ifAtLine ifLine = maybe "" (\line -> " at XSD line " ++ show line) ifLine

-- | Convert a `Maybe` value to a list (which is either empty or
-- singleton).
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just n) = [n]

-- | Synonym for `Data.Time.LocalTime.ZonedTime` with `Eq` membership
-- (based on conversion to UTC per
-- [https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-LocalTime.html]).
newtype ZonedTime = ZonedTime LT.ZonedTime deriving (Show, Read)

instance Eq ZonedTime where
  (ZonedTime zt1) == (ZonedTime zt2) = zonedTimeToUTC zt1 == zonedTimeToUTC zt2

