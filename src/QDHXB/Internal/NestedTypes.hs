
-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  TypeScheme(Sequence, Restriction, Extension),
  DataScheme(
      ElementScheme, AttributeScheme, ComplexTypeScheme, SimpleTypeScheme),
  formatDataScheme, formatDataScheme',
  formatDataSchemes',
  formatDataSchemeInd', formatDataSchemesInd'
) where

import Data.List (intercalate)
import Text.XML.Light.Types (QName)

-- | Representation of certain definitions of one XSD type based on
-- another type.
data TypeScheme =
  Sequence -- ^ The <sequence> complex type.
    [DataScheme] -- ^ List of associated definitions
  | Restriction -- ^ One type with certain values excluded.
    QName -- ^ Base type
  | Extension -- ^ One type extended with additional elements.
    QName -- ^ Base type
    [DataScheme] -- ^ Additional elements
  deriving Show

formatTypeScheme__ :: String -> TypeScheme -> [String]
formatTypeScheme__ ind (Sequence ds) =
  "Sequence"
   : formatDataSchemesInd__ (ind ++ "  ") ds
formatTypeScheme__ _ (Restriction r) = ["Restriction " ++ show r]
formatTypeScheme__ ind (Extension base ds) =
  ("Extension " ++ show base)
   : formatDataSchemesInd__ (ind ++ "  ") ds

formatTypeSchemeInd__ :: String -> TypeScheme -> [String]
formatTypeSchemeInd__ ind s = case formatTypeScheme__ ind s of
  [] -> []
  x:xs -> (ind ++ x) : xs

-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  ElementScheme [DataScheme] -- ^ contents
                (Maybe QName) -- ^ ifName
                (Maybe QName) -- ^ ifType
                (Maybe QName) -- ^ ifRef
                (Maybe Int) -- ^ ifMin
                (Maybe Int) -- ^ ifMax
  | AttributeScheme (Maybe QName) -- ^ ifName
                    (Maybe QName) -- ^ ifType
                    (Maybe QName) -- ^ ifRef
                    String -- ^ use mode: prohibited, optional
                           -- (default), required
  | ComplexTypeScheme TypeScheme -- ^ typeDetail
                      [DataScheme] -- ^ addlAttrs
                      (Maybe QName) -- ^ ifName
  | SimpleTypeScheme QName -- ^ baseSpec
                     QName -- ^ name
  deriving Show

-- | Pretty-print a `DataScheme`.
formatDataScheme :: DataScheme -> String
formatDataScheme = formatDataScheme' ""

-- | Pretty-print a `DataScheme` with the given indentation (except
-- the first line not indented).
formatDataScheme' :: String -> DataScheme -> String
formatDataScheme' ind = intercalate "\n" . formatDataScheme__ ind

-- | Pretty-print and vertically-align a list of `DataScheme` elements
-- (except the first line not indented).
formatDataSchemes' :: String -> [DataScheme] -> String
formatDataSchemes' _ [] = ""
formatDataSchemes' ind (x:xs) =
  formatDataScheme' ind x ++ "\n" ++ formatDataSchemesInd' ind xs

-- | Pretty-print a `DataScheme` with the given indentation on all
-- lines.
formatDataSchemeInd' :: String -> DataScheme -> String
formatDataSchemeInd' ind ds = ind ++ formatDataScheme' ind ds

-- | Pretty-print and vertically-align a list of `DataScheme` elements
-- on all lines.
formatDataSchemesInd' :: String -> [DataScheme] -> String
formatDataSchemesInd' ind = intercalate "\n" . map (formatDataSchemeInd' ind)

formatDataScheme__ :: String -> DataScheme -> [String]
formatDataScheme__ ind (ElementScheme ctnts ifName ifType ifRef ifMin ifMax) =
  ("ElementScheme name="
     ++ (case ifName of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
     ++ " type="
     ++ (case ifType of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
     ++ " ref="
     ++ (case ifRef of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
     ++ " min="
     ++ (case ifMin of
           Nothing -> "undef"
           Just s  -> show s)
     ++ " max="
     ++ (case ifMax of
           Nothing -> "undef"
           Just s  -> show s))
   : (foldl (++) [] $ map (formatDataSchemeInd__ (ind ++ "  ")) ctnts)

formatDataScheme__ _ (AttributeScheme ifName ifType ifRef usage) = [
  "AttributeScheme name="
    ++ (case ifName of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
    ++ " type="
    ++ (case ifType of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
     ++ " ref="
     ++ (case ifRef of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\"")
     ++ " usage=\"" ++ usage ++ "\""
  ]

formatDataScheme__ ind (ComplexTypeScheme form attrs ifName) =
  ("ComplexTypeScheme name="
     ++ (case ifName of
           Nothing -> "undef"
           Just s  -> "\"" ++ show s ++ "\""))
  : (formatTypeSchemeInd__ (ind ++ "  ") form
     ++ (concat $ map (formatDataSchemeInd__ (ind ++ "  ")) attrs))

formatDataScheme__ _ (SimpleTypeScheme base name) = [
  "SimpleTypeScheme base=\"" ++ show base ++ "\" name=\"" ++ show name ++ "\""
  ]

formatDataSchemeInd__ :: String -> DataScheme -> [String]
formatDataSchemeInd__ ind s = case formatDataScheme__ ind s of
  [] -> []
  x:xs -> (ind ++ x) : xs

formatDataSchemesInd__ :: String -> [DataScheme] -> [String]
formatDataSchemesInd__ ind = concat . map (formatDataSchemeInd__ ind)

