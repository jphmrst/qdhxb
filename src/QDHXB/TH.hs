{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Template Haskell definitions
module QDHXB.TH (
  -- * Possibly-absent integers from XSD text
  decodeIntOrUnbound, decodeMaybeIntOrUnbound1, xsdTypeNameToType,

  -- * XSD types
  intType, stringType, floatType, boolType, doubleType,
  zonedTimeType, diffTimeType, timeOfDayType, dayType, qnameType,

  -- * Utilities for building expressions for ZOM types
  zeroName, oneName, manyName, nothingName, justName,

  -- * Miscellaneous
  firstToUpper, todoStr, throwsError)
where

import Language.Haskell.TH
-- import System.Directory
import Data.Char

-- | Decode the `String` representation of an XSD integer as a Haskell
-- `Int`.  Might fail, so the result is `Maybe`-wrapped.
decodeIntOrUnbound :: String -> Maybe Int
decodeIntOrUnbound "unbounded" = Nothing
decodeIntOrUnbound s = Just $ read s

-- | Another decoder of the `String` representation of an XSD integer
-- as a Haskell `Int`, where there may be no `String` in the first
-- place.
decodeMaybeIntOrUnbound1 :: Maybe String -> Maybe Int
decodeMaybeIntOrUnbound1 Nothing = Just 1
decodeMaybeIntOrUnbound1 (Just s) = decodeIntOrUnbound s

-- | Convert the `String` representation of a primitive XSD type to a
-- Template Haskell `Type`.
xsdTypeNameToType :: String -> Type
xsdTypeNameToType ('x':'s':':':str) = xsdTypeNameToType str
xsdTypeNameToType "anyType" = stringType
xsdTypeNameToType "anySimpleType" = stringType
xsdTypeNameToType "anyAtomicType" = stringType
xsdTypeNameToType "anyURI" = stringType
xsdTypeNameToType "boolean" = boolType
xsdTypeNameToType "date" = dayType
xsdTypeNameToType "dateTime" = zonedTimeType
xsdTypeNameToType "decimal" = doubleType
xsdTypeNameToType "double" = doubleType
xsdTypeNameToType "duration" = diffTimeType
xsdTypeNameToType "float" = floatType
xsdTypeNameToType "hexBinary" = stringType
xsdTypeNameToType "gDay" = dayType
xsdTypeNameToType "gMonth" = dayType
xsdTypeNameToType "gMonthDay" = dayType
xsdTypeNameToType "gYear" = dayType
xsdTypeNameToType "gYearMonth" = dayType
xsdTypeNameToType "NOTATION" = stringType
xsdTypeNameToType "QName" = qnameType
xsdTypeNameToType "positiveInteger" = intType
xsdTypeNameToType "integer" = intType
xsdTypeNameToType "long" = intType
xsdTypeNameToType "int" = intType
xsdTypeNameToType "short" = intType
xsdTypeNameToType "byte" = intType
xsdTypeNameToType "nonNegativeInteger" = intType
xsdTypeNameToType "positiveInteger" = intType
xsdTypeNameToType "unsignedInt" = intType
xsdTypeNameToType "unsignedShort" = intType
xsdTypeNameToType "unsignedByte" = intType
xsdTypeNameToType "positiveInteger" = intType
xsdTypeNameToType "nonPositiveInteger" = intType
xsdTypeNameToType "negativeInteger" = intType
xsdTypeNameToType "string" = stringType
xsdTypeNameToType "normalizedString" = stringType
xsdTypeNameToType "token" = stringType
xsdTypeNameToType "language" = stringType
xsdTypeNameToType "Name" = stringType
xsdTypeNameToType "NCName" = stringType
xsdTypeNameToType "ENTITY" = stringType
xsdTypeNameToType "ID" = stringType
xsdTypeNameToType "IDREF" = stringType
xsdTypeNameToType "NMTOKEN" = stringType
xsdTypeNameToType "time" = timeOfDayType
xsdTypeNameToType "ENTITIES" = stringListType
xsdTypeNameToType "IDREFS" = stringListType
xsdTypeNameToType "MNTOKENS" = stringListType
xsdTypeNameToType name = ConT $ mkName $ firstToUpper name

-- | TH `Int` type representation
intType :: Type
intType = ConT (mkName "Int")

-- | TH `String` type representation
stringType :: Type
stringType = ConT (mkName "String")

-- | TH `String` list type representation
stringListType :: Type
stringListType = AppT ListT stringType

-- | TH `Float` type representation
floatType :: Type
floatType = ConT (mkName "Float")

-- | TH `Bool` type representation
boolType :: Type
boolType = ConT (mkName "Bool")

-- | TH `Double` type representation
doubleType :: Type
doubleType = ConT (mkName "Double")

-- | TH `Data.Time.LocalTime.ZonedTime` type representation
zonedTimeType :: Type
zonedTimeType = ConT (mkName "ZonedTime")

-- | TH `Data.Time.LocalTime.DiffTime` type representation
diffTimeType :: Type
diffTimeType = ConT (mkName "DiffTime")

-- | TH `Data.Time.LocalTime.TimeOfDay` type representation
timeOfDayType :: Type
timeOfDayType = ConT (mkName "TimeOfDay")

-- | TH `Data.Time.LocalTime.Day` type representation
dayType :: Type
dayType = ConT (mkName "Day")

-- | TH `Text.XML.Light.Types.QName` type representation
qnameType :: Type
qnameType = ConT (mkName "QName")

-- | Capitalize the first character of a `String`.
firstToUpper :: String -> String
firstToUpper "" = ""
firstToUpper (c:cs) = toUpper c : cs

{-
booleanTestBinding :: String -> Dec
booleanTestBinding name =
  ValD (VarP $ mkName name) (NormalB $ ConE $ mkName "True") []
-}

-- | Helper definition, to get a TODO-marker into in-progress TH work.
-- Should go away eventually.
todoStr :: String
todoStr = "TODO"

nothingName :: Name
nothingName = mkName "Nothing"

justName :: Name
justName = mkName "Just"

zeroName :: Name
zeroName = mkName "Zero"

oneName :: Name
oneName = mkName "One"

manyName :: Name
manyName = mkName "Many"

throwsError :: String -> Exp
throwsError msg = AppE (VarE $ mkName "error") (LitE $ StringL msg)

