{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Template Haskell definitions
module QDHXB.TH (
  -- * Possibly-absent integers from XSD text
  decodeIntOrUnbound, decodeMaybeIntOrUnbound1, xsdTypeNameTranslation,

  -- * XSD types
  intType, stringType, floatType, boolType, doubleType,
  zonedTimeType, diffTimeType, timeOfDayType, dayType, qnameType,

  intBasicDecoder, stringBasicDecoder, floatBasicDecoder,
  boolBasicDecoder, doubleBasicDecoder, zonedTimeBasicDecoder,
  diffTimeBasicDecoder, timeOfDayBasicDecoder, dayBasicDecoder,
  qnameBasicDecoder,

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
xsdTypeNameTranslation :: String -> (Type, Exp -> Exp)
xsdTypeNameTranslation ('x':'s':':':str) = xsdTypeNameTranslation str
xsdTypeNameTranslation "anyType" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "anySimpleType" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "anyAtomicType" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "anyURI" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "boolean" = (boolType, boolBasicDecoder)
xsdTypeNameTranslation "date" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "dateTime" = (zonedTimeType, zonedTimeBasicDecoder)
xsdTypeNameTranslation "decimal" = (doubleType, doubleBasicDecoder)
xsdTypeNameTranslation "double" = (doubleType, doubleBasicDecoder)
xsdTypeNameTranslation "duration" = (diffTimeType, diffTimeBasicDecoder)
xsdTypeNameTranslation "float" = (floatType, floatBasicDecoder)
xsdTypeNameTranslation "hexBinary" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "gDay" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "gMonth" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "gMonthDay" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "gYear" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "gYearMonth" = (dayType, dayBasicDecoder)
xsdTypeNameTranslation "NOTATION" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "QName" = (qnameType, qnameBasicDecoder)
xsdTypeNameTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "integer" = (intType, intBasicDecoder)
xsdTypeNameTranslation "long" = (intType, intBasicDecoder)
xsdTypeNameTranslation "int" = (intType, intBasicDecoder)
xsdTypeNameTranslation "short" = (intType, intBasicDecoder)
xsdTypeNameTranslation "byte" = (intType, intBasicDecoder)
xsdTypeNameTranslation "nonNegativeInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "unsignedInt" = (intType, intBasicDecoder)
xsdTypeNameTranslation "unsignedShort" = (intType, intBasicDecoder)
xsdTypeNameTranslation "unsignedByte" = (intType, intBasicDecoder)
xsdTypeNameTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "nonPositiveInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "negativeInteger" = (intType, intBasicDecoder)
xsdTypeNameTranslation "string" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "normalizedString" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "token" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "language" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "Name" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "NCName" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "ENTITY" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "ID" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "IDREF" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "NMTOKEN" = (stringType, stringBasicDecoder)
xsdTypeNameTranslation "time" = (timeOfDayType, timeOfDayBasicDecoder)
xsdTypeNameTranslation "ENTITIES" = (stringListType, stringListBasicDecoder)
xsdTypeNameTranslation "IDREFS" = (stringListType, stringListBasicDecoder)
xsdTypeNameTranslation "MNTOKENS" = (stringListType, stringListBasicDecoder)
xsdTypeNameTranslation name = (ConT $ mkName $ firstToUpper name, id)

-- | TH `Int` type representation
intType :: Type
intType = ConT (mkName "Int")

-- | TH `Int` type converter from `String`
intBasicDecoder :: Exp -> Exp
intBasicDecoder expr = SigE (AppE (VarE $ mkName "read") expr)
                            (ConT (mkName "Int"))

-- | TH `String` type representation
stringType :: Type
stringType = ConT (mkName "String")

-- | TH `String` type converter from `String`
stringBasicDecoder :: Exp -> Exp
stringBasicDecoder expr = expr

-- | TH `String` list type representation
stringListType :: Type
stringListType = AppT ListT stringType

-- | TH `String` list converter from `String`.
stringListBasicDecoder :: Exp -> Exp
stringListBasicDecoder _ = error "TODO"

-- | TH `Float` type representation
floatType :: Type
floatType = ConT (mkName "Float")

-- | TH `Float` converter from `String`.
floatBasicDecoder :: Exp -> Exp
floatBasicDecoder expr = SigE (AppE (VarE $ mkName "read") expr)
                              (ConT (mkName "Float"))

-- | TH `Bool` type representation
boolType :: Type
boolType = ConT (mkName "Bool")

-- | TH `Bool` converter from `String`.
boolBasicDecoder :: Exp -> Exp
boolBasicDecoder expr = SigE (AppE (VarE $ mkName "read") expr)
                             (ConT (mkName "Bool"))

-- | TH `Double` type representation
doubleType :: Type
doubleType = ConT (mkName "Double")

-- | TH `Double` converter from `String`.
doubleBasicDecoder :: Exp -> Exp
doubleBasicDecoder expr = SigE (AppE (VarE $ mkName "read") expr)
                               (ConT (mkName "Double"))

-- | TH `Data.Time.LocalTime.ZonedTime` type representation
zonedTimeType :: Type
zonedTimeType = ConT (mkName "ZonedTime")

-- | TH `Data.Time.LocalTime.ZonedTime` converter from `String`.
zonedTimeBasicDecoder :: Exp -> Exp
zonedTimeBasicDecoder _ = error "TODO"

-- | TH `Data.Time.LocalTime.DiffTime` type representation
diffTimeType :: Type
diffTimeType = ConT (mkName "DiffTime")

-- | TH `Data.Time.LocalTime.DiffTime` converter from `String`.
diffTimeBasicDecoder :: Exp -> Exp
diffTimeBasicDecoder _ = error "TODO"

-- | TH `Data.Time.LocalTime.TimeOfDay` type representation
timeOfDayType :: Type
timeOfDayType = ConT (mkName "TimeOfDay")

-- | TH `Data.Time.LocalTime.TimeOfDay` converter from `String`.
timeOfDayBasicDecoder :: Exp -> Exp
timeOfDayBasicDecoder _ = error "TODO"

-- | TH `Data.Time.LocalTime.Day` type representation
dayType :: Type
dayType = ConT (mkName "Day")

-- | TH `Data.Time.LocalTime.Day` converter from `String`.
dayBasicDecoder :: Exp -> Exp
dayBasicDecoder _ = error "TODO"

-- | TH `Text.XML.Light.Types.QName` type representation
qnameType :: Type
qnameType = ConT (mkName "QName")

-- | TH `Text.XML.Light.Types.QName` converter from `String`.
qnameBasicDecoder :: Exp -> Exp
qnameBasicDecoder _ = error "TODO"

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

