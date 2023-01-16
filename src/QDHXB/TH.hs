{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Template Haskell definitions
module QDHXB.TH (
  -- * Possibly-absent integers from XSD text
  decodeIntOrUnbound, decodeMaybeIntOrUnbound1, decodeTypeAttrVal,

  -- * XSD types
  intType, stringType, floatType, boolType, doubleType,
  zonedTimeType, diffTimeType, timeOfDayType, dayType, qnameType,

  -- * Utilities for building expressions for ZOM types
  zeroName, oneName, manyName,

  -- * Miscellaneous
  firstToUpper, todoStr, throwsError)
where

import Language.Haskell.TH
-- import System.Directory
import Data.Char
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import QDHXB.Internal.XSDQ


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
decodeTypeAttrVal :: String -> Type
decodeTypeAttrVal ('x':'s':':':str) = decodeTypeAttrVal str
decodeTypeAttrVal "anyType" = stringType
decodeTypeAttrVal "anySimpleType" = stringType
decodeTypeAttrVal "anyAtomicType" = stringType
decodeTypeAttrVal "anyURI" = stringType
decodeTypeAttrVal "boolean" = boolType
decodeTypeAttrVal "date" = dayType
decodeTypeAttrVal "dateTime" = zonedTimeType
decodeTypeAttrVal "decimal" = doubleType
decodeTypeAttrVal "double" = doubleType
decodeTypeAttrVal "duration" = diffTimeType
decodeTypeAttrVal "float" = floatType
decodeTypeAttrVal "hexBinary" = stringType
decodeTypeAttrVal "gDay" = dayType
decodeTypeAttrVal "gMonth" = dayType
decodeTypeAttrVal "gMonthDay" = dayType
decodeTypeAttrVal "gYear" = dayType
decodeTypeAttrVal "gYearMonth" = dayType
decodeTypeAttrVal "NOTATION" = stringType
decodeTypeAttrVal "QName" = qnameType
decodeTypeAttrVal "positiveInteger" = intType
decodeTypeAttrVal "integer" = intType
decodeTypeAttrVal "long" = intType
decodeTypeAttrVal "int" = intType
decodeTypeAttrVal "short" = intType
decodeTypeAttrVal "byte" = intType
decodeTypeAttrVal "nonNegativeInteger" = intType
decodeTypeAttrVal "positiveInteger" = intType
decodeTypeAttrVal "unsignedInt" = intType
decodeTypeAttrVal "unsignedShort" = intType
decodeTypeAttrVal "unsignedByte" = intType
decodeTypeAttrVal "positiveInteger" = intType
decodeTypeAttrVal "nonPositiveInteger" = intType
decodeTypeAttrVal "negativeInteger" = intType
decodeTypeAttrVal "string" = stringType
decodeTypeAttrVal "normalizedString" = stringType
decodeTypeAttrVal "token" = stringType
decodeTypeAttrVal "language" = stringType
decodeTypeAttrVal "Name" = stringType
decodeTypeAttrVal "NCName" = stringType
decodeTypeAttrVal "ENTITY" = stringType
decodeTypeAttrVal "ID" = stringType
decodeTypeAttrVal "IDREF" = stringType
decodeTypeAttrVal "NMTOKEN" = stringType
decodeTypeAttrVal "time" = timeOfDayType
decodeTypeAttrVal "ENTITIES" = stringListType
decodeTypeAttrVal "IDREFS" = stringListType
decodeTypeAttrVal "MNTOKENS" = stringListType
decodeTypeAttrVal name = ConT $ mkName $ firstToUpper name

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

zeroName :: Name
zeroName = mkName "Zero"

oneName :: Name
oneName = mkName "One"

manyName :: Name
manyName = mkName "Many"

throwsError :: String -> Exp
throwsError msg = AppE (VarE $ mkName "error") (LitE $ StringL msg)

