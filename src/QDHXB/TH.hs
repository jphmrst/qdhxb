{-# LANGUAGE TemplateHaskell #-}

module QDHXB.TH where

import Language.Haskell.TH
-- import System.Directory
import Data.Char
import Text.XML.Light.Types

decodeTypeAttrVal :: String -> Type
decodeTypeAttrVal ('x':'s':':':str) = decodeTypeAttrVal str
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
decodeTypeAttrVal "string" = stringType
decodeTypeAttrVal "time" = timeOfDayType
decodeTypeAttrVal name = ConT $ mkName $ firstToUpper name

intType :: Type
intType = ConT (mkName "Int")

stringType :: Type
stringType = ConT (mkName "String")

floatType :: Type
floatType = ConT (mkName "Float")

boolType :: Type
boolType = ConT (mkName "Bool")

doubleType :: Type
doubleType = ConT (mkName "Double")

zonedTimeType :: Type
zonedTimeType = ConT (mkName "ZonedTime")

diffTimeType :: Type
diffTimeType = ConT (mkName "DiffTime")

timeOfDayType :: Type
timeOfDayType = ConT (mkName "TimeOfDay")

dayType :: Type
dayType = ConT (mkName "Day")

qnameType :: Type
qnameType = ConT (mkName "QName")

firstToUpper :: String -> String
firstToUpper "" = ""
firstToUpper (c:cs) = toUpper c : cs

{-
booleanTestBinding :: String -> Dec
booleanTestBinding name =
  ValD (VarP $ mkName name) (NormalB $ ConE $ mkName "True") []
-}
