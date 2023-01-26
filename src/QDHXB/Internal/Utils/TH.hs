
-- | Template Haskell definitions
module QDHXB.Internal.Utils.TH (
  -- * Possibly-absent integers from XSD text
  xsdTypeNameTranslation,

  -- * XSD types
  intType, stringType, floatType, boolType, doubleType,
  zonedTimeType, diffTimeType, timeOfDayType, dayType, qnameType,

  intBasicDecoder, stringBasicDecoder, floatBasicDecoder,
  boolBasicDecoder, doubleBasicDecoder, zonedTimeBasicDecoder,
  diffTimeBasicDecoder, timeOfDayBasicDecoder, dayBasicDecoder,
  qnameBasicDecoder,

  -- * Utilities for building expressions for ZOM types
  zeroName, oneName, manyName, zomToListName,
  nothingName, justName, maybeName,
  stringName, errorName, eqName, showName, intName, floatName, boolName,
  mapName, zonedTimeName,
  contentName, ioName, aName, eName, ctxtName, readName,
  stringConT, contentConT, maybeConT, showConT, eqConT, zonedTimeConT,
  readVarE, errorVarE, mapVarE, zomToListVarE,
  quoteStr,

  -- * Miscellaneous
  firstToUpper, todoStr, throwsError, fn1Type, fn2Type)
where

import Language.Haskell.TH
-- import System.Directory
import Data.Char

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
intType = ConT intName

-- | TH `Int` type converter from `String`
intBasicDecoder :: Exp -> Exp
intBasicDecoder expr = SigE (AppE readVarE expr) intType

-- | TH `String` type representation
stringType :: Type
stringType = ConT stringName

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
floatBasicDecoder expr = SigE (AppE readVarE expr) floatType

-- | TH `Bool` type representation
boolType :: Type
boolType = ConT (mkName "Bool")

-- | TH `Bool` converter from `String`.
boolBasicDecoder :: Exp -> Exp
boolBasicDecoder expr = SigE (AppE readVarE expr) boolType

-- | TH `Double` type representation
doubleType :: Type
doubleType = ConT (mkName "Double")

-- | TH `Double` converter from `String`.
doubleBasicDecoder :: Exp -> Exp
doubleBasicDecoder expr = SigE (AppE readVarE expr) doubleType

-- | TH `Data.Time.LocalTime.ZonedTime` type representation
zonedTimeType :: Type
zonedTimeType = zonedTimeConT

-- | TH `Data.Time.LocalTime.ZonedTime` converter from `String`.
zonedTimeBasicDecoder :: Exp -> Exp
zonedTimeBasicDecoder expr = SigE (AppE readVarE expr) zonedTimeConT

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

-- | TH `Name` for "Nothing"
nothingName :: Name
nothingName = mkName "Nothing"

-- | TH `Name` for "Just"
justName :: Name
justName = mkName "Just"

-- | TH `Name` for "Zero"
zeroName :: Name
zeroName = mkName "Zero"

-- | TH `Name` for "Int"
intName :: Name
intName = mkName "Int"

-- | TH `Name` for "Float"
floatName :: Name
floatName = mkName "Float"

-- | TH `Name` for "Bool"
boolName :: Name
boolName = mkName "Bool"

-- | TH `Name` for "One"
oneName :: Name
oneName = mkName "One"

-- | TH `Name` for "Many"
manyName :: Name
manyName = mkName "Many"

-- | TH `Name` for "String"
stringName :: Name
stringName = mkName "String"

-- | TH `Name` for "Content"
contentName :: Name
contentName = mkName "Content"

-- | TH `Name` for "IO"
ioName :: Name
ioName = mkName "IO"

-- | TH `Name` for "e"
eName :: Name
eName = mkName "e"

-- | TH `Name` for "ctxt"
ctxtName :: Name
ctxtName = mkName "ctxt"

-- | TH `Name` for "error"
errorName :: Name
errorName = mkName "error"

-- | TH `Name` for "Maybe"
maybeName :: Name
maybeName = mkName "Maybe"

-- | TH `Name` for "Eq"
eqName :: Name
eqName = mkName "Eq"

-- | TH `Name` for "Show"
showName :: Name
showName = mkName "Show"

-- | TH `Name` for "ZonedTime"
zonedTimeName :: Name
zonedTimeName = mkName "ZonedTime"

-- | TH `Name` for "map"
mapName :: Name
mapName = mkName "map"

-- | TH `Name` for "read"
readName :: Name
readName = mkName "read"

-- | TH `Name` for "zomToList"
zomToListName :: Name
zomToListName = mkName "zomToList"

-- | TH `Name` for "a"
aName :: Name
aName = mkName "a"

-- | TH `Expr` for function `read`
readVarE :: Exp
readVarE = VarE readName

-- | TH `Expr` for function `zomToList`
zomToListVarE :: Exp
zomToListVarE = VarE zomToListName

-- | TH `Expr` for function `map`
mapVarE :: Exp
mapVarE = VarE mapName

-- | TH `Expr` for function `error`
errorVarE :: Exp
errorVarE = VarE errorName

-- | TH `Type` for `String`
stringConT :: Type
stringConT = ConT stringName

-- | TH `Type` for `ZonedTime`
zonedTimeConT :: Type
zonedTimeConT = ConT zonedTimeName

-- | TH `Type` for `Content`
contentConT :: Type
contentConT = ConT contentName

-- | TH `Type` for `Maybe`
maybeConT :: Type
maybeConT = ConT maybeName

-- | TH `Type` for `Show`
showConT :: Type
showConT = ConT showName

-- | TH `Type` for `Eq`
eqConT :: Type
eqConT = ConT eqName

-- | TH `Exp` which will throw an exception with the given name.
throwsError :: String -> Exp
throwsError msg = AppE errorVarE (LitE $ StringL msg)

-- | TH `Type` for the one-argument function type of the given
-- argument and result.
fn1Type :: Type -> Type -> Type
fn1Type argT resT = (AppT (AppT ArrowT argT) resT)

-- | TH `Type` for the two-argument function type of the given
-- (curried) arguments and result.
fn2Type :: Type -> Type -> Type -> Type
fn2Type arg1T arg2T resT = fn1Type arg1T $ fn1Type arg2T resT

quoteStr :: String -> Exp
quoteStr = LitE . StringL
