
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
  mapName, zonedTimeName, xName, xVarE, justMatchId,
  contentName, ioName, aName, eName, ctxtName, readName,
  stringConT, contentConT, maybeConT, showConT, eqConT, zonedTimeConT, ioConT,
  readVarE, errorVarE, mapVarE, zomToListVarE, justConE, nothingConE,
  nothingPat, justPat,
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

-- | TH `Data.Time.Clock.DiffTime` type representation
diffTimeType :: Type
diffTimeType = ConT (mkName "DiffTime")

-- | TH `Data.Time.Clock.DiffTime` converter from `String`.
diffTimeBasicDecoder :: Exp -> Exp
diffTimeBasicDecoder expr = SigE (AppE readVarE expr) diffTimeType

-- | TH `Data.Time.LocalTime.TimeOfDay` type representation
timeOfDayType :: Type
timeOfDayType = ConT (mkName "TimeOfDay")

-- | TH `Data.Time.LocalTime.TimeOfDay` converter from `String`.
timeOfDayBasicDecoder :: Exp -> Exp
timeOfDayBasicDecoder expr = SigE (AppE readVarE expr) timeOfDayType

-- | TH `Data.Time.Calendar.OrdinalDate.Day` type representation
dayType :: Type
dayType = ConT (mkName "Day")

-- | TH `Data.Time.Calendar.OrdinalDate.Day` converter from `String`.
dayBasicDecoder :: Exp -> Exp
dayBasicDecoder expr = SigE (AppE readVarE expr) dayType

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

-- | TH `Name` for "x"
xName :: Name
xName = mkName "x"

-- | TH `Exp` for "x"
xVarE :: Exp
xVarE = VarE xName

justMatchId :: Match
justMatchId = Match (justPat xName) (NormalB xVarE) []

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

-- | TH `Name` for `Text.XML.Light.Types.Content`
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

-- | TH `Name` for `Data.Time.LocalTime.ZonedTime`
zonedTimeName :: Name
zonedTimeName = mkName "ZonedTime"

-- | TH `Name` for "map"
mapName :: Name
mapName = mkName "map"

-- | TH `Name` for "read"
readName :: Name
readName = mkName "read"

-- | TH `Name` for `QDHXB.Internal.Utils.XMLLight.zomToList`
zomToListName :: Name
zomToListName = mkName "zomToList"

-- | TH `Name` for "a"
aName :: Name
aName = mkName "a"

-- | TH `Exp` for function `read`
readVarE :: Exp
readVarE = VarE readName

-- | TH `Exp` for function `QDHXB.Internal.Utils.XMLLight.zomToList`
zomToListVarE :: Exp
zomToListVarE = VarE zomToListName

-- | TH `Exp` for function `map`
mapVarE :: Exp
mapVarE = VarE mapName

-- | TH `Exp` for function `error`
errorVarE :: Exp
errorVarE = VarE errorName

-- | TH `Exp` for constructor `Just`
justConE :: Exp
justConE = ConE justName

-- | TH `Exp` for constructor `Nothing`
nothingConE :: Exp
nothingConE = ConE nothingName

-- | TH `Pat` for matching constructor `Nothing`
nothingPat :: Pat
nothingPat = ConP nothingName [] []

-- | TH `Pat` for matching constructor `Nothing`
justPat :: Name -> Pat
justPat n = ConP justName [] [VarP n]

-- | TH `Type` for `String`
stringConT :: Type
stringConT = ConT stringName

-- | TH `Type` for `Data.Time.LocalTime.ZonedTime`
zonedTimeConT :: Type
zonedTimeConT = ConT zonedTimeName

-- | TH `Type` for `Text.XML.Light.Types.Content`
contentConT :: Type
contentConT = ConT contentName

-- | TH `Type` for `IO`
ioConT :: Type
ioConT = ConT ioName

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

-- | Convert a `String` into a Template Haskell `Exp`ression
-- representing that string literal.
quoteStr :: String -> Exp
quoteStr = LitE . StringL
