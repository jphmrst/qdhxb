
-- | Template Haskell definitions
module QDHXB.Internal.Utils.TH (
  -- * Possibly-absent integers from XSD text
  xsdNameToTypeTranslation, xsdNameToNameTranslation,

  -- * XSD types
  intType, stringType, floatType, boolType, doubleType,
  zonedTimeConT, diffTimeType, timeOfDayType, dayType, qnameType,
  timeOfDayType, stringListType,

  intBasicDecoder, stringBasicDecoder, floatBasicDecoder,
  boolBasicDecoder, doubleBasicDecoder, zonedTimeBasicDecoder,
  diffTimeBasicDecoder, timeOfDayBasicDecoder, dayBasicDecoder,
  qnameBasicDecoder, stringListBasicDecoder,

  -- * Building expressions related to the exceptions in `QDHXB.Errs`
  qHXBExcT,
  qMiscError, qNoValidContentInUnion, qAtMostOnceIn,
  qMustBePresentIn, qCrefMustBePresentIn, qCouldNotDecodeSimpleType,
  qthMiscError, qthNoValidContentInUnion, qthAtMostOnceIn,
  qthMustBePresentIn, qthCrefMustBePresentIn, qthCouldNotDecodeSimpleType,

  -- * Utilities for building expressions for other standard Haskell types and values
  caseLeftRight, caseLeftRight',
  caseNothingJust, caseNothingJust',
  -- ** Primitive types
  stringConT, quoteStr,
  -- ** Primitive functions
  errorVarE, throwsError, throwsErrorExp,
  -- ** `Eq`
  eqConT,
  -- ** `Read` and `Show`
  showConT, readVarE,
  -- ** `Maybe`
  maybeConT, appMaybeType,
  -- *** With `Nothing`
  nothingConE, nothingPat,
  -- *** With `Just`
  justConE, justMatchId, justPat, applyJust,
  -- ** `Data.List`
  mapVarE,
  -- ** `IO`
  ioConT,
  -- ** `Functor` and `Contol.Monad.Monad`
  fmapVarE, applyMapM,
  -- ** Utilities for building expressions with `Control.Monad.Except.Except`
  exceptConT, applyExceptCon,
  -- *** With `Control.Monad.Except.runAccept`
  runExceptVarE, applyRunExceptExp, resultOrThrow,
  -- *** With `Control.Monad.Except.throwError`
  throwVarE, applyThrowStmt, applyThrowStrStmt,
  applyThrowExp, applyThrowStrExp,
  -- *** With `Control.Monad.Except.catchError`
  catchErrorVarE, applyCatchErrorExp, replaceOnError,
  -- *** Monadic statements
  throwsExc, returnExp, applyReturn,
  -- ** Miscellaneous expression builders
  fn1Type, fn2Type, app2Exp, app3Exp, app4Exp,

  -- * `QDHXB.Internal.Utils.ZeroOneMany`
  zeroPat, onePat, manyPat, zomToListVarE,

  -- * @XMLLight@
  contentConT,

  -- * Functions used in TH expansions
  simpleTypeDecoderVarE, spaceSepApp,

  -- * Local names
  xName, yName, zName, resName,
  xVarE, yVarE, aName, eName, ctxtName, ctxtVarE, ctxtVarP,

  -- * Other
  firstToUpper, todoStr)
where

import Language.Haskell.TH
import Data.Char
import Text.XML.Light.Types (Line)

-- | Convert the `String` representation of a primitive XSD type to a
-- Template Haskell `Type`.
xsdNameToTypeTranslation :: String -> (Type, Exp -> Exp)
xsdNameToTypeTranslation ('x':'s':':':str) = xsdNameToTypeTranslation str
xsdNameToTypeTranslation "anyType" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "anySimpleType" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "anyAtomicType" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "anyURI" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "boolean" = (boolType, boolBasicDecoder)
xsdNameToTypeTranslation "date" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "dateTime" = (zonedTimeConT, zonedTimeBasicDecoder)
xsdNameToTypeTranslation "decimal" = (doubleType, doubleBasicDecoder)
xsdNameToTypeTranslation "double" = (doubleType, doubleBasicDecoder)
xsdNameToTypeTranslation "duration" = (diffTimeType, diffTimeBasicDecoder)
xsdNameToTypeTranslation "float" = (floatType, floatBasicDecoder)
xsdNameToTypeTranslation "hexBinary" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "gDay" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "gMonth" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "gMonthDay" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "gYear" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "gYearMonth" = (dayType, dayBasicDecoder)
xsdNameToTypeTranslation "NOTATION" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "QName" = (qnameType, qnameBasicDecoder)
xsdNameToTypeTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "integer" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "long" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "int" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "short" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "byte" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "nonNegativeInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "unsignedInt" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "unsignedShort" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "unsignedByte" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "positiveInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "nonPositiveInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "negativeInteger" = (intType, intBasicDecoder)
xsdNameToTypeTranslation "string" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "normalizedString" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "token" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "language" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "Name" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "NCName" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "ENTITY" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "ID" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "IDREF" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "NMTOKEN" = (stringType, stringBasicDecoder)
xsdNameToTypeTranslation "time" = (timeOfDayType, timeOfDayBasicDecoder)
xsdNameToTypeTranslation "ENTITIES" = (stringListType, stringListBasicDecoder)
xsdNameToTypeTranslation "IDREFS" = (stringListType, stringListBasicDecoder)
xsdNameToTypeTranslation "MNTOKENS" = (stringListType, stringListBasicDecoder)
xsdNameToTypeTranslation name = (ConT $ mkName $ firstToUpper name, applyReturn)

-- | Convert the `String` representation of a primitive XSD type to a
-- Template Haskell `Type`.
xsdNameToNameTranslation :: String -> (String, Exp -> Exp)
xsdNameToNameTranslation ('x':'s':':':str) = xsdNameToNameTranslation str
xsdNameToNameTranslation "anyType" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "anySimpleType" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "anyAtomicType" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "anyURI" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "boolean" = ("Bool", boolBasicDecoder)
xsdNameToNameTranslation "date" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "dateTime" = ("ZonedTime", zonedTimeBasicDecoder)
xsdNameToNameTranslation "decimal" = ("Double", doubleBasicDecoder)
xsdNameToNameTranslation "double" = ("Double", doubleBasicDecoder)
xsdNameToNameTranslation "duration" = ("DiffTime", diffTimeBasicDecoder)
xsdNameToNameTranslation "float" = ("Float", floatBasicDecoder)
xsdNameToNameTranslation "hexBinary" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "gDay" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "gMonth" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "gMonthDay" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "gYear" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "gYearMonth" = ("Day", dayBasicDecoder)
xsdNameToNameTranslation "NOTATION" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "QName" = ("QName", qnameBasicDecoder)
xsdNameToNameTranslation "positiveInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "integer" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "long" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "int" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "short" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "byte" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "nonNegativeInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "positiveInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "unsignedInt" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "unsignedShort" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "unsignedByte" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "positiveInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "nonPositiveInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "negativeInteger" = ("Int", intBasicDecoder)
xsdNameToNameTranslation "string" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "normalizedString" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "token" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "language" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "Name" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "NCName" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "ENTITY" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "ID" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "IDREF" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "NMTOKEN" = ("String", stringBasicDecoder)
xsdNameToNameTranslation "time" = ("TimeOfDay", timeOfDayBasicDecoder)
xsdNameToNameTranslation "ENTITIES" = ("StringList", stringListBasicDecoder)
xsdNameToNameTranslation "IDREFS" = ("StringList", stringListBasicDecoder)
xsdNameToNameTranslation "MNTOKENS" = ("StringList", stringListBasicDecoder)
xsdNameToNameTranslation name = (name, applyReturn)

-- | Convert an expression of type @Maybe a@ to an expression of type
-- `QDHXB.Errs.HXBExcept` @a@.  The first argument should be a
-- quotation of the `QDHXB.Errs.HXBErr` to be raised when the original
-- `Exp`ression returns `Nothing`.
maybeToExceptExp :: Exp -> Exp -> Exp
maybeToExceptExp s e = caseNothingJust e s $ applyReturn . VarE

-- | TH `Name` for "Int"
intName :: Name
intName = mkName "Int"

-- | TH `Int` type representation
intType :: Type
intType = ConT intName

-- | TH `Int` type converter from `String`
intBasicDecoder :: Exp -> Exp
intBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "Int" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType intType

-- | TH `Name` for "String"
stringName :: Name
stringName = mkName "String"

-- | TH `String` type representation
stringType :: Type
stringType = ConT stringName

-- | TH `String` type converter from `String`
stringBasicDecoder :: Exp -> Exp
stringBasicDecoder expr = applyReturn expr

-- | TH `String` list type representation
stringListType :: Type
stringListType = AppT ListT stringType

-- | TH `String` list converter from `String`.
stringListBasicDecoder :: Exp -> Exp
stringListBasicDecoder _ = error "TODO"

-- | TH `Name` for "Float"
floatName :: Name
floatName = mkName "Float"

-- | TH `Float` type representation
floatType :: Type
floatType = ConT floatName

-- | TH `Float` converter from `String`.
floatBasicDecoder :: Exp -> Exp
floatBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "Float" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType floatType

-- | TH `Name` for "Bool"
boolName :: Name
boolName = mkName "Bool"

-- | TH `Bool` type representation
boolType :: Type
boolType = ConT boolName

-- | TH `Bool` converter from `String`.
boolBasicDecoder :: Exp -> Exp
boolBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "Bool" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType boolType

-- | TH `Double` type representation
doubleName :: Name
doubleName = mkName "Double"

-- | TH `Double` type representation
doubleType :: Type
doubleType = ConT doubleName

-- | TH `Double` converter from `String`.
doubleBasicDecoder :: Exp -> Exp
doubleBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "Double" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType doubleType

-- | TH `Name` for `Data.Time.LocalTime.ZonedTime`
zonedTimeName :: Name
zonedTimeName = mkName "QDHXB.Expansions.ZonedTime"

-- | TH `Type` for `Data.Time.LocalTime.ZonedTime`
zonedTimeConT :: Type
zonedTimeConT = ConT zonedTimeName

-- | TH `Data.Time.LocalTime.ZonedTime` converter from `String`.
zonedTimeBasicDecoder :: Exp -> Exp
zonedTimeBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "ZonedTime" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType zonedTimeConT

-- | TH `Data.Time.Clock.DiffTime` type representation
diffTimeName :: Name
diffTimeName = mkName "QDHXB.Expansions.DiffTime"

-- | TH `Data.Time.Clock.DiffTime` type representation
diffTimeType :: Type
diffTimeType = ConT diffTimeName

-- | TH `Data.Time.Clock.DiffTime` converter from `String`.
diffTimeBasicDecoder :: Exp -> Exp
diffTimeBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "DiffTime" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType diffTimeType

-- | TH `Data.Time.LocalTime.TimeOfDay` type representation
timeOfDayName :: Name
timeOfDayName = mkName "QDHXB.Expansions.TimeOfDay"

-- | TH `Data.Time.LocalTime.TimeOfDay` type representation
timeOfDayType :: Type
timeOfDayType = ConT timeOfDayName

-- | TH `Data.Time.LocalTime.TimeOfDay` converter from `String`.
timeOfDayBasicDecoder :: Exp -> Exp
timeOfDayBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "TimeOfDay" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType timeOfDayType

-- | TH `Data.Time.Calendar.OrdinalDate.Day` type representation
dayTypeName :: Name
dayTypeName = mkName "QDHXB.Expansions.Day"

-- | TH `Data.Time.Calendar.OrdinalDate.Day` type representation
dayType :: Type
dayType = ConT dayTypeName

-- | TH `Data.Time.Calendar.OrdinalDate.Day` converter from `String`.
dayBasicDecoder :: Exp -> Exp
dayBasicDecoder expr =
  maybeToExceptExp (qthCouldNotDecodeSimpleType "Day" Nothing) $
    SigE (AppE readMaybeVarE expr) $ appMaybeType dayType

-- | TH `Text.XML.Light.Types.QName` type representation
qnameName :: Name
qnameName = mkName "QName"

-- | TH `Text.XML.Light.Types.QName` type representation
qnameType :: Type
qnameType = ConT qnameName

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

-- | TH `Name` for "y"
yName :: Name
yName = mkName "y"

-- | TH `Name` for "z"
zName :: Name
zName = mkName "z"

-- | TH `Name` for "res"
resName :: Name
resName = mkName "res"

-- | TH `Exp` for "x"
xVarE :: Exp
xVarE = VarE xName

-- | TH `Exp` for "y"
yVarE :: Exp
yVarE = VarE yName

-- | TH `Match` clause that returns whatever it matches.
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
zeroName = mkName "QDHXB.Expansions.Zero"

-- | TH `Pat` for "Zero"
zeroPat :: Pat
zeroPat = ConP zeroName [] []

-- | TH `Pat` for "One"
onePat :: Pat -> Pat
onePat p = ConP oneName [] [p]

-- | TH `Pat` for "Many"
manyPat :: Pat -> Pat
manyPat p = ConP manyName [] [p]

-- | TH `Name` for "One"
oneName :: Name
oneName = mkName "QDHXB.Expansions.One"

-- | TH `Name` for "Many"
manyName :: Name
manyName = mkName "QDHXB.Expansions.Many"

-- | TH `Name` for the re-export of `Text.XML.Light.Types.Content` in
-- `QDHXB.Expansions`.
contentName :: Name
contentName = mkName "QDHXB.Expansions.Content"

-- | TH `Name` for "IO"
ioName :: Name
ioName = mkName "IO"

-- | TH `Name` for "e"
eName :: Name
eName = mkName "e"

-- | TH `Name` for "ctxt"
ctxtName :: Name
ctxtName = mkName "ctxt"

-- | TH `VarE` for "ctxt"
ctxtVarE :: Exp
ctxtVarE = VarE ctxtName

-- | TH `VarP` for "ctxt"
ctxtVarP :: Pat
ctxtVarP = VarP ctxtName

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

-- | TH `Name` for "map"
mapName :: Name
mapName = mkName "map"

-- | TH `Name` for "read"
readName :: Name
readName = mkName "read"

-- | TH `Name` for "readMaybe"
readMaybeName :: Name
readMaybeName = mkName "QDHXB.Expansions.readMaybe"

-- | TH `Name` for `QDHXB.Internal.Utils.XMLLight.zomToList`
zomToListName :: Name
zomToListName = mkName "QDHXB.Expansions.zomToList"

-- | TH `Name` for "a"
aName :: Name
aName = mkName "a"

-- | TH `Exp` for function `read`
readVarE :: Exp
readVarE = VarE readName

-- | TH `Exp` for function `read`
readMaybeVarE :: Exp
readMaybeVarE = VarE readMaybeName

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

-- | TH `Exp` for constructor `Just`
applyJust :: Exp -> Exp
applyJust = AppE justConE

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

-- | TH `Type` for `Text.XML.Light.Types.Content`
contentConT :: Type
contentConT = ConT contentName

-- | TH `Type` for `IO`
ioConT :: Type
ioConT = ConT ioName

-- | TH `Type` for `Maybe`
maybeConT :: Type
maybeConT = ConT maybeName

-- | Given a `Type`, wrap it as an argument to `Maybe`.
appMaybeType :: Type -> Type
appMaybeType t = AppT maybeConT t

-- | TH `Type` for `Show`
showConT :: Type
showConT = ConT showName

-- | TH `Type` for `Eq`
eqConT :: Type
eqConT = ConT eqName

-- | TH `Name` for the `QDHXB.Expansions.Except` re-export of
-- `Control.Monad.Except.Except`.
exceptName :: Name
exceptName = mkName "QDHXB.Expansions.Except"

-- | TH `Type` for `Control.Monad.Except.Except`
exceptConT :: Type
exceptConT = ConT exceptName

-- | Given two TH `Type`s, return the TH `Type` corresponding to apply
-- `Control.Monad.Except.Except` to the two argument types.
applyExceptCon :: Type -> Type -> Type
applyExceptCon excT valT = AppT (AppT exceptConT excT) valT

-- | TH `Name` for "return"
returnName :: Name
returnName = mkName "return"

-- | TH `Exp` for the `returnExp` function
returnExp :: Exp
returnExp = VarE returnName

-- | Given a TH `Exp`, return the TH `Exp` which applies the `return`
-- function to the argument.
applyReturn :: Exp -> Exp
applyReturn = AppE returnExp

-- | TH `Name` for the `QDHXB.Expansions.runExcept` re-export of
-- `Control.Monad.Except.runExcept`.
runExceptName :: Name
runExceptName = mkName "QDHXB.Expansions.runExcept"

-- | TH `Exp` for the `Control.Monad.Except.runExcept` function
runExceptVarE :: Exp
runExceptVarE = VarE runExceptName

-- | Given a TH `Exp`, return the TH `Exp` which applies the
-- `Control.Monad.Except.runExcept` function to the argument
applyRunExceptExp :: Exp -> Exp
applyRunExceptExp = AppE runExceptVarE

-- | TH `Name` for the `QDHXB.Expansions.throwError` re-export of
-- `Control.Monad.Except.throwError`.
throwName :: Name
throwName = mkName "QDHXB.Expansions.throwError"

-- | TH `Exp` for the `Control.Monad.Except.throwError` function
throwVarE :: Exp
throwVarE = VarE throwName

-- | Given a TH `Exp`, return the TH `Stmt` which applies the
-- `Control.Monad.Except.throwError` function to the argument
-- expression as non-binding TH `Stmt`.
applyThrowStmt :: Exp -> Stmt
applyThrowStmt = NoBindS . applyThrowExp

-- | Given a TH `Exp`, return the TH `Exp` which applies the
-- `Control.Monad.Except.throwError` function to the argument.
applyThrowExp :: Exp -> Exp
applyThrowExp = AppE throwVarE

-- | Given a `String`, return a TH non-binding `Stmt` which applies
-- the `Control.Monad.Except.throwError` function to the argument
-- expression.
applyThrowStrStmt :: String -> Stmt
applyThrowStrStmt = NoBindS . applyThrowStrExp

-- | Given a `String`, return a TH `Exp` which applies the
-- `Control.Monad.Except.throwError` function to the argument
-- expression.
applyThrowStrExp :: String -> Exp
applyThrowStrExp = AppE throwVarE . quoteStr

-- | TH `Name` for the `QDHXB.Expansions.catchError` re-export of
-- `Control.Monad.Except.catchError`.
catchErrorName :: Name
catchErrorName = mkName "QDHXB.Expansions.catchError"

-- | TH `Exp` for the catchError`` function
catchErrorVarE :: Exp
catchErrorVarE = VarE catchErrorName

-- | Given two `Exp`ressions corresponding to a monad and an error
-- handler, return a TH `Exp` which applies the
-- `Control.Monad.Except.catchError` function to the two argument
-- expressions.
infixl `applyCatchErrorExp`
applyCatchErrorExp :: Exp -> Exp -> Exp
-- applyCatchErrorExp e1 e2 = AppE (AppE catchErrorVarE e1) e2
applyCatchErrorExp e1 e2 = InfixE (Just e1) catchErrorVarE (Just e2)

-- | Given two expressions of type `Except a`, run the first first and
-- return its result, but if it fails run the other instead.
infixl `replaceOnError`
replaceOnError :: Exp -> Exp -> Exp
replaceOnError e1 e2 = app2Exp catchErrorVarE e1 (LamE [WildP] e2)

-- | TH `Name` for "Left"
leftName :: Name
leftName = mkName "Left"

-- | TH `Name` for "Right"
rightName :: Name
rightName = mkName "Right"

-- | TH `Name` for `fmap`
fmapName :: Name
fmapName = mkName "fmap"

-- | TH `Exp` for `fmap`
fmapVarE :: Exp
fmapVarE = VarE fmapName

-- | TH `Exp` which will throw an exception with the given name.
throwsExc :: String -> Stmt
throwsExc msg = NoBindS $ AppE errorVarE $ quoteStr msg

-- | TH `Exp` which will throw a program-ending error with the message
-- calculated from the given `Exp`ression.
throwsErrorExp :: Exp -> Exp
throwsErrorExp = AppE errorVarE

-- | TH `Exp` which will throw a program-ending error with the given name.
throwsError :: String -> Exp
throwsError = throwsErrorExp . quoteStr

-- | TH `Type` for the one-argument function type of the given
-- argument and result.
fn1Type :: Type -> Type -> Type
fn1Type argT resT = (AppT (AppT ArrowT argT) resT)

-- | TH `Type` for the two-argument function type of the given
-- (curried) arguments and result.
fn2Type :: Type -> Type -> Type -> Type
fn2Type arg1T arg2T resT = fn1Type arg1T $ fn1Type arg2T resT

-- | TH `Exp` for the application of a function to two arguments.
app2Exp :: Exp -> Exp -> Exp -> Exp
app2Exp f e1 e2 = AppE (AppE f e1) e2

-- | TH `Exp` for the application of a function to three arguments.
app3Exp :: Exp -> Exp -> Exp -> Exp -> Exp
app3Exp f e1 e2 e3 = AppE (AppE (AppE f e1) e2) e3

-- | TH `Exp` for the application of a function to four arguments.
app4Exp :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
app4Exp f e1 e2 e3 e4 = AppE (AppE (AppE (AppE f e1) e2) e3) e4

-- | Build a TH @case@ expression over `Either` values with the given
-- bound variable and result expression for respectively the `Left`
-- and `Right` possibilities.
caseLeftRight' :: Exp -> Name -> Exp -> Name -> Exp -> Exp
caseLeftRight' e nl el nr er =
  CaseE e [
    Match (ConP leftName [] [VarP nl]) (NormalB el) [],
    Match (ConP rightName [] [VarP nr]) (NormalB er) []
    ]

-- | Build a TH @case@ expression over `Either` values where we do not
-- care about the `Name` of the bound variable.
caseLeftRight :: Exp -> (Name -> Exp) -> (Name -> Exp) -> Exp
caseLeftRight e elf erf = caseLeftRight' e zName (elf zName) yName (erf yName)

-- | Build a TH @case@ expression over `Maybe` values with the given
-- bound variable and result expression for respectively the `Nothing`
-- and `Just` possibilities (the former having no associated `Name`).
caseNothingJust' :: Exp -> Exp -> Name -> Exp -> Exp
caseNothingJust' e en nj ej =
  CaseE e [
    Match (ConP nothingName [] []) (NormalB en) [],
    Match (ConP justName [] [VarP nj]) (NormalB ej) []
    ]

-- | Build a TH @case@ expression over `Maybe` values where we do not
-- care about the `Name` of the bound variable.
caseNothingJust :: Exp -> Exp -> (Name -> Exp) -> Exp
caseNothingJust e el erf = caseNothingJust' e el zName (erf zName)

-- | Given a TH `Exp` expression of type @Except String a@, either
-- returns a value of type @a@ or throws an `error` with the given
-- `String`
resultOrThrow :: Exp -> Exp
resultOrThrow ex =
  caseLeftRight (applyRunExceptExp ex) (throwsErrorExp . applyBPP . VarE) VarE

--  (`applyCatchErrorExp` (LamE [VarP xName] $ throwsErrorExp (VarE xName)))

-- | Convert a `String` into a Template Haskell `Exp`ression
-- representing that string literal.
quoteStr :: String -> Exp
quoteStr = LitE . StringL

-- | `Name` for the `QDHXB.Expansions.simpleTypeDecoder` function.
simpleTypeDecoderName :: Name
simpleTypeDecoderName = mkName "QDHXB.Expansions.simpleTypeDecoder"

-- | `Name` for the `QDHXB.Expansions.__decodeForSimpleType` function.
simpleTypeDecoderVarE :: Exp
simpleTypeDecoderVarE = VarE simpleTypeDecoderName

-- | `Name` for the `QDHXB.Expansions.spaceSep` re-export of the
-- `QDHXB.Internal.Utils.Misc.spaceSep` function.
spaceSepName :: Name
spaceSepName = mkName "QDHXB.Expansions.spaceSep"

-- | Apply for the
-- `QDHXB.Expansions.spaceSep`/`QDHXB.Internal.Utils.Misc.spaceSep`
-- function.
spaceSepApp :: Exp -> Exp
spaceSepApp = AppE (VarE spaceSepName)

-- | `Name` for the `QDHXB.Expansions.mapM` re-export of the
-- `Control.Monad.mapM` function.
mapMName :: Name
mapMName = mkName "QDHXB.Expansions.mapM"

-- | `Name` for the `QDHXB.Expansions.mapM` re-export of the
-- `Control.Monad.mapM` function.
applyMapM :: Exp -> Exp -> Exp
applyMapM f = AppE (AppE (VarE mapMName) f)

applyBPP :: Exp -> Exp
applyBPP = AppE (VarE $ mkName "QDHXB.Expansions.bpp")

-------------------------------------------------------------------
-- Relating to module Errs ----------------------------------------
------------------------------------------------------------------

-- | Helper for building a quoted `Control.Monad.Except`ion using a
-- constructor taking a `String` and a `Maybe` `Line`.  The first
-- argument is the `String` name of the constructor, which is assumed
-- to be re-exported (and prefixed) by `QDHXB.Expansions`.
qExcStringLoc :: String -> String -> Maybe Line -> Exp
qExcStringLoc conStr sl ll =
  app2Exp (ConE $ mkName $ "QDHXB.Expansions." ++ conStr)
          (quoteStr sl) (quoteLoc ll)

-- | Helper for building a quoted `Control.Monad.Except`ion-thrower
-- using a constructor taking a `String` and a `Maybe` `Line`.  The
-- first argument is the `String` name of the constructor, which is
-- assumed to be re-exported (and prefixed) by `QDHXB.Expansions`.
qthExcStringLoc :: String -> String -> Maybe Line -> Exp
qthExcStringLoc c s l = applyThrowExp $ qExcStringLoc c s l

{-
-- | Helper for building a quoted `Control.Monad.Except`ion-thrower
-- using a constructor taking two `String`s and a `Maybe` `Line`.  The
-- first argument is the `String` name of the constructor, which is
-- assumed to be re-exported (and prefixed) by `QDHXB.Expansions`.
qthExcStrStrLoc :: String -> String -> String -> Maybe Line -> Exp
qthExcStrStrLoc conStr sl1 sl2 ll = applyThrowExp $
  app3Exp (ConE $ mkName $ "QDHXB.Expansions." ++ conStr)
          (quoteStr sl1) (quoteStr sl2) (quoteLoc ll)
-}

-- | Build an expression to throw a `QDHXB.Errs.MiscError` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qthMiscError :: String -> Maybe Line -> Exp
qthMiscError = qthExcStringLoc "MiscError"

-- | Build an expression to throw a `QDHXB.Errs.MiscError` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qMiscError :: String -> Maybe Line -> Exp
qMiscError = qExcStringLoc "MiscError"

-- | Build an expression to throw a `QDHXB.Errs.NoValidContentInUnion`
-- in an `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qthNoValidContentInUnion :: String -> Maybe Line -> Exp
qthNoValidContentInUnion = qthExcStringLoc "NoValidContentInUnion"

-- | Build an expression to throw a `QDHXB.Errs.NoValidContentInUnion`
-- in an `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qNoValidContentInUnion :: String -> Maybe Line -> Exp
qNoValidContentInUnion = qExcStringLoc "NoValidContentInUnion"

-- | Build an expression to throw a `QDHXB.Errs.AtMostOnceIn` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qthAtMostOnceIn :: String -> Maybe Line -> Exp
qthAtMostOnceIn = qthExcStringLoc "AtMostOnceIn"

-- | Build an expression to throw a `QDHXB.Errs.AtMostOnceIn` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qAtMostOnceIn :: String -> Maybe Line -> Exp
qAtMostOnceIn = qExcStringLoc "AtMostOnceIn"

-- | Build an expression to throw a `QDHXB.Errs.MustBePresentIn` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qthMustBePresentIn :: String -> Maybe Line -> Exp
qthMustBePresentIn = qthExcStringLoc "MustBePresentIn"

-- | Build an expression to throw a `QDHXB.Errs.MustBePresentIn` in an
-- `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qMustBePresentIn :: String -> Maybe Line -> Exp
qMustBePresentIn = qExcStringLoc "MustBePresentIn"

-- | Build an expression to throw a `QDHXB.Errs.CrefMustBePresentIn`
-- in an `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qthCrefMustBePresentIn :: String -> Maybe Line -> Exp
qthCrefMustBePresentIn = qthExcStringLoc "CrefMustBePresentIn"

-- | Build an expression to throw a `QDHXB.Errs.CrefMustBePresentIn`
-- in an `Control.Monad.Except` `QDHXB.Errs.HXBErr` @a@ computation.
qCrefMustBePresentIn :: String -> Maybe Line -> Exp
qCrefMustBePresentIn = qExcStringLoc "CrefMustBePresentIn"

-- | Build an expression to throw a
-- `QDHXB.Errs.CouldNotDecodeSimpleType` in an `Control.Monad.Except`
-- `QDHXB.Errs.HXBErr` @a@ computation.
qthCouldNotDecodeSimpleType :: String -> Maybe Line -> Exp
qthCouldNotDecodeSimpleType = qthExcStringLoc "CouldNotDecodeSimpleType"

-- | Build an expression to throw a
-- `QDHXB.Errs.CouldNotDecodeSimpleType` in an `Control.Monad.Except`
-- `QDHXB.Errs.HXBErr` @a@ computation.
qCouldNotDecodeSimpleType :: String -> Maybe Line -> Exp
qCouldNotDecodeSimpleType = qExcStringLoc "CouldNotDecodeSimpleType"

quoteLoc :: Maybe Line -> Exp
quoteLoc Nothing = nothingConE
quoteLoc (Just l) = applyJust $ LitE $ IntegerL l

-- -----------------------------------------------------------------

hxbExceptConT :: Type
hxbExceptConT = ConT $ mkName "QDHXB.Expansions.HXBExcept"

-- | Build the `Type` of an `Control.Monad.Except` computation
-- throwing a `QDHXB.Errs.HXBErr`.  The second argument to
-- `Control.Monad.Except` is not yet supplied.
qHXBExcT :: Type -> Type
qHXBExcT = AppT hxbExceptConT
