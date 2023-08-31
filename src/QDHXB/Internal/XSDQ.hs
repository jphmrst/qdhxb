{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
module QDHXB.Internal.XSDQ (
  -- * Internal state definition
  QdxhbState, initialQdxhbState,
  -- * Monad transformer
  XSDQ, runXSDQ, liftIOtoXSDQ, liftQtoXSDQ, liftStatetoXSDQ, liftExcepttoXSDQ,
  -- * Information about declarations
  fileNewDefinition, anyTypeQName,
  -- ** Elements
  addElementType, getElementType, getElementTypeOrFail,
  -- ** Attributes
  addAttributeType, getAttributeType, getAttributeDefn, getAttributeTypeOrFail,
  getAttributeOrGroup, adjustTypeForUsage, getAttributeOrGroupTypeForUsage,
  -- ** Attribute groups
  getAttributeGroup, buildAttrOrGroupHaskellName, buildAttrOrGroupHaskellType,
  -- ** Groups
  addGroupDefn, getGroupDefn, getGroupDefnOrFail,
  -- ** Types
  addTypeDefn, getTypeDefn, isKnownType, ifKnownType, isSimpleType,
  isComplexType, getTypeHaskellName, getTypeHaskellType,
  getTypeDecoderAsName, getTypeSafeDecoderAsName,
  addUsedTypeName, typeNameIsInUse,
  -- ** Name freshening
  freshenStringForBinding, freshenQNameForBinding,

  -- *** XML and XSD primitive types
  installXmlPrimitives, installXsdPrimitives,
  -- * Configuration options
  getOptions, getUseNewtype, getDebugging, whenDebugging, ifDebuggingDoc,

  -- * Managing namespaces
  pushNamespaces, popNamespaces,
  getNamespaces, getDefaultNamespace, inDefaultNamespace, useNameOrWrap,
  decodePrefixedName, decodePrefixedNameList, getURIprefix, getNextCapName,

  -- * Debugging output
  indenting, dbgLn, dbgPt, dbgBlock, dbgBLabel, dbgBLabelPt,
  boxed, debugXSDQ,
  dbgResult, dbgResultSrcDest,
  dbgBLabelFn1, dbgBLabelFn2, dbgBLabelSrcDest,
  dbgResultFn1, dbgResultFn2, dbgResultM,

  -- * Logging
  resetLog, localLoggingStart, localLoggingEnd,
  whenResetLog, whenLogging, whenLocalLogging, putLog,

  -- * Miscellaneous
  NameStore, containForBounds)
where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Data.Time.Clock
import Data.Time.Format
import Text.XML.Light.Types
import Text.XML.Light.Output (showQName)
import QDHXB.Utils.BPP (
  Blockable, block, stringToBlock, labelBlock, stack2, bpp, bprintLn, follow)
import QDHXB.Utils.Debugln (
  Debugln, runDebugln, MonadDebugln, getIndentation,
  fileLocalDebuglnSubject, fileLocalDebuglnCall)
import QDHXB.Utils.DebuglnBlock (
  -- fileLocalDebuglnBlockSubject,
  fileLocalDebuglnBlockCall)
import QDHXB.Utils.Namespaces
import QDHXB.Utils.Misc
import QDHXB.Utils.XMLLight (inSameNamspace)
import QDHXB.Utils.TH (
  timeOfDayBasicDecoder, stringListBasicDecoder, stringBasicDecoder,
    intBasicDecoder, dayBasicDecoder, diffTimeBasicDecoder, floatBasicDecoder,
    doubleBasicDecoder, zonedTimeBasicDecoder, boolBasicDecoder,
    qnameBasicDecoder, appMaybeType,
    stringType, boolType, floatType, doubleType, intType,
    diffTimeType, dayType, zonedTimeConT, timeOfDayType,
    stringListType, qnameType, srcName, destName,
    firstToUpper)
import QDHXB.Internal.Types
import QDHXB.Options
import qualified QDHXB.Utils.Debugln as DL
import qualified QDHXB.Utils.DebuglnBlock as DLB

-- | Synonym for an association list from a `String` to the argument
-- type.
type NameStore a = [(String, a)]

-- | Synonym for an association list from a `QName` to the argument
-- type.
type QNameStore a = [(QName, a)]

-- | The type of the internal state of an `XSDQ` computation, tracking
-- the names of XSD entities and their definitions.
data QdxhbState = QdxhbState {
  stateOptions :: QDHXBOptionSet,
  stateElementTypes :: (QNameStore QName),
  stateAttributeTypes :: (QNameStore AttributeDefn),
  stateAttributeGroups :: (QNameStore AttributeDefn),
  stateTypeDefinitions :: (QNameStore Definition),
  stateGroupDefinitions :: (QNameStore Definition),
  stateDefaults :: [Maybe String],
  stateNamespacesList :: [Namespaces],
  stateCapitalizedNames :: [String],
  stateIndentation :: String,
  stateResetLog :: Bool,
  stateLocalLog :: (Maybe String),
  stateNextDisambig :: Int,
  stateDisambigString :: String,
  stateUsedTypeNames :: [String]
  }

instance Blockable QdxhbState where
  block st =
    stringToBlock "Current state"
    `stack2` (labelBlock "- " $ block $ stateOptions st)
    `stack2` (let elemTypes = stateElementTypes st
              in if null elemTypes
                 then stringToBlock "- No element types"
                 else (labelBlock "- Element types: " $
                        foldr1 stack2
                             (map (\(qn,defn) ->
                                      labelBlock (bpp qn ++ " -> ") $
                                        block defn)
                                elemTypes)))
    `stack2` (let attrTypes = stateAttributeTypes st
               in if null attrTypes
                  then stringToBlock "- No attribute types"
                  else (labelBlock "- Attribute types: " $
                         foldr1 stack2
                           (map (\(qn,defn) ->
                                  labelBlock (bpp qn ++ " -> ") $ block defn)
                             attrTypes)))
    `stack2` (let attrGroups = stateAttributeGroups st
               in if null attrGroups
                  then stringToBlock "- No attribute groups"
                  else (labelBlock "- Attribute groups: " $
                         foldr1 stack2
                           (map (\(qn,defn) ->
                                  labelBlock (bpp qn ++ " -> ") $ block defn)
                             attrGroups)))
    `stack2` (let typeDefns = stateTypeDefinitions st
              in if null typeDefns
                 then stringToBlock "- No type defs"
                 else (labelBlock "- Type defs: " $
                        foldr1 stack2
                          (map (\(qn,defn) ->
                                  (labelBlock (bpp qn ++ " -> ") $ block defn)
                                  `stack2`
                                  ((stringToBlock "  decoder ")))
                             typeDefns)))
    `stack2` (let groupDefns = stateGroupDefinitions st
              in if null groupDefns
                 then stringToBlock "- No group defs"
                 else (labelBlock "- Group defs: " $
                        foldr1 stack2
                          (map (\(qn,defn) ->
                                  (labelBlock (bpp qn ++ " -> ") $ block defn)
                                  `stack2`
                                  ((stringToBlock "  decoder ")))
                             groupDefns)))
    `stack2` (stringToBlock $ "- Indentation \"" ++ stateIndentation st ++ "\"")
    `stack2` (stringToBlock $ "- Resetting logs: " ++ (show $ stateResetLog st))
    `stack2` (stringToBlock $ "- Local log \""
              ++ maybe "(inactive)" id (stateLocalLog st) ++ "\"")
    `stack2` (stringToBlock $ "- File logging "
              ++ maybe "inactive" (const "active") (stateLocalLog st))

-- | The base URL for the XML/XSD specification
baseXmlNamespace :: String
baseXmlNamespace = "http://www.w3.org/2001/XMLSchema"

fileLocalDebuglnSubject "xsdq" ["indenting"]
fileLocalDebuglnCall "xsdq" 0 ["dbgLn", "dbgPt", "boxed"]
fileLocalDebuglnBlockCall "xsdq" 0 [
  "dbgBlock", "dbgBLabel", "dbgBLabelPt", "dbgBLabelFn1", "dbgBLabelFn2",
  "dbgResult", "dbgResultM", "dbgResultFn1", "dbgResultFn2"]

-- | The initial value of `XSDQ` states.
initialQdxhbState :: QDHXBOption -> QdxhbState
initialQdxhbState optsF =
  let opts = optsF defaultOptionSet
      initNamespaces = map (\x -> [(x, baseXmlNamespace)]) $
        optXmlNamespacePrefixes opts
  in QdxhbState {
    stateOptions = opts,
    stateElementTypes = [],
    stateAttributeTypes = [],
    stateAttributeGroups = [],
    stateTypeDefinitions = [],
    stateGroupDefinitions = [],
    stateDefaults = [],
    stateNamespacesList = initNamespaces,
    stateCapitalizedNames = nameBodies,
    stateIndentation = "",
    stateResetLog = optResetLogging opts,
    stateLocalLog = optLogToFile opts,
    stateNextDisambig = 1,
    stateDisambigString = "X",
    stateUsedTypeNames = []
    }

-- | Monadic type for loading and interpreting XSD files, making
-- definitions available after they are loaded.
newtype XSDQ a = XSDQ (ExceptT String (StateT QdxhbState (Debugln Q)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Lift a computation from the `ExceptT` layer to the `XSDQ` wrapper.
liftExcepttoXSDQ :: ExceptT String (StateT QdxhbState (Debugln Q)) a -> XSDQ a
liftExcepttoXSDQ = XSDQ

-- | Lift a computation from the `StateT` layer to the `XSDQ` wrapper.
liftStatetoXSDQ :: StateT QdxhbState (Debugln Q) a -> XSDQ a
liftStatetoXSDQ = XSDQ . lift

-- | Lift a computation from the `Q` monad to the `XSDQ` wrapper.
liftDebuglnToXSDQ :: Debugln Q a -> XSDQ a
liftDebuglnToXSDQ = XSDQ . lift . lift
instance MonadDebugln XSDQ Q where liftDebugln = liftDebuglnToXSDQ

-- | Lift a computation from the `Q` monad to the `XSDQ` wrapper.
liftQtoXSDQ :: Q a -> XSDQ a
liftQtoXSDQ = XSDQ . lift . lift . lift

-- | Lift a computation from the `IO` monad to the `XSDQ` wrapper.
liftIOtoXSDQ :: IO a -> XSDQ a
liftIOtoXSDQ = XSDQ . lift . lift . lift . liftIO

instance MonadError String XSDQ where
  throwError e = liftExcepttoXSDQ $ throwError e
  catchError (XSDQ p) h =
    liftExcepttoXSDQ $ catchError p (\e -> case h e of XSDQ h' -> h')

-- | Provide the `newName` function from the internal `Q` layer to
-- top-level `XSDQ` computations.
instance Quote XSDQ where
  newName = liftQtoXSDQ . newName

-- | Run an `XSDQ` monad, exposing the underlying `Q` computation.
runXSDQ :: QDHXBOption -> XSDQ a -> Q a
runXSDQ optsF (XSDQ m) = do
  resEither <- runDebugln (evalStateT (runExceptT m) $ initialQdxhbState optsF)
                          True [] "| " -- TODO Pull from optsF, not hardcode
  case resEither of
    Left errStr -> error errStr
    Right a -> return a

-- | Transform a Haskell type representation based on the (possibly
-- absent) lower and upper bounds of an XSD type constraint.
containForBounds :: Maybe Int -> Maybe Int -> XSDQ Type -> XSDQ Type
containForBounds (Just 0) (Just 0) _ = [t|()|]
containForBounds (Just 0) (Just 1) t = [t|Maybe $t|]
containForBounds Nothing (Just 1) t = [t|Maybe $t|]
containForBounds (Just 1) (Just 1) t = t
containForBounds _ _ t = [t|[$t]|]

-- | Register a `Definition` with the tracking tables in the `XSDQ`
-- state.
fileNewDefinition :: Definition -> XSDQ ()
fileNewDefinition d@(SimpleSynonymDefn qn _ _ _) = addTypeDefn qn d
fileNewDefinition (AttributeDefn qn defn@(SingleAttributeDefn _ _) _ _)  = do
  addAttributeType qn defn
fileNewDefinition (AttributeDefn qn defn@(AttributeGroupDefn _) _ _)  = do
  addAttributeGroup qn defn
fileNewDefinition d@(SequenceDefn qn _ _ _)   = addTypeDefn qn d
fileNewDefinition d@(UnionDefn qn _ _ _) = addTypeDefn qn d
fileNewDefinition d@(ListDefn qn _ _ _) = addTypeDefn qn d
fileNewDefinition (ElementDefn n t _ _ _)  = do
  whenDebugging $ do
    ind <- getIndentation
    liftIO $ putStrLn $ show $
      (labelBlock (ind ++ "Filing ElementDefn: ") $ block n)
       `follow` (labelBlock " :: " $ block t)
  addElementType n t
fileNewDefinition d@(ComplexSynonymDefn qn _ _ _) = addTypeDefn qn d
fileNewDefinition d@(ChoiceDefn qn _ _ _) = addTypeDefn qn d
fileNewDefinition d@(ExtensionDefn qn _ _ _ _) = addTypeDefn qn d
fileNewDefinition d@(GroupDefn qn _ _ _) = addGroupDefn qn d
fileNewDefinition d@(BuiltinDefn qn _ _ _) = do
  whenDebugging $ do
    ind <- getIndentation
    liftIO $ putStrLn $ show $
      (labelBlock (ind ++ "Filing BuiltinDefn: ") $ block qn)
       -- `follow` (labelBlock " :: " $ block t)
  addTypeDefn qn d

-- | Retrieve the `QName` of the standard XSD type @anyType@.
anyTypeQName :: XSDQ QName
anyTypeQName = do
  st <- liftStatetoXSDQ $ get
  return $ get_anyType $ stateTypeDefinitions st
  where get_anyType ((qn, BuiltinDefn (QName "anyType" _ _) _ _ _):_) = qn
        get_anyType (_:env) = get_anyType env
        get_anyType [] = error "Bad call to anyTypeQName"

-- | Write the current state of the `XSDQ` internals to the standard
-- output.
debugXSDQ :: XSDQ ()
debugXSDQ = do
  st <- liftStatetoXSDQ $ get
  liftIO $ bprintLn st

-- | Adds descriptions of XSD primitive types to the `XSDQ`
-- environment.
installXmlPrimitives :: String -> Maybe String -> XSDQ ()
installXmlPrimitives ns pfx = do
  fileNewDefinition $
    BuiltinDefn (QName "lang" (Just ns) pfx) "String"
      stringType stringBasicDecoder

-- | Adds descriptions of XSD primitive types to the `XSDQ`
-- environment.
installXsdPrimitives :: String -> Maybe String -> XSDQ ()
installXsdPrimitives ns pfx = do
  let anyTypeQN = QName "anyType" (Just ns) pfx
  fileNewDefinition $
    BuiltinDefn anyTypeQN "String" stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "anySimpleType" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "anyAtomicType" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "anyURI" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "boolean" (Just ns) pfx) "Bool"
      boolType boolBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "date" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "dateTime" (Just ns) pfx) "QDHXB.Expansions.ZonedTime"
      zonedTimeConT zonedTimeBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "decimal" (Just ns) pfx) "Double"
      doubleType doubleBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "double" (Just ns) pfx) "Double"
      doubleType doubleBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "duration" (Just ns) pfx) "QDHXB.Expansions.DiffTime"
      diffTimeType diffTimeBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "float" (Just ns) pfx) "Float"
      floatType floatBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "hexBinary" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "gDay" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "gMonth" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "gMonthDay" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "gYear" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "gYearMonth" (Just ns) pfx) "QDHXB.Expansions.Day"
      dayType dayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "NOTATION" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "QName" (Just ns) pfx) "QDHXB.Expansions.QName"
      qnameType qnameBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "positiveInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "integer" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "long" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "int" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "short" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "byte" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "nonNegativeInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "positiveInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "unsignedInt" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "unsignedShort" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "unsignedByte" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "positiveInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "nonPositiveInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "negativeInteger" (Just ns) pfx) "Int"
      intType intBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "string" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "normalizedString" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "token" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "language" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "Name" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "NCName" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "ENTITY" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "ID" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "IDREF" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "NMTOKEN" (Just ns) pfx) "String"
      stringType stringBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "time" (Just ns) pfx) "QDHXB.Expansions.TimeOfDay"
      timeOfDayType timeOfDayBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "ENTITIES" (Just ns) pfx) "StringList"
      stringListType stringListBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "IDREFS" (Just ns) pfx) "StringList"
      stringListType stringListBasicDecoder
  fileNewDefinition $
    BuiltinDefn (QName "MNTOKENS" (Just ns) pfx) "StringList"
      stringListType stringListBasicDecoder

-- | Register the `Definition` of an XSD type with the tracking tables
-- in the `XSDQ` state.
addTypeDefn :: QName -> Definition -> XSDQ ()
addTypeDefn name defn = liftStatetoXSDQ $ do
  st <- get
  put $ st { stateTypeDefinitions = ((name, defn) : stateTypeDefinitions st) }

-- | Return the `Definition` of an XSD type from the tracking tables
-- in the `XSDQ` state.
getTypeDefn :: QName -> XSDQ (Maybe Definition)
getTypeDefn name = liftStatetoXSDQ $ do
  st <- get
  return $ lookupFirst (stateTypeDefinitions st) name

-- | Register the `Definition` of an XSD group with the tracking tables
-- in the `XSDQ` state.
addGroupDefn :: QName -> Definition -> XSDQ ()
addGroupDefn name defn = liftStatetoXSDQ $ do
  st <- get
  put $ st { stateGroupDefinitions = ((name, defn) : stateGroupDefinitions st) }

-- | Return the `Definition` of an XSD group from the tracking tables
-- in the `XSDQ` state.
getGroupDefn :: QName -> XSDQ (Maybe Definition)
getGroupDefn name = liftStatetoXSDQ $ do
  st <- get
  return $ lookupFirst (stateGroupDefinitions st) name

-- | Return the `Definition` of an XSD group from the tracking tables
-- in the `XSDQ` state.
getGroupDefnOrFail :: QName -> XSDQ Definition
getGroupDefnOrFail name = do
  ifGroup <- getGroupDefn name
  case ifGroup of
    Just gr -> return gr
    Nothing -> error $ "No such group " ++ showQName name

-- | If the argument names an XSD type known to the translator, then
-- return the `String` name of the corresponding Haskell type (and
-- throw on error in the `XSDQ` monad otherwise).
getTypeHaskellName :: QName -> XSDQ String
getTypeHaskellName qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $
      "No type \"" ++ bpp qn ++ "\" found"
    Just defn -> return $ case defn of
                            BuiltinDefn _ hn _ _ -> hn
                            _ -> firstToUpper $ qName qn

-- | If the argument names an XSD type known to the translator, then
-- return the the corresponding TH `Type` (and throw on error in the
-- `XSDQ` monad otherwise).
getTypeHaskellType :: QName -> XSDQ Type
getTypeHaskellType qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> return $ case defn of
                            BuiltinDefn _ _ ht _ -> ht
                            _ -> ConT $ mkName $ firstToUpper $ qName qn

-- | If the argument names an XSD attribute group known to the
-- translator, then return the `String` name of the corresponding
-- Haskell type (and throw on error in the `XSDQ` monad otherwise).
buildAttrOrGroupHaskellName :: QName -> XSDQ String
buildAttrOrGroupHaskellName qn = return $ firstToUpper $ qName qn

-- | If the argument names an XSD attribute group known to the
-- translator, then return the the corresponding TH `Type` (and throw
-- on error in the `XSDQ` monad otherwise).
buildAttrOrGroupHaskellType :: QName -> XSDQ Type
buildAttrOrGroupHaskellType = fmap (ConT . mkName) . buildAttrOrGroupHaskellName

-- | Build the @"tryDecodeAs"@-prefixed name for the XSD type named by
-- the argument `QName`.  __NOTE__: this function will not necessarily
-- exist; it's just an operation on the `QName` contents.
getTypeSafeDecoderAsName :: QName -> XSDQ Name
getTypeSafeDecoderAsName =
  return . mkName . ("tryDecodeAs" ++) . firstToUpper . qName

-- | Build the @"decodeAs"@-prefixed name for the XSD type named by
-- the argument `QName`.  __NOTE__: this function will not necessarily
-- exist; it's just an operation on the `QName` contents.
getTypeDecoderAsName :: QName -> XSDQ Name
getTypeDecoderAsName =
  return . mkName . ("decodeAs" ++) . firstToUpper . qName

-- | Return `True` if the string argument names an XSD type known to
-- the tracking tables in the `XSDQ` state.
isKnownType :: QName -> XSDQ Bool
isKnownType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just _ -> True
    _ -> False

-- | Boolean test based on `isKnownType`.
ifKnownType :: QName -> XSDQ a -> XSDQ a -> XSDQ a
{-# INLINE ifKnownType #-}
ifKnownType str thenM elseM = do
  isKnown <- isKnownType str
  if isKnown then thenM else elseM

-- | Return `True` if the string argument names a simple XSD type
-- known to the tracking tables in the `XSDQ` state.
isSimpleType :: QName -> XSDQ Bool
isSimpleType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just (SimpleSynonymDefn _ _ _ _) -> True
    _ -> False

-- | Return `True` if the string argument names a complex XSD type
-- known to the tracking tables in the `XSDQ` state.
isComplexType :: QName -> XSDQ Bool
isComplexType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just (SequenceDefn _ _ _ _) -> True
    _ -> False

-- | Register the type name associated with an element tag with the
-- tracking tables in the `XSDQ` state.
addElementType :: QName -> QName -> XSDQ ()
addElementType name typ = liftStatetoXSDQ $ do
  st <- get
  put $ st { stateElementTypes = (name, typ) : stateElementTypes st }

-- | Get the type name associated with an element tag from the
-- tracking tables in the `XSDQ` state, or `Nothing` if there is no
-- such element name.
getElementType :: QName -> XSDQ (Maybe QName)
getElementType name = liftStatetoXSDQ $ do
  st <- get
  return $ lookupFirst (stateElementTypes st) name

-- | Get the type name associated with an element tag from the
-- tracking tables in the `XSDQ` state, throwing an error if there is
-- no such element name.
getElementTypeOrFail :: QName -> XSDQ QName
getElementTypeOrFail name = do
  typM <- getElementType name
  case typM of
    Just typ -> return typ
    Nothing -> error $ "Undefined element " ++ show name

-- | Register the type name associated with an attribute tag with the
-- tracking tables in the `XSDQ` state.
addAttributeType :: QName -> AttributeDefn -> XSDQ ()
addAttributeType name typ = liftStatetoXSDQ $ do
  st <- get
  put $ st { stateAttributeTypes = (name, typ) : stateAttributeTypes st }

-- | Get the type name associated with an attribute tag from the
-- tracking tables in the `XSDQ` state, or `Nothing` if there is no
-- such attribute name.
getAttributeDefn :: QName -> XSDQ (Maybe AttributeDefn)
getAttributeDefn name = liftStatetoXSDQ $ do
  st <- get
  return $ lookupFirst (stateAttributeTypes st) name

-- | Get the type name associated with an attribute tag from the
-- tracking tables in the `XSDQ` state, or `Nothing` if there is no
-- such attribute name.
getAttributeType :: QName -> XSDQ (Maybe QName)
getAttributeType name = do
  defn <- getAttributeDefn name
  case defn of
    Just (SingleAttributeDefn t _) -> return $ Just t
    Just (AttributeGroupDefn _) -> throwError $
      "Single attribute type requested for " ++ qName name ++
      " but group definition found"
    Nothing -> throwError $ "No attribute information for " ++ qName name

-- | Return the `Definition` of an XSD attribute group from the tracking tables
-- in the `XSDQ` state.
getAttributeGroup :: QName -> XSDQ (Maybe AttributeDefn)
getAttributeGroup name = liftStatetoXSDQ $ do
  st <- get
  return $ lookupFirst (stateAttributeGroups st) name

-- | Given the base type for an attribute, return the type
-- corresponding to a particular mode of usage for that attribute.
adjustTypeForUsage :: AttributeUsage -> Type -> Type
adjustTypeForUsage Forbidden _ = TupleT 0
adjustTypeForUsage Optional t = appMaybeType t
adjustTypeForUsage Required t = t

-- | Return the Haskell type of an attribute or attribute group in the
-- context of a particular usage declaration.
getAttributeOrGroupTypeForUsage :: (QName, AttributeUsage) -> XSDQ Type
getAttributeOrGroupTypeForUsage (qn, _usage) = do
  whenDebugging $ dbgBLabel "[gAoGTfU @XSDQ] for " qn
  ifDefn <- getAttributeOrGroup qn
  case ifDefn of
    Nothing -> throwError $ "No attribute or group " ++ show qn
    Just defn -> do
      typ <- indenting $ buildAttrOrGroupHaskellType qn
      whenDebugging $ dbgBLabel "- typ " typ
      case defn of
        SingleAttributeDefn _ usage -> do
          whenDebugging $ dbgBLabel "- defn Single attr with usage " usage
          dbgResult "Returns type:" $ adjustTypeForUsage usage typ
        AttributeGroupDefn _subAttrs {- usage -} -> do
          whenDebugging $ dbgLn "- defn Attr group, assuming Optional"
          dbgResult "Returns type as-is:" $ typ

-- | Return the `Definition` of an XSD attribute or attribute group
-- from the tracking tables in the `XSDQ` state.
getAttributeOrGroup :: QName -> XSDQ (Maybe AttributeDefn)
getAttributeOrGroup name = indenting $ do
  whenDebugging $ dbgBLabel "[XSDQ.getAttributeOrGroup] " name
  ifSingle <- getAttributeDefn name
  case ifSingle of
    Just _ -> dbgResult "- returns" ifSingle
    Nothing -> dbgResultM "- returns" $ getAttributeGroup name

-- | Register the name associated with an attribute group
-- @AttributeDefn@.
addAttributeGroup :: QName -> AttributeDefn -> XSDQ ()
addAttributeGroup name defn = liftStatetoXSDQ $ do
  st <- get
  put $ st { stateAttributeGroups = (name, defn) : stateAttributeGroups st }

-- | Get the type name associated with an attribute tag from the
-- tracking tables in the `XSDQ` state, throwing an error if there is
-- no such attribute name.
getAttributeTypeOrFail :: QName -> XSDQ QName
getAttributeTypeOrFail name = do
  typM <- getAttributeType name
  case typM of
    Just typ -> return typ
    Nothing -> error $ "Undefined attribute " ++ show name

-- |Return the QDHXBOptionSet in effect for this run.
getOptions :: XSDQ (QDHXBOptionSet)
getOptions = liftStatetoXSDQ $ do
  st <- get
  return $ stateOptions st

-- |Return whether @newtype@ should be used in this run.
getUseNewtype :: XSDQ Bool
getUseNewtype = fmap optUseNewType getOptions

-- |Return whether debugging output should be generated in this run.
getDebugging :: XSDQ Bool
getDebugging = fmap optDebugging getOptions

-- |Guard a block executed only when debugging is activated.
whenDebugging :: XSDQ () -> XSDQ ()
whenDebugging = whenM getDebugging

-- |Execute one of two blocks depending on whether documentation
-- debugging is selected.
ifDebuggingDoc :: XSDQ () -> XSDQ () -> XSDQ ()
ifDebuggingDoc yes no = ifM getDebugging yes no

-- |Create a new namespace scope on the `XSDQ` state corresponding to
-- the attributes, presumably from an XSD element schema.
pushNamespaces :: [Attr] -> XSDQ ()
pushNamespaces attrs = do
  st <- liftStatetoXSDQ $ get
  liftStatetoXSDQ $ do
    let (thisNS, thisDft) = decodeAttrsForNamespaces attrs
    put $ st { stateDefaults = thisDft : stateDefaults st,
               stateNamespacesList = thisNS : stateNamespacesList st }
  nss <- getNamespaces
  let addXml = optAddXmlBindings $ stateOptions st

  -- If we use the XSD namespace, then load in the builting type
  -- definitions.
  forM_ nss $ \ns -> do
    forM_ ns $ \(p,u) -> do
      when (u == "http://www.w3.org/2001/XMLSchema") $ do
        when addXml $ installXmlPrimitives u (Just p)
        installXsdPrimitives u (Just p)

  whenDebugging $ do
    dft <- getDefaultNamespace
    liftIO $ do
      putStrLn $ "Default namespace: " ++ show dft
      putStrLn $ "Namespaces:"
    forM_ nss $ \ns ->
      forM_ ns $ \(p,u) ->
        liftIO $ putStrLn $ p ++ " --> " ++ u

-- |Remove a namespace scope, corrsponding to finishing the processing
-- of a file.
popNamespaces :: XSDQ ()
popNamespaces = liftStatetoXSDQ $ do
  st <- get
  case stateNamespacesList st of
    [] -> return ()
    (_:nss') -> put $ st { stateNamespacesList = nss' }

-- |Return the stack of `Namespaces` currently known to the `XSDQ`
-- state.
getNamespaces :: XSDQ [Namespaces]
getNamespaces = liftStatetoXSDQ $ do
  st <- get
  return $ stateNamespacesList st

-- |Return the stack of `Namespaces` currently known to the `XSDQ`
-- state.
getCurrentNamespaces :: XSDQ (Maybe Namespaces)
getCurrentNamespaces = do
  nss <- getNamespaces
  return $ case nss of
             [] -> Nothing
             ns:_ -> Just ns

-- |Return the current target namespace URI.
getDefaultNamespace :: XSDQ (Maybe String)
getDefaultNamespace = liftStatetoXSDQ $ do
  st <- get
  getFirstActual $ stateDefaults st
    where getFirstActual [] = return Nothing
          getFirstActual (r@(Just _) : _) = return r
          getFirstActual (Nothing : ds) = getFirstActual ds

-- |Where the argument gives the core name string, return a `QName`
-- based in the default namespace of the computation.
inDefaultNamespace :: String -> XSDQ QName
inDefaultNamespace s = do
  uri <- getDefaultNamespace
  ns <- getCurrentNamespaces
  let prefix = case uri of
                 Nothing -> Nothing
                 Just us -> case ns of
                   Nothing -> Nothing
                   Just n -> lookupPrefixForURI us n
  return $ QName s uri prefix

-- |If the first argument does carry a `QName` then return it, else
-- build a new fully contextually-qualified name.
useNameOrWrap :: Maybe QName -> String -> String -> XSDQ QName
useNameOrWrap ifName outer dft =
  maybe (inDefaultNamespace $ outer ++ dft) return ifName

-- |Given a string which may be namespace-prefixed, reconstruct the
-- corresponding `QName` according to the current `XSDQ` state.
decodePrefixedName :: String -> XSDQ QName
decodePrefixedName str = do
  nss <- getNamespaces
  dft <- getDefaultNamespace
  let dftPrefix :: Maybe String
      dftPrefix = compressMaybe $
        fmap (\s -> getFirst lookupPrefixForURI s nss) dft
  return $ decodePrefixed dftPrefix dft [] nss str

-- |Given a string which may be namespace-prefixed, reconstruct the
-- corresponding `QName` according to the current `XSDQ` state.
decodePrefixedNameList :: String -> XSDQ [QName]
decodePrefixedNameList str = mapM decodePrefixedName $ words str

getFirst ::
  (String -> Namespaces -> Maybe String) -> String -> [Namespaces] ->
    Maybe String
getFirst _ _ [] = Nothing
getFirst fn val (ns:nss) = case fn val ns of
                             Nothing -> getFirst fn val nss
                             res -> res

-- |Given the string of a URI, return the associated prefix in the
-- current namespaces.
getURIprefix :: String -> XSDQ (Maybe String)
getURIprefix uri = do
  nss <- getNamespaces
  return $ getFirst lookupPrefixForURI uri nss

lookupPrefixForURI :: String -> Namespaces -> Maybe String
lookupPrefixForURI _ [] = Nothing
lookupPrefixForURI target ((p,u):_) | target == u = Just p
lookupPrefixForURI target (_:ns) = lookupPrefixForURI target ns

-- ------------------------------------------------------------

lookupFirst :: Eq k => [(k, a)] -> k -> Maybe a
lookupFirst [] _ = Nothing
lookupFirst ((fnd, x):_) targ | fnd == targ = Just x
lookupFirst (_:xs) targ = lookupFirst xs targ

-- | Return the next generated capitalized name from the `XSDQ`
-- monad's list.  TODO --- these names are not yet checked for
-- uniqueness with respect to the names already in use.
getNextCapName :: XSDQ String
getNextCapName = liftStatetoXSDQ $ do
  st <- get
  let (z, zs) = case stateCapitalizedNames st of
                  x:xs -> (x, xs)
                  [] -> error "Capitalized names should not run out"
  put $ st { stateCapitalizedNames = zs }
  return $ "Z__" ++ z

nameBodies :: [String]
nameBodies = basicNamePool ++ (concat $ map (\n -> map (n ++) basicNamePool) basicNamePool)

basicNamePool :: [String]
basicNamePool = map (\x -> [x]) ['a'..'z']

-- ------------------------------------------------------------

-- |Given a computation result which is a function of two arguments
-- corresponding to source and destination, emit debugging information
-- about it if debugging mode is on.
dbgBLabelSrcDest ::
  Blockable c => String -> (Name -> Name -> c) -> XSDQ ()
{-# INLINE dbgBLabelSrcDest #-}
dbgBLabelSrcDest msg = dbgBLabelFn2 msg srcName destName

-- |Given a computation result which is a function of two arguments
-- corresponding to source and destination, emit debugging information
-- about it if debugging mode is on.
dbgResultSrcDest ::
  Blockable c => String -> (Name -> Name -> c) -> XSDQ (Name -> Name -> c)
{-# INLINE dbgResultSrcDest #-}
dbgResultSrcDest msg = dbgResultFn2 msg srcName destName

-- | Run statements when log files should be reset at each run.
whenResetLog :: XSDQ () -> XSDQ ()
whenResetLog m = do
  st <- liftStatetoXSDQ get
  if stateResetLog st then m else return ()

-- | Run statements when any kind of logging is selected.
whenLogging :: (String -> XSDQ ()) -> XSDQ ()
whenLogging m = do
  whenLocalLogging m

-- | Run statements when per-XSD logging is selected.
whenLocalLogging :: (String -> XSDQ ()) -> XSDQ ()
whenLocalLogging m = do
  st <- liftStatetoXSDQ get
  case stateLocalLog st of
    Just f -> m f
    Nothing -> return ()

-- | Run statements when per-XSD logging is selected.
localLoggingStart :: XSDQ ()
localLoggingStart = do
  st <- liftStatetoXSDQ get
  case stateLocalLog st of
    Just logfile -> do
      whenResetLog $ resetLog logfile
    Nothing -> return ()

-- | Run statements when per-XSD logging is selected.
localLoggingEnd :: XSDQ ()
localLoggingEnd = return ()

-- | Write a `String` to any selected logging method.
putLog :: String -> XSDQ ()
putLog s = do
  st <- liftStatetoXSDQ get
  writeIf $ stateLocalLog st
  where writer file = liftIO $ appendFile file s
        writeIf = maybe (return ()) writer

-- | Reset a log file.
resetLog :: String -> XSDQ ()
resetLog file = liftIO $ do
  now <- getCurrentTime
  writeFile file ""
  appendFile file $
    "QDHXB "
    ++ formatTime defaultTimeLocale "%Y/%m/%d %T %Z" now
    ++ " -*- mode: text -*-\n"

-- | Check if a type name is already in use.
typeNameIsInUse :: String -> XSDQ Bool
typeNameIsInUse s = do
  st <- liftStatetoXSDQ $ get
  return $ elem s (stateUsedTypeNames st)

-- | Note another in-use type name.
addUsedTypeName :: String -> XSDQ ()
addUsedTypeName s = do
  st <- liftStatetoXSDQ $ get
  liftStatetoXSDQ $ put $
    st { stateUsedTypeNames = s : stateUsedTypeNames st }

-- | Master name freshening function for `String` results
freshenStringForBinding :: Maybe String -> Maybe QName -> String -> XSDQ String
freshenStringForBinding ifOuter _ifOrigQName core = do
  inUse <- typeNameIsInUse core
  if not inUse
    then do
      addUsedTypeName core
      return core
    else case ifOuter of
           Just outer -> getBindingName $ outer ++ core
           Nothing -> getBindingName core

-- | Master name freshening function for `QName` results
freshenQNameForBinding :: Maybe String -> QName -> XSDQ QName
freshenQNameForBinding ifOuter orig = do
  let core = qName orig
  inUse <- typeNameIsInUse core
  if not inUse
    then do
      addUsedTypeName core
      return orig
    else do
      core' <- case ifOuter of
        Just outer -> getBindingName $ outer ++ core
        Nothing -> getBindingName core
      return $ inSameNamspace core' orig

-- | Add disambiguating suffixes to a `String` until it is unique,
-- registering the result as used in a binding.
getBindingName :: String -> XSDQ String
getBindingName given = do
  -- let orig = firstToUpper given
  inUse <- typeNameIsInUse given -- orig
  if inUse
    then do possible <- disambigString given -- orig
            getBindingName possible
    else return given -- orig

{-
-- | TODO (Exactly how to rename, test here if actually needed?)
-- Return a previously-unused type name for an outer wrapper, and
-- (possibly) a given `QName`.
freshenTypeName :: String -> Maybe QName -> XSDQ String
freshenTypeName outer ifName =
  return $ maybe outer id $ fmap qName ifName

-- | Return a previously-unused type name for an outer wrapper, and
-- (possibly) a given `QName`.
freshenTypeQName :: String -> Maybe QName -> XSDQ QName
freshenTypeQName outer ifName = do
  strName <- freshenTypeName outer ifName
  inDefaultNamespace strName
-}

-- | Return the next number to be used for disambiguating type names.
getNextDisambig :: XSDQ Int
getNextDisambig = do
  st <- liftStatetoXSDQ get
  let result = stateNextDisambig st
  liftStatetoXSDQ $ put $ st { stateNextDisambig = 1 + result }
  return result

-- | Get the `String` to be prepended to the disambiguation number
-- when creating distinct type and constructor names.
getDisambigString :: XSDQ String
getDisambigString = fmap stateDisambigString $ liftStatetoXSDQ get

-- | Use the state's separator string and disambiguation number to
-- append a disambiguator to the argument string.  This function does
-- no checking on the result.
disambigString :: String -> XSDQ String
disambigString str = do
  sep <-getDisambigString
  num <- getNextDisambig
  return $ str ++ sep ++ show num
