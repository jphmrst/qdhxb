{-# LANGUAGE TemplateHaskell #-}

module QDHXB (qdhxb) where

import Language.Haskell.TH
-- import System.Directory
import System.IO
import Control.Monad.IO.Class
import Data.Char
import Text.XML.Light.Types
import Text.XML.Light.Input

qdhxb :: [String] -> Q [Dec]
qdhxb xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  fmap concat $ mapM loadFile xsds

loadFile :: String -> Q [Dec]
loadFile xsdFile = do
  xsd <- liftIO $ readFile' xsdFile
  let xml = parseXML xsd
  xmlToDecs $ filter isElem xml
  -- fmap concat $ mapM

-- -----------------------------------------------------------------

data IntOrUnbound = Bound Int | Unbounded
decodeIntOrUnbound :: String -> IntOrUnbound
decodeIntOrUnbound "unbounded" = Unbounded
decodeIntOrUnbound s = Bound $ read s

data ComplexTypeSchemeRep = Sequence [SchemeRep]

data SchemeRep =
  ElementScheme { contents :: [SchemeRep],
                  ifName :: Maybe String,
                  ifType :: Maybe String,
                  ifRef :: Maybe String }
  | AttributeScheme { ifName :: Maybe String,
                      ifType :: Maybe String,
                      ifRef :: Maybe String }
  | ComplexTypeScheme { typeDetail :: ComplexTypeSchemeRep,
                        ifName :: Maybe String,
                        ifType :: Maybe String,
                        ifRef :: Maybe String }

encodeSchemaItems :: [Content] -> [SchemeRep]
encodeSchemaItems = map encodeSchemaItem

encodeSchemaItem :: Content -> SchemeRep
encodeSchemaItem (Elem (Element (QName "element" _ _) ats content _)) =
  ElementScheme (encodeSchemaItems content)
                (pullAttr "name" ats) (pullAttr "type" ats) (pullAttr "ref" ats)
encodeSchemaItem (Elem (Element (QName "attribute" _ _) ats [] _)) =
  AttributeScheme (pullAttr "name" ats) (pullAttr "type" ats)
                  (pullAttr "ref" ats)
encodeSchemaItem (Elem (Element (QName "complexType" _ _) ats [internal] _)) =
  encodeComplexTypeScheme ats internal

encodeComplexTypeScheme :: [Attr] -> Content -> SchemeRep
encodeComplexTypeScheme ats (Elem (Element (QName "sequence" _ _) ats' seqItems _)) =
  ComplexTypeScheme (Sequence $ encodeSchemaItems seqItems)
                    (pullAttr "name" ats) (pullAttr "type" ats)
                    (pullAttr "ref" ats)

-- -----------------------------------------------------------------

data ItemRef =
  ElementItem String
  | AttributeItem String
  | ComplexTypeItem String

data ItemDefn =
  SimpleRep String String
  | AttributeRep String String
  | SequenceRep String [ItemRef]

flattenSchemaItem :: [SchemeRep] -> [ItemDefn]
flattenSchemaItem = error "TODO"

-- -----------------------------------------------------------------

xmlToDecs :: [Content] -> Q [Dec]
xmlToDecs ((Elem (Element (QName "?xml" _ _) _ _ _))
           : (Elem (Element (QName "schema" _ _) _ forms _))
           : []) = do
  -- liftIO $ putStrLn $ "======\n" ++ show (filter isElem forms) ++ "\n======"
  fmap concat $ mapM formToDecs $ filter isElem forms
xmlToDecs _ = error "Missing <?xml> element"

formToDecs :: Content -> Q [Dec]
formToDecs (Elem (Element (QName tag _ _) attrs content _)) =
  unpackItemRep tag attrs content
formToDecs _ = return []

unpackItemRep :: String -> [Attr] -> [Content] -> Q [Dec]
unpackItemRep tag ats cts = do
  decodeElement tag (filter isElem cts)
    (pullAttr "name" ats) (pullAttr "type" ats) (pullAttr "ref" ats)

decodeElement ::
  String -> [Content] -> Maybe String -> Maybe String -> Maybe String
    -> Q [Dec]
decodeElement tag cts (Just nam) (Just typ) Nothing
  | tag == "element" || tag == "attribute"
  = return [
      TySynD (mkName $ firstToUpper nam ++ "Type") [] (decodeTypeAttrVal typ)
      ]
decodeElement tag cts ifName ifType ifRef = do
  liftIO $ putStrLn $ ">>> Tag " ++ tag
  liftIO $ putStrLn $ ">>> Name " ++ show ifName
  liftIO $ putStrLn $ ">>> Type " ++ show ifType
  liftIO $ putStrLn $ ">>> Ref " ++ show ifRef
  liftIO $ putStrLn $ ">>> Contents " ++ show cts
  return []

modelSimpleType :: String -> Type -> [Attr] -> Q [Dec]
modelSimpleType nam baseTyp attrs = do
  -- TODO Decode arity
  liftIO $ putStrLn $ nam ++ " :: " ++ show baseTyp
  return [stringTestBinding nam (show baseTyp)]
  where stringTestBinding :: String -> String -> Dec
        stringTestBinding name val =
          ValD (VarP $ mkName name) (NormalB $ LitE $ StringL val) []

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

pullAttr :: String -> [Attr] -> Maybe String
pullAttr _ [] = Nothing
pullAttr nam ((Attr (QName nam' _ _) val) : ats) =
  if nam == nam' then Just val else pullAttr nam ats

isElem :: Content -> Bool
isElem (Elem _) = True
isElem _ = False

firstToUpper :: String -> String
firstToUpper "" = ""
firstToUpper (c:cs) = toUpper c : cs

{-
booleanTestBinding :: String -> Dec
booleanTestBinding name =
  ValD (VarP $ mkName name) (NormalB $ ConE $ mkName "True") []
-}

