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

xmlToDecs :: [Content] -> Q [Dec]
xmlToDecs ((Elem (Element (QName "?xml" _ _) _ _ _))
           : (Elem (Element (QName "schema" _ _) _ forms _))
           : []) = do
  -- return [ booleanTestBinding (filter isAlpha qn) ]
  liftIO $ putStrLn $ "======\n" ++ show (filter isElem forms) ++ "\n======"
  fmap concat $ mapM formToDecs $ filter isElem forms
xmlToDecs _ = error "Missing <?xml> element"

formToDecs :: Content -> Q [Dec]
formToDecs (Elem (Element (QName "schema" _ _) attrs content _)) =
  decodeSchema attrs content
formToDecs _ = return []


decodeSchema :: [Attr] -> [Content] -> Q [Dec]
decodeSchema ats cts =
  case tryNameType ats of
    Just (nam, typ) -> modelSimpleType nam (decodeTypeAttrVal typ) ats
    Nothing -> do
      liftIO $ putStrLn $ ">>> " ++ show ats
      return []

modelSimpleType :: String -> Type -> [Attr] -> Q [Dec]
modelSimpleType nam baseTyp attrs = do
  -- TODO Decode arity
  liftIO $ putStrLn $ nam ++ " :: " ++ show baseTyp
  return [stringTestBinding nam (show baseTyp)]

decodeTypeAttrVal :: String -> Type
decodeTypeAttrVal "xs:string" = stringType
decodeTypeAttrVal "string" = stringType
decodeTypeAttrVal (c:cs) = ConT $ mkName $ toUpper c : cs

stringType :: Type
stringType = ConT (mkName "String")

tryNameType :: [Attr] -> Maybe (String, String)
tryNameType attrs =
  case pullAttr "name" attrs of
    Just nam -> case pullAttr "type" attrs of
                  Just typ -> Just (nam, typ)
                  Nothing -> Nothing
    Nothing -> Nothing

pullAttr :: String -> [Attr] -> Maybe String
pullAttr _ [] = Nothing
pullAttr nam ((Attr (QName nam' _ _) val) : ats) =
  if nam == nam' then Just val else pullAttr nam ats

isElem :: Content -> Bool
isElem (Elem _) = True
isElem _ = False

{-
booleanTestBinding :: String -> Dec
booleanTestBinding name =
  ValD (VarP $ mkName name) (NormalB $ ConE $ mkName "True") []
-}

stringTestBinding :: String -> String -> Dec
stringTestBinding name val =
  ValD (VarP $ mkName name) (NormalB $ LitE $ StringL val) []
