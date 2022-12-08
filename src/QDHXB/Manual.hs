{-# LANGUAGE TemplateHaskell #-}

module QDHXB.Manual (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
import Text.XML.Light.Types
import QDHXB.Internal
import QDHXB.TH
import QDHXB.XMLLight

xmlToDecs :: [Content] -> Q [Dec]
xmlToDecs ((Elem (Element (QName "?xml" _ _) _ _ _))
           : (Elem (Element (QName "schema" _ _) _ forms _))
           : []) = do
  -- liftIO $ putStrLn $ "======\n" ++ show (filter isElem forms) ++ "\n======"
  fmap concat $ mapM formToDecs $ filter isElem forms
xmlToDecs _ = error "Missing <?xml> element"

-- -----------------------------------------------------------------

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

flattenSchemaItems :: [SchemeRep] -> [ItemDefn]
flattenSchemaItems = concat . map flattenSchemaItem

flattenSchemaItem :: SchemeRep -> [ItemDefn]
flattenSchemaItem (ElementScheme contents ifName ifType ifRef) = error "TODO"
flattenSchemaItem (AttributeScheme ifName ifType ifRef) = error "TODO"
flattenSchemaItem (ComplexTypeScheme typeDetail ifName ifType ifRef) = error "TODO"

-- =================================================================

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
