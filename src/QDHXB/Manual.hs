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
  let schemaReps = encodeSchemaItems forms
  let ir = flattenSchemaItems schemaReps
  let decls = hdecls ir
  return decls
  -- liftIO $ putStrLn $ "======\n" ++ show (filter isElem forms) ++ "\n======"
  -- fmap concat $ mapM formToDecs $ filter isElem forms
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
encodeSchemaItems = concat . map encodeSchemaItem

encodeSchemaItem :: Content -> [SchemeRep]
encodeSchemaItem (Elem (Element (QName "element" _ _) ats content _)) =
  [ ElementScheme (encodeSchemaItems content)
                  (pullAttr "name" ats) (pullAttr "type" ats)
                  (pullAttr "ref" ats)
  ]
encodeSchemaItem (Elem (Element (QName "attribute" _ _) ats [] _)) =
  [ AttributeScheme (pullAttr "name" ats) (pullAttr "type" ats)
                    (pullAttr "ref" ats)
  ]
encodeSchemaItem (Elem (Element (QName "complexType" _ _) ats [internal] _)) =
  encodeComplexTypeScheme ats internal
encodeSchemaItem _ = []

encodeComplexTypeScheme :: [Attr] -> Content -> [SchemeRep]
encodeComplexTypeScheme ats (Elem (Element (QName "sequence" _ _)
                                           ats' seqItems _)) =
  [ ComplexTypeScheme (Sequence $ encodeSchemaItems seqItems)
                      (pullAttr "name" ats) (pullAttr "type" ats)
                      (pullAttr "ref" ats)
  ]

-- -----------------------------------------------------------------

flattenSchemaItems :: [SchemeRep] -> [ItemDefn]
flattenSchemaItems = concat . map flattenSchemaItem

flattenSchemaItem :: SchemeRep -> [ItemDefn]
flattenSchemaItem (ElementScheme contents (Just nam) (Just typ) Nothing) =
  [ SimpleRep nam typ ]
flattenSchemaItem (ElementScheme contents _ _ (Just ref)) =
  error "Reference in top-level decl"
flattenSchemaItem (AttributeScheme (Just nam) (Just typ) Nothing) =
  [ AttributeRep nam typ ]
flattenSchemaItem (AttributeScheme _ _ (Just _)) =
  error "Reference in attribute"
flattenSchemaItem (ComplexTypeScheme typeDetail ifName ifType ifRef) =
  error "TODO"
flattenSchemaItem _ = []
