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
  schemaReps <- encodeSchemaItems forms
  -- liftIO $ putStrLn $ "====== REPS\n" ++ show schemaReps ++ "\n======"
  let ir = flattenSchemaItems schemaReps
  -- liftIO $ putStrLn $ "====== IR\n" ++ show ir ++ "\n======"
  decls <- hdecls ir
  return decls
  -- fmap concat $ mapM formToDecs $ filter isElem forms
xmlToDecs _ = error "Missing <?xml> element"

-- -----------------------------------------------------------------

data ComplexTypeSchemeRep = Sequence [SchemeRep]
  deriving Show

data SchemeRep =
  ElementScheme { contents :: [SchemeRep],
                  ifName :: Maybe String,
                  ifType :: Maybe String,
                  ifRef :: Maybe String,
                  ifMin :: IntOrUnbound,
                  ifMax :: IntOrUnbound }
  | AttributeScheme { ifName :: Maybe String,
                      ifType :: Maybe String,
                      ifRef :: Maybe String }
  | ComplexTypeScheme { typeDetail :: ComplexTypeSchemeRep,
                        addlAttrs :: [SchemeRep],
                        ifName :: Maybe String }
  deriving Show

encodeSchemaItems :: [Content] -> Q [SchemeRep]
encodeSchemaItems items = do
  res <- fmap concat $ mapM encodeSchemaItem items
  -- liftIO $ putStrLn $ show res
  return res

encodeSchemaItem :: Content -> Q [SchemeRep]
encodeSchemaItem (Elem (Element (QName "element" _ _) ats content _)) = do
  included <- encodeSchemaItems $ filter isElem content
  return [
    ElementScheme included
                  (pullAttr "name" ats) (pullAttr "type" ats)
                  (pullAttr "ref" ats)
                  (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
                  (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
    ]
encodeSchemaItem (Elem (Element (QName "attribute" _ _) ats [] _)) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem attribute"
  return [
    AttributeScheme (pullAttr "name" ats) (pullAttr "type" ats)
                    (pullAttr "ref" ats)
    ]
encodeSchemaItem (Elem (Element (QName "complexType" _ _) ats ctnts _)) = do
  let ctnts' = filter isElem ctnts
  case separateComplexTypeContents ctnts' of
    (One intl, ats') -> do
      -- liftIO $ putStrLn $ "     <<< " ++ show ats'
      res <- encodeComplexTypeScheme ats intl ats'
      -- liftIO $ putStrLn $ "     <<< " ++ show res
      return res
encodeSchemaItem (Elem (Element (QName tag _ _) ats ctnts _)) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem Element " ++ tag
  -- liftIO $ putStrLn $ ">>>    " ++ show ctnts
  return $ [] -- TODO explore
encodeSchemaItem (Text _) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem Text"
  return $ []
encodeSchemaItem (CRef _) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem CRef"
  return $ []

separateComplexTypeContents ::
  [Content] -> (ZeroOneMany Content, ZeroOneMany Content)
separateComplexTypeContents cts =
  (pullContent "sequence" cts, pullContent "attribute" cts)

encodeComplexTypeScheme ::
  [Attr] -> Content -> ZeroOneMany Content -> Q [SchemeRep]
encodeComplexTypeScheme ats
                        (Elem (Element (QName "sequence" _ _) ats' items _))
                        ats'' = do
  included <- encodeSchemaItems items
  atrSpecs <- encodeSchemaItems $ zomToList ats''
  -- liftIO $ putStrLn ">>> encodeComplexTypeScheme"
  return [
    ComplexTypeScheme (Sequence included) atrSpecs (pullAttr "name" ats)
    ]

-- -----------------------------------------------------------------

flattenSchemaItems :: [SchemeRep] -> [ItemDefn]
flattenSchemaItems = concat . map flattenSchemaItem

flattenSchemaItem :: SchemeRep -> [ItemDefn]
flattenSchemaItem (ElementScheme [] (Just nam) (Just typ) Nothing _ _) =
  [ SimpleRep nam typ ]
flattenSchemaItem (ElementScheme [ComplexTypeScheme (Sequence steps)
                                                    ats Nothing]
                                 (Just nam) Nothing Nothing _ _) =
  let (otherDefs, refs) = flattenSchemaRefs steps
      (atsDefs, atsRefs) = flattenSchemaRefs ats
  in otherDefs ++ atsDefs ++ [ SequenceRep nam $ atsRefs ++ refs ]
flattenSchemaItem (AttributeScheme (Just nam) (Just typ) Nothing) =
  [ AttributeRep nam typ ]
flattenSchemaItem (AttributeScheme _ _ (Just _)) =
  error "Reference in attribute"
flattenSchemaItem (ComplexTypeScheme (Sequence _) _ (Just name)) =
  error "TODO flattenSchemaItem ComplexTypeScheme"
flattenSchemaItem _ = error "TODO 1"

flattenSchemaRefs :: [SchemeRep] -> ([ItemDefn], [ItemRef])
flattenSchemaRefs = applyFst concat . unzip . map flattenSchemaRef

flattenSchemaRef :: SchemeRep -> ([ItemDefn], ItemRef)
flattenSchemaRef (ElementScheme [] Nothing Nothing (Just ref) lower upper) =
  ([], ElementItem ref lower upper)
flattenSchemaRef (ElementScheme contents ifName ifType ifRef lower upper) =
  error "TODO a"
flattenSchemaRef (AttributeScheme Nothing Nothing (Just ref)) =
  ([], AttributeItem ref)
flattenSchemaRef (AttributeScheme ifName ifType ifRef) = error "TODO b"
flattenSchemaRef (ComplexTypeScheme typeDetail _ ifName) = error "TODO c"

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)
applySnd :: (a -> b) -> (c, a) -> (c, b)
applySnd f (x,y) = (x, f y)
