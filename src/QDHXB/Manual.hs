{-# LANGUAGE TemplateHaskell #-}

-- | Manual translation of an XSD file into the internal @ItemDefn@
-- representation.
module QDHXB.Manual (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
import Text.XML.Light.Types
import QDHXB.Internal
import QDHXB.TH
import QDHXB.XMLLight
import QDHXB.UtilMisc

-- | Convert XML `Content` into a quotation monad returning top-level
-- Haskell declarations.
xmlToDecs :: [Content] -> Q [Dec]
xmlToDecs ((Elem (Element (QName "?xml" _ _) _ _ _))
           : (Elem (Element (QName "schema" _ _) _ forms _))
           : []) = do
  schemaReps <- encodeSchemaItems forms
  -- liftIO $ putStrLn $ "====== REPS\n" ++ show schemaReps ++ "\n======\n"
  ir <- flattenSchemaItems schemaReps
  -- liftIO $ putStrLn $ "====== IR\n" ++ show ir ++ "\n======\n"
  decls <- hdecls ir
  return decls
  -- fmap concat $ mapM formToDecs $ filter isElem forms
xmlToDecs _ = error "Missing <?xml> element"

-- -----------------------------------------------------------------

data TypeSchemeRep = Sequence [SchemeRep]
  | Restriction String -- ^ base
  | Extension String -- ^ base
              [SchemeRep] -- ^ additional
  deriving Show

data SchemeRep =
  ElementScheme [SchemeRep] -- ^ contents
                (Maybe String) -- ^ ifName
                (Maybe String) -- ^ ifType
                (Maybe String) -- ^ ifRef
                (Maybe Int) -- ^ ifMin
                (Maybe Int) -- ^ ifMax
  | AttributeScheme (Maybe String) -- ^ ifName
                    (Maybe String) -- ^ ifType
                    (Maybe String) -- ^ ifRef
  | ComplexTypeScheme TypeSchemeRep -- ^ typeDetail
                      [SchemeRep] -- ^ addlAttrs
                      (Maybe String) -- ^ ifName
  | SimpleTypeScheme String -- ^ baseSpec
                     String -- ^ name
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
encodeSchemaItem (Elem (Element (QName "complexType" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  case separateComplexTypeContents ctnts' of
    -- <sequence>
    (One intl, Zero, Zero, attrSpecs) -> do
      res <- encodeComplexTypeScheme ats [] intl attrSpecs
      return res
    -- <complexContent>
    (Zero, One ctnt, Zero, attrSpecs) -> do
      res <- encodeComplexTypeScheme ats [] ctnt attrSpecs
      return res
      -- error $
      --   "TODO encodeSchemaItem > complexType > other complexContent"
      --   ++ ifAtLine ifLn
    -- <simpleContent>
    (Zero, Zero, One ctnt, attrSpecs) -> do
      liftIO $ putStrLn $ "CTNT " ++ show ctnt
      liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
      error $
        "TODO encodeSchemaItem > complexType > simpleContent"
        ++ ifAtLine ifLn
    (seqnce, cplxCtnt, simplCtnt, attrSpecs) -> do
      liftIO $ putStrLn $ "ATS " ++ show ats
      liftIO $ putStrLn $ "CTNTS' " ++ show ctnts'
      liftIO $ putStrLn $ "SEQ " ++ show seqnce
      liftIO $ putStrLn $ "CPLXCTNT " ++ show cplxCtnt
      liftIO $ putStrLn $ "SIMPLCTNT " ++ show simplCtnt
      liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
      error $
        "TODO encodeSchemaItem > complexType > another separation case"
        ++ ifAtLine ifLn
encodeSchemaItem (Elem (Element (QName "simpleType" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  case separateSimpleTypeContents ats ctnts' of
    (nam, One restr) -> do
      -- liftIO $ putStrLn $ "     <<< " ++ show ats'
      res <- encodeSimpleTypeByRestriction nam ats restr
      -- liftIO $ putStrLn $ "     <<< " ++ show res
      return res
    (x, y) -> do
      liftIO $ putStrLn $ "ATS " ++ show ats
      liftIO $ putStrLn $ "CTNTS' " ++ show ctnts'
      liftIO $ putStrLn $ "X " ++ show x
      liftIO $ putStrLn $ "Y " ++ show y
      error $ "TODO encodeSchemaItem > simpleType > another separation case"
        ++ ifAtLine ifLn
encodeSchemaItem (Elem (Element (QName "annotation" _ _) _ _ _)) = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  return []
encodeSchemaItem (Elem (Element (QName tag _ _) _ _ ifLine)) |
    tag == "include" || tag == "import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine ifLine
  return []
encodeSchemaItem (Elem (Element (QName tag _ _) ats ctnts ifLn)) = do
  liftIO $ putStrLn $ "TAG " ++ show tag
  liftIO $ putStrLn $ "ATS " ++ show ats
  liftIO $ putStrLn $ "CTNTS " ++ show ctnts
  error $ "TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn
encodeSchemaItem (Text _) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem Text"
  return $ []
encodeSchemaItem (CRef _) = do
  -- liftIO $ putStrLn $ ">>>  encodeSchemaItem CRef"
  return $ []

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = case ifLine of
                    Nothing -> ""
                    Just line -> " at line " ++ show line

separateComplexTypeContents ::
  [Content] ->
    (ZeroOneMany Content, ZeroOneMany Content, ZeroOneMany Content,
     ZeroOneMany Content)
separateComplexTypeContents cts =
  (pullContent "sequence" cts,
   pullContent "complexContent" cts,
   pullContent "simpleContent" cts,
   pullContent "attribute" cts)

separateSimpleTypeContents ::
  [Attr] -> [Content] -> (Maybe String, ZeroOneMany Content)
separateSimpleTypeContents attrs cts =
  (pullAttr "name" attrs, pullContent "restriction" cts)

encodeComplexTypeScheme ::
  [Attr] -> [Content] -> Content -> ZeroOneMany Content -> Q [SchemeRep]
encodeComplexTypeScheme ats attrSpecs
                        (Elem (Element (QName tag _ _) ats' ctnts _)) ats'' =
  encodeComplexTypeSchemeElement (ats ++ ats') attrSpecs tag ctnts ats''
encodeComplexTypeScheme ats attrSpecs s ats'' = do
  liftIO $ putStrLn $ "ATS " ++ show ats
  liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "S " ++ show s
  liftIO $ putStrLn $ "ATS'' " ++ show ats''
  error "TODO encodeComplexTypeScheme > another case"

encodeComplexTypeSchemeElement ::
  [Attr] -> [Content] -> String -> [Content] -> ZeroOneMany Content ->
    Q [SchemeRep]
encodeComplexTypeSchemeElement ats attrSpecs "complexContent" ctnts ats'' =
  case filter isElem ctnts of
    [ctnt] -> encodeComplexTypeScheme ats attrSpecs ctnt ats''
    _ -> error $ "Expected a single child for complexContent node"
encodeComplexTypeSchemeElement ats _ "sequence" ctnts ats'' = do
  -- TODO Do something with the attrSpecs (second) argument.
  included <- encodeSchemaItems ctnts
  atrSpecs <- encodeSchemaItems $ zomToList ats''
  -- liftIO $ putStrLn ">>> encodeComplexTypeScheme"
  return [
    ComplexTypeScheme (Sequence included) atrSpecs (pullAttr "name" ats)
    ]
encodeComplexTypeSchemeElement ats attrSpecs tag ctnts ats'' = do
  liftIO $ putStrLn $ "ATS "       ++ show ats
  liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "TAG "       ++ show tag
  liftIO $ putStrLn $ "CTNTS "     ++ show ctnts
  liftIO $ putStrLn $ "ATS'' "     ++ show ats''
  error "TODO encodeComplexTypeScheme > another case"


encodeSimpleTypeByRestriction ::
  Maybe String -> [Attr] -> Content -> Q [SchemeRep]
encodeSimpleTypeByRestriction -- Note ignoring ats
    (Just nam) _ (Elem (Element (QName "restriction" _ _) ats' _ _)) = do
  case pullAttr "base" ats' of
    Just base -> return [ SimpleTypeScheme base nam ]
    Nothing -> error "restriction without base"
encodeSimpleTypeByRestriction ifNam ats s = do
  liftIO $ putStrLn $ ">>> IFNAM " ++ show ifNam
  liftIO $ putStrLn $ ">>> ATS "   ++ show ats
  liftIO $ putStrLn $ ">>> S "     ++ show s
  error "encodeSimpleTypeByRestriction > additional cases"

-- -----------------------------------------------------------------

flattenSchemaItems :: [SchemeRep] -> Q [ItemDefn]
flattenSchemaItems = fmap concat . mapM flattenSchemaItem

flattenSchemaItem :: SchemeRep -> Q [ItemDefn]
flattenSchemaItem (ElementScheme [] (Just nam) (Just typ) Nothing _ _) =
  return [ SimpleRep nam typ ]
flattenSchemaItem (ElementScheme [ComplexTypeScheme (Sequence steps)
                                                    ats Nothing]
                                 (Just nam) Nothing Nothing _ _) =
  assembleComplexSequence steps ats nam
flattenSchemaItem (AttributeScheme (Just nam) (Just typ) Nothing) =
  return [ AttributeRep nam typ ]
flattenSchemaItem (AttributeScheme _ _ (Just _)) =
  return $ error "Reference in attribute"
flattenSchemaItem (ComplexTypeScheme (Sequence cts) ats (Just nam)) =
  assembleComplexSequence cts ats nam
flattenSchemaItem (SimpleTypeScheme base nam) =
  return $ [ SimpleRep nam base ]
flattenSchemaItem s = do
  liftIO $ putStrLn $ ">>> " ++ show s
  error "TODO another flatten case"

assembleComplexSequence ::
  [SchemeRep] ->  [SchemeRep] -> String -> Q [ItemDefn]
assembleComplexSequence steps ats nam = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return $ otherDefs ++ atsDefs ++ [ SequenceRep nam $ atsRefs ++ refs ]

flattenSchemaRefs :: [SchemeRep] -> Q ([ItemDefn], [ItemRef])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: SchemeRep -> Q ([ItemDefn], ItemRef)
flattenSchemaRef (ElementScheme [] Nothing Nothing (Just ref) lower upper) =
  return ([], ElementItem ref lower upper)
flattenSchemaRef (ElementScheme [] (Just nam) (Just typ) Nothing lower upper) =
  return ([SimpleRep nam typ], ElementItem nam lower upper)
flattenSchemaRef s@(ElementScheme [ComplexTypeScheme _ _ Nothing]
                                  (Just nam) Nothing Nothing lower upper) = do
  prev <- flattenSchemaItem s
  return (prev, ElementItem nam lower upper)
flattenSchemaRef (ElementScheme ctnts maybeName maybeType maybeRef
                                lower upper) = do
  liftIO $ putStrLn $ "CONTENTS " ++ show ctnts
  liftIO $ putStrLn $ "IFNAME " ++ show maybeName
  liftIO $ putStrLn $ "IFTYPE " ++ show maybeType
  liftIO $ putStrLn $ "IFREF " ++ show maybeRef
  liftIO $ putStrLn $ "LOWER " ++ show lower
  liftIO $ putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"
flattenSchemaRef (AttributeScheme Nothing Nothing (Just ref)) =
  return ([], AttributeItem ref)
flattenSchemaRef (AttributeScheme (Just nam) (Just typ) Nothing) =
  return ([AttributeRep nam typ], AttributeItem nam)
flattenSchemaRef (AttributeScheme maybeName maybeType maybeRef) = do
  liftIO $ putStrLn $ "IFNAME " ++ show maybeName
  liftIO $ putStrLn $ "IFTYPE " ++ show maybeType
  liftIO $ putStrLn $ "IFREF " ++ show maybeRef
  error "TODO flattenSchemaRef > unmatched AttributeScheme"
flattenSchemaRef (ComplexTypeScheme _ _ _) = -- typeDetail _ maybeName
  error "TODO flattenSchemaRef > ComplexTypeScheme"
flattenSchemaRef s = do
  liftIO $ putStrLn $ "S " ++ show s
  error "TODO flattenSchemaRef > additional case"
