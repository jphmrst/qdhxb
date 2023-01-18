{-# LANGUAGE TemplateHaskell #-}

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (encodeSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import Text.XML.Light.Types
import QDHXB.Internal.Utils.XMLLight
import QDHXB.Internal.NestedTypes
import QDHXB.Internal.XSDQ

encodeSchemaItems :: [Content] -> XSDQ [SchemeRep]
encodeSchemaItems items = do
  res <- fmap concat $ mapM encodeSchemaItem items
  -- liftIO $ putStrLn $ show res
  return res

encodeSchemaItem :: Content -> XSDQ [SchemeRep]
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
      (case pullAttr "use" ats of
         Nothing -> "optional"
         Just s -> s)
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
  [Attr] -> [Content] -> Content -> ZeroOneMany Content -> XSDQ [SchemeRep]
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
    XSDQ [SchemeRep]
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
  Maybe String -> [Attr] -> Content -> XSDQ [SchemeRep]
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

-- | Decode the `String` representation of an XSD integer as a Haskell
-- `Int`.  Might fail, so the result is `Maybe`-wrapped.
decodeIntOrUnbound :: String -> Maybe Int
decodeIntOrUnbound "unbounded" = Nothing
decodeIntOrUnbound s = Just $ read s

-- | Another decoder of the `String` representation of an XSD integer
-- as a Haskell `Int`, where there may be no `String` in the first
-- place.
decodeMaybeIntOrUnbound1 :: Maybe String -> Maybe Int
decodeMaybeIntOrUnbound1 Nothing = Just 1
decodeMaybeIntOrUnbound1 (Just s) = decodeIntOrUnbound s
