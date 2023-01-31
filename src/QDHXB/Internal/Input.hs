
-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (encodeSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.XML.Light.Output
import Text.XML.Light.Types
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.XMLLight
import QDHXB.Internal.NestedTypes
import QDHXB.Internal.XSDQ

-- |Rewrite otherwise-unstructured parsed XML content structures as a
-- sequence of internal XSD representations.
encodeSchemaItems :: [Content] -> XSDQ [DataScheme]
encodeSchemaItems items = do
  res <- mapM encodeSchemaItem items
  -- liftIO $ putStrLn $ show res
  return res

encodeSchemaItem :: Content -> XSDQ DataScheme
encodeSchemaItem (Elem e@(Element (QName "element" _ _) ats content _)) = do
  included <- encodeSchemaItems $ filter isElem content
  typeQName <- pullAttrQName "type" ats
  nameQName <- pullAttrQName "name" ats
  refQName <- pullAttrQName "ref" ats
  let res =
        ElementScheme included nameQName typeQName refQName
                      (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
                      (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
  whenDebugging $ do
    liftIO $ putStrLn $ "> Encoding element"
    -- liftIO $ putStrLn $ mlineIndent "    " (show e)
    liftIO $ putStrLn $ mlineIndent "    " (showElement e)
    liftIO $ bLabelPrintln "  as " res
  return res
encodeSchemaItem (Elem e@(Element (QName "attribute" _ _) ats [] _)) = do
  typeQName <- pullAttrQName "type" ats
  refQName <- pullAttrQName "ref" ats
  nameQname <- pullAttrQName "name" ats
  let res =
        AttributeScheme nameQname typeQName refQName
          (case pullAttr "use" ats of
             Nothing -> "optional"
             Just s -> s)
  whenDebugging $ do
    liftIO $ putStrLn "> Encoding attribute"
    liftIO $ putStrLn $ mlineIndent "    " (showElement e)
    liftIO $ bLabelPrintln "  as " res
  return res
encodeSchemaItem (Elem e@(Element (QName "complexType" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  separateAndDispatchComplexContents ctnts' e ats ifLn
encodeSchemaItem (Elem e@(Element (QName "simpleType" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  case separateSimpleTypeContents ats ctnts' of
    (nam, One restr) -> do
      qnam <- mapM decodePrefixedName nam
      res <- encodeSimpleTypeByRestriction qnam ats restr
      whenDebugging $ do
        liftIO $ putStrLn "> Encoding simpleType "
        liftIO $ putStrLn $ mlineIndent "    " (showElement e)
        liftIO $ bLabelPrintln "  as " res
      return res
    (x, y) -> do
      -- whenDebugging $ do
      liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "CTNTS' " ++ (intercalate "\n    " $ map showContent ctnts')
      liftIO $ putStrLn $ "X " ++ show x
      liftIO $ putStrLn $ "Y " ++ show y
      error $ "TODO encodeSchemaItem > simpleType > another separation case"
        ++ ifAtLine ifLn
encodeSchemaItem (Elem (Element (QName "annotation" _ _) _ _ _)) = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <annotation> element"
  return Skip
encodeSchemaItem (Elem (Element (QName tag _ _) _ _ ifLine)) |
    tag == "include" || tag == "import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine ifLine
  return Skip
encodeSchemaItem (Elem (Element (QName tagname _ _) _ _ _))
  | tagname == "key" || tagname == "keyref" = do
  whenDebugging $ do
    liftIO $ putStrLn $ "> Dropping <" ++ tagname ++ "> entry "
  return Skip
encodeSchemaItem c@(Elem e@(Element (QName "sequence" _ _) ats _ ifLn)) = do
  whenDebugging $ liftIO $ putStrLn "> For <sequence> schema:"
  separateAndDispatchComplexContents [c] e ats ifLn
encodeSchemaItem (Elem (Element (QName "group" _ _) ats ctnts _ifLn)) = do
  whenDebugging $ do
    liftIO $ putStrLn "> For <group> schema:"
  name <- pullAttrQName "name" ats
  liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "NAME " ++ show (fmap showQName name)
  liftIO $ putStrLn $ "CTNTS " ++
    (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  case filter isElem ctnts of
    (Elem (Element (QName "sequence" _ _) _ats _ ifLn')):[] ->
        error $ "TODO encodeSchemaItem > group with sequence" ++ ifAtLine ifLn'
    (Elem (Element (QName "choice" _ _) attrs' ctnts' _ifLn')):[] -> do
      ts <- encodeChoiceTypeScheme name attrs' ctnts'
      return $ Group name $ Just ts
    (Elem (Element (QName "all" _ _) _ats _ ifLn')):[] ->
        error $ "TODO encodeSchemaItem > group with all" ++ ifAtLine ifLn'
    _ -> return $ Group name Nothing
encodeSchemaItem (Elem (Element (QName tag _ _) ats ctnts ifLn)) = do
  whenDebugging $ do
    liftIO $ putStrLn $ "> For <" ++ tag ++ "> element (ZZZ):"
  liftIO $ putStrLn $ "TAG " ++ show tag
  liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "CTNTS " ++ (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  error $ "TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn
encodeSchemaItem (Text _) = do
  whenDebugging $ liftIO $ putStrLn "> Dropping Text entry "
  return Skip
encodeSchemaItem (CRef txt) = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping CRef entry " ++ txt
  return Skip

encodeChoiceTypeScheme ::
  Maybe QName -> [Attr] -> [Content] -> XSDQ TypeScheme
encodeChoiceTypeScheme ifNam attrs allCtnts = do
  let ctnts = filter isElem allCtnts
  liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
  liftIO $ putStrLn $ "IFNAM " ++ show ifNam
  liftIO $ putStrLn $ "CTNTS " ++
    (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  contentSchemes <- mapM encodeSchemaItem ctnts
  return $ Choice ifNam contentSchemes

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = case ifLine of
                    Nothing -> ""
                    Just line -> " at XSD line " ++ show line

separateAndDispatchComplexContents ::
  [Content] -> Element -> [Attr] -> Maybe Line -> XSDQ DataScheme
separateAndDispatchComplexContents contents e ats ifLn =
  case separateComplexTypeContents contents of
    -- <sequence>
    (One intl, Zero, Zero, attrSpecs) -> do
      res <- encodeComplexTypeScheme ats [] intl attrSpecs
      whenDebugging $ do
        liftIO $ putStrLn "> Encoding complexType (case 1)"
        liftIO $ putStrLn $ mlineIndent "    " (showElement e)
        liftIO $ bLabelPrintln "  as " res
      return res
    -- <complexContent>
    (Zero, One ctnt, Zero, attrSpecs) -> do
      res <- encodeComplexTypeScheme ats [] ctnt attrSpecs
      whenDebugging $ do
        liftIO $ putStrLn $ "> Encoding complexType (case 2)"
        liftIO $ putStrLn $ mlineIndent "    " (showElement e)
        liftIO $ bLabelPrintln "  as " res
      return res
    (Zero, Zero, One ctnt, attrSpecs) -> do
      whenDebugging $ do
        liftIO $ putStrLn $ "> Encoding complexType (case 3) EEEEEEEE"
        liftIO $ putStrLn $ "CTNT " ++ ppContent ctnt
        liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
      error $
        "TODO encodeSchemaItem > complexType > simpleContent"
        ++ ifAtLine ifLn
    (seqnce, cplxCtnt, simplCtnt, attrSpecs) -> do
      {- whenDebugging $ do -}
      liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "CTNTS' " ++
        (intercalate "\n    " $ map ppContent $ filter isElem contents)
      liftIO $ bLabelPrintln "SEQ " seqnce
      liftIO $ putStrLn $ "CPLXCTNT " ++
        (intercalate "\n    " $ map ppContent $ filter isElem $ zomToList cplxCtnt)
      liftIO $ putStrLn $ "SIMPLCTNT " ++
        (intercalate "\n    " $ map ppContent $ filter isElem $ zomToList simplCtnt)
      liftIO $ putStrLn $ "ATTRSPECS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem $ zomToList attrSpecs)
      error $
        "TODO encodeSchemaItem > complexType > another separation case"
        ++ ifAtLine ifLn

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
  [Attr] -> [Content] -> Content -> ZeroOneMany Content -> XSDQ DataScheme
encodeComplexTypeScheme ats attrSpecs
                        (Elem (Element (QName tag _ _) ats' ctnts _)) ats'' =
  encodeComplexTypeSchemeElement (ats ++ ats') attrSpecs tag ctnts ats''
encodeComplexTypeScheme ats attrSpecs s ats'' = do
  liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "S " ++ show s
  liftIO $ putStrLn $ "ATS'' " ++ (intercalate "\n    " $ map showContent $ zomToList ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeComplexTypeSchemeElement ::
  [Attr] -> [Content] -> String -> [Content] -> ZeroOneMany Content ->
    XSDQ DataScheme
encodeComplexTypeSchemeElement ats attrSpecs "complexContent" ctnts ats'' =
  case filter isElem ctnts of
    [ctnt] -> encodeComplexTypeScheme ats attrSpecs ctnt ats''
    _ -> error $ "Expected a single child for complexContent node"
encodeComplexTypeSchemeElement ats _ "sequence" ctnts ats'' = do
  -- TODO Do something with the attrSpecs (second) argument.
  included <- encodeSchemaItems ctnts
  atrSpecs <- encodeSchemaItems $ zomToList ats''
  -- liftIO $ putStrLn ">>> encodeComplexTypeScheme"
  nameAttrQName <- pullAttrQName "name" ats
  return $ ComplexTypeScheme (Sequence $ filter nonSkip included) atrSpecs nameAttrQName
encodeComplexTypeSchemeElement ats _ "restriction" _ctnts _ats'' = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  case baseType of
    Nothing -> error "Attribute base required in <restriction> element"
    Just t -> return $ ComplexTypeScheme (Restriction t) [] nameAttr
encodeComplexTypeSchemeElement ats _ "extension" ctnts _ats'' = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  included <- encodeSchemaItems ctnts
  case baseType of
    Nothing -> error "Attribute base required in <extension> element"
    Just t -> return $ ComplexTypeScheme (Extension t included) [] nameAttr
encodeComplexTypeSchemeElement ats attrSpecs tag ctnts ats'' = do
  liftIO $ putStrLn $ "ATS "       ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "TAG "       ++ show tag
  liftIO $ putStrLn $ "CTNTS "     ++ (intercalate "\n    " $ map showContent ctnts)
  liftIO $ putStrLn $ "ATS'' "     ++ (intercalate "\n    " $ map showContent $ zomToList ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeSimpleTypeByRestriction ::
  Maybe QName -> [Attr] -> Content -> XSDQ DataScheme
encodeSimpleTypeByRestriction -- Note ignoring ats
    (Just nam) _ (Elem (Element (QName "restriction" _ _) ats' _ _)) = do
  case pullAttr "base" ats' of
    Just base -> do
      baseQName <- decodePrefixedName base
      return $ SimpleTypeScheme baseQName nam
    Nothing -> error "restriction without base"
encodeSimpleTypeByRestriction ifNam ats s = do
  liftIO $ putStrLn $ ">>> IFNAM " ++ show ifNam
  liftIO $ putStrLn $ ">>> ATS "   ++ (intercalate "\n    " $ map showAttr ats)
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

mlineIndent :: String -> String -> String
mlineIndent ind str = ind ++ (intercalate ("\n" ++ ind) $ lines str)

pullAttrQName :: String -> [Attr] -> XSDQ (Maybe QName)
pullAttrQName str attrs = mapM decodePrefixedName (pullAttr str attrs)
