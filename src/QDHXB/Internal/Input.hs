-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (encodeSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.Read (readMaybe)
import Text.XML.Light.Output
import Text.XML.Light.Types
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.ZeroOneMany
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
encodeSchemaItem (Elem (Element qname attrs contents ifLine)) =
  encodeElement qname attrs contents ifLine
encodeSchemaItem (Text _) = do
  whenDebugging $ liftIO $ putStrLn "> Dropping Text entry "
  return Skip
encodeSchemaItem (CRef txt) = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping CRef entry " ++ txt
  return Skip

encodeElement :: QName -> [Attr] -> [Content] -> Maybe Line -> XSDQ DataScheme
encodeElement (QName "element" _ _) ats content _ = do
  included <- encodeSchemaItems $ filter isElem content
  typeQName <- pullAttrQName "type" ats
  nameQName <- pullAttrQName "name" ats
  refQName <- pullAttrQName "ref" ats
  let res =
        ElementScheme included nameQName typeQName refQName
                      (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
                      (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
  whenDebugging $ liftIO $ do
    putStrLn $ "> Encoding element attrs="
      ++ intercalate ", " (map showAttr ats)
    putStrLn $ ">                  content="
      ++ intercalate ", " (map showContent content)
    bLabelPrintln "  as " res
  return res
encodeElement q@(QName "attribute" _ _) a c l = do
  scheme <- encodeAttribute q a c l
  return $ ComplexTypeScheme scheme [] $ Just q
encodeElement q@(QName "attributeGroup" _ _) a c l = do
  scheme <- encodeAttribute q a c l
  return $ ComplexTypeScheme scheme [] $ Just q
encodeElement q@(QName "complexType" _ _) ats ctnts l = do
  let ctnts' = filter isElem ctnts
  separateAndDispatchComplexContents ctnts' (Element q ats ctnts l) ats l
encodeElement (QName "simpleType" _ _) ats ctnts ifLn = do
  let ctnts' = filter isElem ctnts
  whenDebugging $ liftIO $ do
    putStrLn $ "> Encoding simpleType attrs="
      ++ intercalate ", " (map showAttr ats)
    putStrLn $ "                      contents="
      ++ intercalate ", " (map showContent ctnts)
  case separateSimpleTypeContents ats ctnts' of
    (nam, One restr, Zero, Zero) -> do
      qnam <- mapM decodePrefixedName nam
      res <- encodeSimpleTypeByRestriction qnam ats restr
      whenDebugging $ liftIO $ bLabelPrintln "  as " res
      return res
    (Just nam, Zero, One (Elem (Element (QName "union" _ _) _ cs' _)), Zero) ->
      do qnam <- decodePrefixedName nam
         alts <- encodeSchemaItems $ filter isElem cs'
         let res = SimpleTypeScheme (Just qnam) $ Union alts
         whenDebugging $ liftIO $ do
           putStrLn $ "> Encoding simpleType attrs="
             ++ intercalate ", " (map showAttr ats)
           putStrLn $ "                      content="
             ++ intercalate ", " (map showContent ctnts)
           bLabelPrintln "  as " res
         return res
    (ifNam, Zero, Zero,
     One (Elem (Element (QName "list" _ _) ats' _ _))) -> do
      itemTypeAttr <- pullAttrQName "itemType" ats'
      case itemTypeAttr of
        Nothing -> error "Simple type list without itemType attribute"
        Just itemType -> do
          qnam <- case ifNam of
            Just n -> decodePrefixedName n
            Nothing -> return $ QName ("List_" ++ qName itemType)
                                      (qURI itemType) (qPrefix itemType)
          let res = SimpleTypeScheme (Just qnam) $
                      List (Just itemType)
          whenDebugging $ liftIO $ do
            putStrLn $ "> Encoding simpleType attrs="
              ++ intercalate ", " (map showAttr ats)
            putStrLn $ "                      content="
              ++ intercalate ", " (map showContent ctnts)
            bLabelPrintln "  as " res
          return res
    (ifName, zomRestr, zomUnion, zomList) -> do
      -- whenDebugging $
      liftIO $ do
        putStrLn "+------"
        putStrLn $
          "| TODO encodeSchemaItem > simpleType > another separation case"
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| CTNTS' "
          ++ (intercalate "\n    " $ map showContent ctnts')
        putStrLn $ "| IFNAME " ++ show ifName
        putStrLn $ "| ZOMRESTR " ++ (show $ zomToList zomRestr)
        putStrLn $ "| ZOMUNION " ++ (show $ zomToList zomUnion)
        putStrLn $ "| ZOMLIST "  ++ (show zomList)
      error $ "TODO encodeSchemaItem > simpleType > another separation case"
        ++ ifAtLine ifLn
encodeElement (QName "annotation" _ _) _ _ _ = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <annotation> element"
  return Skip
encodeElement (QName tag _ _) _ _ ifLine |
     tag == "include" || tag == "import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine ifLine
  return Skip
encodeElement (QName tag _ _) _ _ _ | tag == "key" || tag == "keyref" = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <" ++ tag ++ "> entry "
  return Skip
encodeElement q@(QName "sequence" _ _) ats ctnts ifLn = do
  whenDebugging $ liftIO $ putStrLn "> For <sequence> schema:"
  separateAndDispatchComplexContents [Elem $ Element q ats ctnts ifLn]
                                     (Element q ats ctnts ifLn) ats ifLn
encodeElement (QName "group" _ _) ats ctnts _ifLn = do
  whenDebugging $ liftIO $ putStrLn "> For <group> schema:"
  name <- pullAttrQName "name" ats
  case filter isElem ctnts of
    (Elem (Element (QName "sequence" _ _) _ats _ ifLn')):[] -> do
      liftIO $ putStrLn $ "- ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "- NAME " ++ show (fmap showQName name)
      liftIO $ putStrLn $ "- CTNTS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO encodeSchemaItem > group with sequence" ++ ifAtLine ifLn'
    (Elem (Element (QName "choice" _ _) attrs' ctnts' _ifLn')):[] -> do
      ts <- encodeChoiceTypeScheme name attrs' ctnts'
      whenDebugging $ liftIO $
        bLabelPrintln
          ("- result is Group " ++ show (fmap showQName name) ++ " ")
          ts
      return $ Group name $ Just ts
    (Elem (Element (QName "all" _ _) _ats _ ifLn')):[] -> do
      liftIO $ putStrLn $ "+-------"
      liftIO $ putStrLn $
        "| TODO encodeSchemaItem > group with all" ++ ifAtLine ifLn'
      liftIO $ putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "| NAME " ++ show (fmap showQName name)
      liftIO $ putStrLn $ "| CTNTS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO encodeSchemaItem > group with all" ++ ifAtLine ifLn'
    _ -> do
      liftIO $ putStrLn $ "- Default is group of nothing"
      return $ Group name Nothing
encodeElement (QName tag _ _) ats ctnts ifLn = do
  -- whenDebugging $
  liftIO $ do
    putStrLn $ "+-------"
    liftIO $ putStrLn $
      "| TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn
    liftIO $ putStrLn $ "| TAG " ++ show tag
    liftIO $ putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
    liftIO $ putStrLn $
      "| CTNTS " ++ (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  error $ "TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn


encodeChoiceTypeScheme ::
  Maybe QName -> [Attr] -> [Content] -> XSDQ ComplexTypeScheme
encodeChoiceTypeScheme ifNam _attrs allCtnts = do
  let ctnts = filter isElem allCtnts
  {-
  whenDebugging $ liftIO $ do
    putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
    putStrLn $ "IFNAM " ++ show ifNam
    putStrLn $ "CTNTS " ++
      (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  -}
  contentSchemes <- mapM encodeSchemaItem ctnts
  return $ Choice ifNam contentSchemes

encodeAttributeScheme :: Content -> XSDQ AttributeScheme
encodeAttributeScheme (Elem (Element q a c l)) =
  fmap ComplexTypeScheme (encodeAttribute q a c l) [] q

encodeAttribute ::
  QName -> [Attr] -> [Content] -> Maybe Line -> XSDQ ComplexTypeScheme
encodeAttribute q@(QName "attribute" _ _) ats [] _ = do
  typeQName <- pullAttrQName "type" ats
  refQName <- pullAttrQName "ref" ats
  nameQname <- pullAttrQName "name" ats
  let res = SingleAttribute nameQname refQName typeQName
                            (case pullAttr "use" ats of
                               Nothing -> "optional"
                               Just s -> s)
  whenDebugging $ liftIO $ do
    putStrLn $ "> Encoding attribute" ++ showQName q
    bLabelPrintln "    attrs " ats
    putStrLn      "    no contents"
  return res
encodeAttribute (QName "attributeGroup" _ _) ats ctnts l
  = do name <- pullAttrQName "name" ats
       ref <- pullAttrQName "ref" ats
       let attrs = filterTagged "attribute" ctnts
           atGroups = filterTagged "attributeGroup" ctnts
       subcontents <- mapM encodeAttributeScheme $ attrs ++ atGroups
       let res = AttributeGroup name ref subcontents
       whenDebugging $ liftIO $ do
         putStrLn $ "> encodeSchemaItem <attributeGroup>" ++ ifAtLine l
         putStrLn $ "  attributes " ++ (intercalate "\n    " $ map showAttr ats)
         putStrLn $ "  contents " ++
           (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
         liftIO $ bLabelPrintln "    --> " res
       return res
encodeAttribute q _ _ _ =
  error $ "Can't use encodeAttributeScheme with a " ++ bpp q

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = case ifLine of
                    Nothing -> ""
                    Just line -> " at XSD line " ++ show line

separateAndDispatchComplexContents ::
  [Content] -> Element -> [Attr] -> Maybe Line -> XSDQ DataScheme
separateAndDispatchComplexContents contents e ats ifLn =
  case separateComplexTypeContents ats contents of
    -- <sequence>
    (_, One intl, Zero, Zero, attrSpecs, _) -> do
      res <- encodeComplexTypeScheme ats [] intl attrSpecs
      whenDebugging $ liftIO $ do
        putStrLn "  > Encoding complexType (case 1)"
        putStrLn $ mlineIndent "      " (showElement e)
        bLabelPrintln "    as " res
      return res
    -- <complexContent>
    (_, Zero, One ctnt, Zero, attrSpecs, _) -> do
      res <- encodeComplexTypeScheme ats [] ctnt attrSpecs
      whenDebugging $ liftIO $ do
        putStrLn $ "  > Encoding complexType (case 2)"
        putStrLn $ mlineIndent "      " (showElement e)
        bLabelPrintln "    as " res
      return res
    (Just typeStr, Zero, Zero, Zero, [attrSpec], []) -> do
          scheme <- fmap AttributeScheme $ encodeAttributeScheme attrSpec
          typeName <- decodePrefixedName typeStr
          let res = ComplexTypeScheme (ComplexSynonym scheme) [] $ Just typeName
          whenDebugging $ liftIO $ do
            putStrLn "  > Encoding complexType (case 3, one attribute only)"
            bLabelPrintln "    as " res
          return res
    (Just typeStr, Zero, Zero, Zero, [], [attrGroup]) -> do
          scheme <- fmap AttributeScheme $ encodeAttributeScheme attrGroup
          typeName <- decodePrefixedName typeStr
          let res = ComplexTypeScheme (Sequence []) [scheme] $ Just typeName
          whenDebugging $ liftIO $ do
            putStrLn "  > Encoding complexType (case 4, one group only)"
            bLabelPrintln "    attrs were " ats
            bLabelPrintln "    as " res
          return res
    (Just typeStr, Zero, Zero, Zero, attrSpecs, attrGroups)
      | length attrSpecs > 0 || length attrGroups > 0 -> do
          let allAttr :: [Content]
              allAttr = attrSpecs ++ attrGroups
          scheme <- fmap (AttributeScheme . AttributeGroup Nothing Nothing) $
                      mapM encodeAttributeScheme allAttr
          typeName <- decodePrefixedName typeStr
          let res = ComplexTypeScheme (ComplexSynonym scheme) [] $ Just typeName
          whenDebugging $ liftIO $ do
            putStrLn "  > Encoding complexType (case 5, attributes and groups)"
            bLabelPrintln "    as " res
          return res
    (_, Zero, Zero, One ctnt, attrSpecs, _) -> do
      -- whenDebugging $
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $ "| Encoding complexType (case EEEEEEEE)"
        putStrLn $ "| CTNT " ++ ppContent ctnt
        putStrLn $ "| ATTRSPECS " ++ show attrSpecs
      error $ "TODO encodeSchemaItem > complexType > simpleContent"
        ++ ifAtLine ifLn
    (nameAttr, seqnce, cplxCtnt, simplCtnt, attrSpecs, attrGroups) -> do
      {- whenDebugging $ -}
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $
          "| Encoding complexType (case FFF) another complexType separation"
        putStrLn $ "| NAMEATTR " ++ show nameAttr
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| CTNTS' " ++
          (intercalate "\n    " $ map ppContent $ filter isElem contents)
        bLabelPrintln "| SEQ " seqnce
        putStrLn $ "| CPLXCTNT " ++
          (zomintercalate "\n    " $ zommap ppContent $
             zomfilter isElem cplxCtnt)
        putStrLn $ "| SIMPLCTNT " ++
          (zomintercalate "\n    " $ zommap ppContent $
             zomfilter isElem simplCtnt)
        putStrLn $ "| ATTRSPECS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem attrSpecs)
        putStrLn $ "| ATTRGROUPS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem attrGroups)
      error $
        "TODO encodeSchemaItem > complexType > another separation case"
        ++ ifAtLine ifLn

separateComplexTypeContents ::
  [Attr] -> [Content] ->
    (Maybe String,
     ZeroOneMany Content, ZeroOneMany Content, ZeroOneMany Content,
     [Content], [Content])
separateComplexTypeContents attrs cts =
  (pullAttr "name" attrs,
   pullContent "sequence" cts,
   pullContent "complexContent" cts,
   pullContent "simpleContent" cts,
   filterTagged "attribute" cts,
   filterTagged "attributeGroup" cts)

separateSimpleTypeContents ::
  [Attr] -> [Content] ->
    (Maybe String, ZeroOneMany Content, ZeroOneMany Content,
     ZeroOneMany Content)
separateSimpleTypeContents attrs cts =
  (pullAttr "name" attrs,
   pullContent "restriction" cts,
   pullContent "union" cts,
   pullContent "list" cts)

encodeComplexTypeScheme ::
  [Attr] -> [Content] -> Content -> [Content] -> XSDQ DataScheme
encodeComplexTypeScheme ats attrSpecs
                        (Elem (Element (QName tag _ _) ats' ctnts _)) ats'' =
  encodeComplexTypeSchemeElement (ats ++ ats') attrSpecs tag ctnts ats''
encodeComplexTypeScheme ats attrSpecs s ats'' = do
  liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "S " ++ show s
  liftIO $ putStrLn $ "ATS'' " ++ (intercalate "\n    " $ map showContent ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeComplexTypeSchemeElement ::
  [Attr] -> [Content] -> String -> [Content] -> [Content] ->
    XSDQ DataScheme
encodeComplexTypeSchemeElement ats attrSpecs "complexContent" ctnts ats'' =
  case filter isElem ctnts of
    [ctnt] -> encodeComplexTypeScheme ats attrSpecs ctnt ats''
    _ -> error $ "Expected a single child for complexContent node"
encodeComplexTypeSchemeElement ats _ "sequence" ctnts ats'' = do
  -- TODO Do something with the attrSpecs (second) argument.
  included <- encodeSchemaItems ctnts
  atrSpecs <- encodeSchemaItems ats''
  -- liftIO $ putStrLn ">>> encodeComplexTypeScheme"
  nameAttrQName <- pullAttrQName "name" ats
  return $ ComplexTypeScheme (Sequence $ filter nonSkip included) atrSpecs nameAttrQName
encodeComplexTypeSchemeElement ats _ "restriction" _ctnts _ats'' = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  case baseType of
    Nothing -> error "Attribute base required in <restriction> element"
    Just t -> return $ ComplexTypeScheme (ComplexRestriction t) [] nameAttr
encodeComplexTypeSchemeElement ats _ "extension" ctnts _ats'' = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  included <- encodeSchemaItems ctnts
  case baseType of
    Nothing -> error "Attribute base required in <extension> element"
    Just t -> return $ ComplexTypeScheme (Extension t included) [] nameAttr
encodeComplexTypeSchemeElement ats attrSpecs tag ctnts ats'' = do
  liftIO $ putStrLn "+------"
  liftIO $ putStrLn "| TODO encodeComplexTypeScheme > another case"
  liftIO $ putStrLn $ "| ATS "    ++ (intercalate "\n    " $ map showAttr ats)
  liftIO $ putStrLn $ "| ATTRSPECS " ++ show attrSpecs
  liftIO $ putStrLn $ "| TAG "       ++ show tag
  liftIO $ putStrLn $ "| CTNTS "
    ++ (intercalate "\n    " $ map showContent ctnts)
  liftIO $ putStrLn $ "| ATS'' "
    ++ (intercalate "\n    " $ map showContent ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeSimpleTypeByRestriction ::
  Maybe QName -> [Attr] -> Content -> XSDQ DataScheme
encodeSimpleTypeByRestriction -- Note ignoring ats
    ifName _ (Elem (Element (QName "restriction" _ _) ats' _ _)) = do
  case pullAttr "base" ats' of
    Just base -> do
      baseQName <- decodePrefixedName base
      return $ SimpleTypeScheme ifName $ SimpleRestriction baseQName
    Nothing -> error "restriction without base"
encodeSimpleTypeByRestriction ifNam ats s = do
  liftIO $ putStrLn "+------"
  liftIO $ putStrLn "| TODO encodeSimpleTypeByRestriction > additional cases"
  liftIO $ putStrLn $ "| IFNAM " ++ show ifNam
  liftIO $ putStrLn $ "| ATS "   ++ (intercalate "\n    " $ map showAttr ats)
  case s of
    (Elem (Element _ _ _ (Just l))) ->
      liftIO $ putStrLn $ "| source line: " ++ show l
    _ -> return ()
  liftIO $ bLabelPrintln "| S " s
  error "TODO encodeSimpleTypeByRestriction > additional cases"

-- | Decode the `String` representation of an XSD integer as a Haskell
-- `Int`.  Might fail, so the result is `Maybe`-wrapped.
decodeIntOrUnbound :: String -> Maybe Int
decodeIntOrUnbound "unbounded" = Nothing
decodeIntOrUnbound s = (readMaybe s) :: Maybe Int

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
