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
encodeSchemaItem e@(Elem (Element _ _ _ _)) = do
  whenDebugging $ liftIO $ putStrLn $ "> Encoding element " ++ showContent e
  res <- encodeElement e
  whenDebugging $ liftIO $ bLabelPrintln "    `--> " res
  return res
encodeSchemaItem (Text _) = do
  whenDebugging $ liftIO $ putStrLn "> Dropping Text entry "
  return Skip
encodeSchemaItem (CRef txt) = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping CRef entry " ++ txt
  return Skip

encodeElement :: Content -> XSDQ DataScheme
encodeElement (Elem (Element (QName "element" _ _) ats content _)) = do
  included <- encodeSchemaItems $ filter isElem content
  typeQName <- pullAttrQName "type" ats
  nameQName <- pullAttrQName "name" ats
  refQName <- pullAttrQName "ref" ats
  return $ ElementScheme included nameQName typeQName refQName
             (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
             (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
encodeElement e@(Elem (Element q@(QName "attribute" _ _) a c l)) = do
  scheme <- encodeAttribute q a c l
  return $ AttributeScheme scheme
encodeElement e@(Elem (Element q@(QName "attributeGroup" _ _) a c l)) = do
  scheme <- encodeAttribute q a c l
  return $ AttributeScheme scheme
encodeElement (Elem (Element (QName "complexType" _ _) ats ctnts l)) = do
  let ctnts' = filter isElem ctnts
  separateAndDispatchComplexContents ctnts' ats l
encodeElement (Elem (Element (QName "simpleType" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  case separateSimpleTypeContents ats ctnts' of
    (nam, One restr, Zero, Zero) -> do
      whenDebugging $ liftIO $
        putStrLn "  - Via separateSimpleTypeContents case 1"
      qnam <- mapM decodePrefixedName nam
      encodeSimpleTypeByRestriction qnam ats restr
    (Just nam, Zero, One (Elem (Element (QName "union" _ _) _ cs' _)), Zero) ->
      do whenDebugging $ liftIO $
           putStrLn "  - Via separateSimpleTypeContents case 2"
         qnam <- decodePrefixedName nam
         alts <- encodeSchemaItems $ filter isElem cs'
         return $ SimpleTypeScheme (Just qnam) $ Union alts
    (ifNam, Zero, Zero,
     One (Elem (Element (QName "list" _ _) ats' _ _))) -> do
      whenDebugging $ liftIO $
        putStrLn "  - Via separateSimpleTypeContents case 3"
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
          whenDebugging $ do
            liftIO $ putStrLn "> Encoding simpleType "
            -- liftIO $ putStrLn $ mlineIndent "    " (showElement e)
            liftIO $ bLabelPrintln "  as " res
          return res
    (ifName, zomRestr, zomUnion, zomList) -> do
      -- whenDebugging $ do
      liftIO $ putStrLn "+------"
      liftIO $ putStrLn $
        "| TODO encodeSchemaItem > simpleType > another separation case"
      liftIO $ putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "| CTNTS' "
        ++ (intercalate "\n    " $ map showContent ctnts')
      liftIO $ putStrLn $ "| IFNAME " ++ show ifName
      liftIO $ putStrLn $ "| ZOMRESTR " ++ (show $ zomToList zomRestr)
      liftIO $ putStrLn $ "| ZOMUNION " ++ (show $ zomToList zomUnion)
      liftIO $ putStrLn $ "| ZOMLIST "  ++ (show zomList)
      error $ "TODO encodeElement > simpleType > another separation case"
        ++ ifAtLine ifLn
encodeElement (Elem (Element (QName "annotation" _ _) _ _ _)) = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <annotation> element"
  return Skip
encodeElement (Elem (Element (QName tag _ _) _ _ l)) |
    tag == "include" || tag == "import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
  return Skip
encodeElement (Elem (Element (QName tagname _ _) _ _ _))
  | tagname == "key" || tagname == "keyref" = do
  whenDebugging $ do
    liftIO $ putStrLn $ "  - Dropping <" ++ tagname ++ "> entry "
  return Skip
encodeElement (Elem (Element (QName "sequence" _ _) ats ctnts ifLn)) = do
  let ctnts' = filter isElem ctnts
  separateAndDispatchComplexContents ctnts' ats ifLn
encodeElement (Elem (Element (QName "group" _ _) ats ctnts _ifLn)) = do
  whenDebugging $ liftIO $ putStrLn "  - For <group> schema:"
  name <- pullAttrQName "name" ats
  case filter isElem ctnts of
    (Elem (Element (QName "choice" _ _) attrs' ctnts' _ifLn')):[] -> do
      ts <- encodeChoiceTypeScheme name attrs' ctnts'
      return $ Group name $ Just ts
    (Elem (Element (QName "sequence" _ _) _ats _ ifLn')):[] -> do
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $ "| Case 1 after (filter isElem ctnts)"
        putStrLn $ "| group ATTRS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| sequence ATTRS " ++ (intercalate "\n    " $
                                           map showAttr _ats)
        putStrLn $ "| NAME " ++ show (fmap showQName name)
        putStrLn $ "| CTNTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO encodeSchemaItem > group with sequence" ++ ifAtLine ifLn'
    (Elem (Element (QName "all" _ _) _ats _ ifLn')):[] -> do
      liftIO $ do
        putStrLn $ "+-------"
        putStrLn $
          "| TODO encodeSchemaItem > group with all" ++ ifAtLine ifLn'
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| NAME " ++ show (fmap showQName name)
        putStrLn $ "| CTNTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO encodeSchemaItem > group with all" ++ ifAtLine ifLn'
    _ -> do
      liftIO $ putStrLn $ "  - Default is group of nothing"
      return $ Group name Nothing
encodeElement (Elem (Element (QName tag _ _) ats ctnts ifLn)) = do
  -- whenDebugging $ do
  liftIO $ do
    putStrLn $ "+-------"
    putStrLn $
      "| TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn
    putStrLn $ "| TAG " ++ show tag
    putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
    putStrLn $ "| CTNTS "
      ++ (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  error $ "TODO encodeSchemaItem > another Element case" ++ ifAtLine ifLn

encodeChoiceTypeScheme ::
  Maybe QName -> [Attr] -> [Content] -> XSDQ ComplexTypeScheme
encodeChoiceTypeScheme ifNam _attrs allCtnts = do
  let ctnts = filter isElem allCtnts
  {-
  whenDebugging $ do
    liftIO $ putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
    liftIO $ putStrLn $ "IFNAM " ++ show ifNam
    liftIO $ putStrLn $ "CTNTS " ++
      (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  -}
  contentSchemes <- mapM encodeSchemaItem ctnts
  return $ Choice ifNam contentSchemes

encodeAttributeScheme :: Content -> XSDQ AttributeScheme
encodeAttributeScheme (Elem (Element q a c l)) = do
  whenDebugging $ liftIO $ bLabelPrintln "  - Encoding attribute" q
  res <- encodeAttribute q a c l
  whenDebugging $ liftIO $ bLabelPrintln "      `-->" res
  return res
encodeAttributeScheme c = do
  liftIO $ bLabelPrintln "** Nonattribute" c
  error $ "Illegal use of encodeAttributeScheme on\n" ++ showContent c

encodeAttribute ::
  QName -> [Attr] -> [Content] -> Maybe Line -> XSDQ AttributeScheme
encodeAttribute (QName "attribute" _ _) ats [] _ = do
  typeQName <- pullAttrQName "type" ats
  refQName <- pullAttrQName "ref" ats
  nameQname <- pullAttrQName "name" ats
  return $ SingleAttribute nameQname refQName typeQName
                           (case pullAttr "use" ats of
                              Nothing -> "optional"
                              Just s -> s)
encodeAttribute (QName "attributeGroup" _ _) ats ctnts _ = do
  name <- pullAttrQName "name" ats
  ref <- pullAttrQName "ref" ats
  let attrs = filterTagged "attribute" ctnts
      atGroups = filterTagged "attributeGroup" ctnts
  subcontents <- mapM encodeAttributeScheme $ attrs ++ atGroups
  return $ AttributeGroup name ref subcontents
encodeAttribute (QName c _ _) _ _ _ =
  error $ "Can't use encodeAttribute with <" ++ c ++ ">"

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = case ifLine of
                    Nothing -> ""
                    Just line -> " at XSD line " ++ show line

separateAndDispatchComplexContents ::
  [Content] -> [Attr] -> Maybe Line -> XSDQ DataScheme
separateAndDispatchComplexContents contents ats ifLn =
  case separateComplexTypeContents contents of
    -- <sequence>
    (One intl, Zero, Zero, attrSpecs, _) -> do
      res <- encodeComplexTypeScheme ats [] intl attrSpecs
      whenDebugging $ do
        liftIO $ putStrLn "> Encoding complexType (case 1)"
        liftIO $ bLabelPrintln "  as " res
      return res
    -- <complexContent>
    (Zero, One ctnt, Zero, attrSpecs, _) -> do
      res <- encodeComplexTypeScheme ats [] ctnt attrSpecs
      whenDebugging $ do
        liftIO $ putStrLn $ "> Encoding complexType (case 2)"
        liftIO $ bLabelPrintln "  as " res
      return res
    (Zero, Zero, Zero, [attrSpec], []) -> do
          res <- fmap AttributeScheme $ encodeAttributeScheme attrSpec
          whenDebugging $ do
            liftIO $ putStrLn $
              "  > encodeSchemaItem > complexType > attributes only"
            liftIO $ putStrLn $ "    ATS " ++
              (intercalate "\n    " $ map showAttr ats)
            liftIO $ putStrLn $ "    CTNTS' " ++
              (intercalate "\n    " $ map ppContent $ filter isElem contents)
          return res
    (Zero, Zero, Zero, [], [attrGroup]) -> do
          res <- fmap AttributeScheme $ encodeAttributeScheme attrGroup
          whenDebugging $ do
            liftIO $ putStrLn $
              "  > encodeSchemaItem > complexType > attributes only"
            liftIO $ putStrLn $ "    ATS " ++
              (intercalate "\n    " $ map showAttr ats)
            liftIO $ putStrLn $ "    CTNTS' " ++
              (intercalate "\n    " $ map ppContent $ filter isElem contents)
          return res
    (Zero, Zero, Zero, attrSpecs, attrGroups)
      | length attrSpecs > 0 || length attrGroups > 0 -> do
          let allAttr :: [Content]
              allAttr = attrSpecs ++ attrGroups
          res <- fmap (AttributeScheme . AttributeGroup Nothing Nothing) $
                   mapM encodeAttributeScheme allAttr
          whenDebugging $ do
            liftIO $ putStrLn $
              "> encodeSchemaItem > complexType > attributes only"
            liftIO $ putStrLn $ "    - ats " ++
              (intercalate "\n    " $ map showAttr ats)
            liftIO $ putStrLn $ "    - ctnts' " ++
              (intercalate "\n    " $ map ppContent $ filter isElem contents)
            liftIO $ putStrLn $ "    - attrspecs " ++
              (intercalate "\n    " $ map ppContent $ filter isElem attrSpecs)
            liftIO $ putStrLn $ "    - attrgroups " ++
              (intercalate "\n    " $ map ppContent $ filter isElem attrGroups)
            liftIO $ bLabelPrintln "    --> " res
          return res
    (Zero, Zero, One ctnt, attrSpecs, _) -> do
      -- whenDebugging $ do
      liftIO $ putStrLn $ "+--------"
      liftIO $ putStrLn $ "| Encoding complexType (case 3) EEEEEEEE"
      liftIO $ putStrLn $ "| CTNT " ++ ppContent ctnt
      liftIO $ putStrLn $ "| ATTRSPECS " ++ show attrSpecs
      error $ "TODO encodeSchemaItem > complexType > simpleContent"
        ++ ifAtLine ifLn
    (seqnce, cplxCtnt, simplCtnt, attrSpecs, attrGroups) -> do
      {- whenDebugging $ do -}
      liftIO $ putStrLn $ "+--------"
      liftIO $ putStrLn $ "| encodeSchemaItem > complexType > another separation case"
      liftIO $ putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
      liftIO $ putStrLn $ "| CTNTS' " ++
        (intercalate "\n    " $ map ppContent $ filter isElem contents)
      liftIO $ bLabelPrintln "| SEQ " seqnce
      liftIO $ putStrLn $ "| CPLXCTNT " ++
        (intercalate "\n    " $ map ppContent $ filter isElem $ zomToList cplxCtnt)
      liftIO $ putStrLn $ "| SIMPLCTNT " ++
        (intercalate "\n    " $ map ppContent $ filter isElem $ zomToList simplCtnt)
      liftIO $ putStrLn $ "| ATTRSPECS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem attrSpecs)
      liftIO $ putStrLn $ "| ATTRGROUPS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem attrGroups)
      error $
        "TODO encodeSchemaItem > complexType > another separation case"
        ++ ifAtLine ifLn

separateComplexTypeContents ::
  [Content] ->
    (ZeroOneMany Content, ZeroOneMany Content, ZeroOneMany Content,
     [Content], [Content])
separateComplexTypeContents cts =
  (pullContent "sequence" cts,
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
