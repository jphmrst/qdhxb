-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (encodeSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.Read (readMaybe)
import Text.XML.Light.Output
import Text.XML.Light.Types
import QDHXB.Internal.Utils.Misc (pickOrCombine)
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
encodeSchemaItem e@(Elem (Element q a c l)) = do
  whenDebugging $ liftIO $ putStrLn $ "> Encoding element " ++ showContent e
  res <- encodeElement q a c l $ getAnnotationDocFrom c
  whenDebugging $ liftIO $ bLabelPrintln "    `--> " res
  return res
encodeSchemaItem (Text _) = do
  whenDebugging $ liftIO $ putStrLn "> Dropping Text entry "
  return Skip
encodeSchemaItem (CRef txt) = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping CRef entry " ++ txt
  return Skip

encodeElement ::
  QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String -> XSDQ DataScheme
encodeElement (QName "element" _ _) ats content ln _d = do
  included <- encodeSchemaItems $ filter isElem content
  typeQName <- pullAttrQName "type" ats
  nameQName <- pullAttrQName "name" ats
  refQName <- pullAttrQName "ref" ats
  let ifDoc = getAnnotationDocFrom content
  {-
  liftIO $ do
    putStrLn $ "+++ "
      ++ show (pullContent "annotation" content)
    putStrLn $ "--- "
      ++ case zomToList $ pullContent "annotation" content of
           [] -> "none"
           ann:_ -> show $ pullContentFrom "documentation" ann
    putStrLn $ "--- "
      ++ case zomToList $ pullContent "annotation" content of
           [] -> "none"
           ann:_ -> case zomToList $ pullContentFrom "documentation" ann of
             [] -> "none"
             doc:_ -> show $ pullCRefContent "documentation" doc
    putStrLn $
      "*** Docstring for " ++ show (fmap showQName nameQName)
        ++ " is " ++ show ifDoc
    -}
  let ifId = pullAttr "id" ats
  return $ ElementScheme included nameQName typeQName refQName ifId
             (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
             (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
             ln ifDoc
encodeElement q@(QName "attribute" _ _) a c l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute q a c l ifDoc
  return $ AttributeScheme scheme l ifDoc
encodeElement q@(QName "attributeGroup" _ _) a c l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute q a c l ifDoc
  return $ AttributeScheme scheme l ifDoc
encodeElement (QName "complexType" _ _) ats ctnts l d = do
  let ctnts' = filter isElem ctnts
  whenDebugging $ liftIO $ putStrLn $
    "  - Using separateAndDispatchComplexContents for <complexType> "
  p <- separateAndDispatchComplexContents ctnts' ats l d
  name <- pullAttrQName "name" ats
  let d' = pickOrCombine d $ getAnnotationDocFrom ctnts
  case p of
    (Just ds, []) -> return ds
    (Nothing, as) -> return $ ComplexTypeScheme (Composing [] as) [] name l d'
    (Just ds, as) -> return $ ComplexTypeScheme (Composing [ds] as) [] name l d'
encodeElement (QName "simpleType" _ _) ats ctnts ifLn ifDoc = do
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
         return $ SimpleTypeScheme (Just qnam) (Union alts) ifLn ifDoc
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
          let res = SimpleTypeScheme (Just qnam)
                      (List (Just itemType)) ifLn ifDoc
          whenDebugging $ liftIO $ do
            putStrLn "> Encoding simpleType "
            -- bLabelPrintln "    " e
            bLabelPrintln "  as " res
          return res
    (ifName, zomRestr, zomUnion, zomList) -> do
      -- whenDebugging
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
      error $ "TODO encodeElement > simpleType > another separation case"
        ++ ifAtLine ifLn
encodeElement (QName "annotation" _ _) _ _ _ _ = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <annotation> element"
  return Skip
encodeElement (QName tag _ _) _ _ l _d | tag=="include" || tag=="import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
  return Skip
encodeElement (QName tagname _ _) _ _ _ _
  | tagname == "key" || tagname == "keyref" = do
  whenDebugging $ liftIO $ putStrLn $ "  - Dropping <" ++ tagname ++ "> entry "
  return Skip
encodeElement (QName "sequence" _ _) ats ctnts ifLn ifDoc = do
  let ctnts' = filter isElem ctnts
  whenDebugging $ liftIO $ putStrLn $
    "  - Using separateAndDispatchComplexContents for <sequence> "
  sepPair <- separateAndDispatchComplexContents ctnts' ats ifLn ifDoc
  case sepPair of
    (Just ds, []) -> return ds
    _ -> error "Unexpected separation from <sequence>"
encodeElement (QName "group" _ _) ats ctnts ifLn ifDoc = do
  whenDebugging $ liftIO $ putStrLn "  - For <group> schema:"
  name <- pullAttrQName "name" ats
  case filter isElem ctnts of
    (Elem (Element (QName "choice" _ _) attrs' ctnts' ifLn')):[] -> do
      ts <- encodeChoiceTypeScheme name attrs' ctnts'
      return $ GroupScheme name (Just ts) ifLn' ifDoc
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
    (Elem (Element (QName "all" _ _) _ats _contents ifLn')):[] -> do
      -- let ifDoc' = getAnnotationDocFrom contents
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
      return $ GroupScheme name Nothing ifLn ifDoc
encodeElement (QName tag _ _) ats ctnts ifLn _ifDoc = do
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
  whenDebugging $ liftIO $ do
    putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
      putStrLn $ "IFNAM " ++ show ifNam
      putStrLn $ "CTNTS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  -}
  contentSchemes <- mapM encodeSchemaItem ctnts
  return $ Choice ifNam contentSchemes

encodeAttributeScheme :: Content -> XSDQ AttributeScheme
encodeAttributeScheme (Elem (Element q a c l)) = do
  whenDebugging $ liftIO $ bLabelPrintln "    - Encoding attribute " q
  let ifDoc = getAnnotationDocFrom c
  res <- encodeAttribute q a c l ifDoc
  whenDebugging $ liftIO $ bLabelPrintln "        `--> " res
  return res
encodeAttributeScheme c = do
  liftIO $ bLabelPrintln "** Nonattribute" c
  error $ "Illegal use of encodeAttributeScheme on\n" ++ showContent c

encodeAttribute ::
  QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String
  -> XSDQ AttributeScheme
encodeAttribute (QName "attribute" _ _) ats [] _ d = do
  typeQName <- pullAttrQName "type" ats
  refQName <- pullAttrQName "ref" ats
  nameQname <- pullAttrQName "name" ats
  return $ SingleAttribute nameQname refQName typeQName
                           (case pullAttr "use" ats of
                              Nothing -> "optional"
                              Just s -> s) d
encodeAttribute (QName "attributeGroup" _ _) ats ctnts _ d = do
  name <- pullAttrQName "name" ats
  ref <- pullAttrQName "ref" ats
  let attrs = filterTagged "attribute" ctnts
      atGroups = filterTagged "attributeGroup" ctnts
  subcontents <- mapM encodeAttributeScheme $ attrs ++ atGroups
  return $ AttributeGroup name ref subcontents d
encodeAttribute (QName c _ _) _ _ _ _ =
  error $ "Can't use encodeAttribute with <" ++ c ++ ">"

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = case ifLine of
                    Nothing -> ""
                    Just line -> " at XSD line " ++ show line

separateAndDispatchComplexContents ::
  [Content] -> [Attr] -> Maybe Line -> Maybe String
  -> XSDQ (Maybe DataScheme, [AttributeScheme])
separateAndDispatchComplexContents contents ats ifLn ifDoc = do
  let d = getAnnotationDocFrom contents
  case separateComplexTypeContents contents of
    -- <sequence>
    (One intl, Zero, Zero, attrSpecs, _) -> do
      whenDebugging $ liftIO $ putStrLn "    (case 1)"
      res <- encodeComplexTypeScheme ats [] intl attrSpecs ifLn d
      return (Just res, [])
    -- <complexContent>
    (Zero, One ctnt, Zero, attrSpecs, _) -> do
      whenDebugging $ liftIO $ putStrLn "    (case 2)"
      res <- encodeComplexTypeScheme ats [] ctnt attrSpecs ifLn d
      return (Just res, [])
    (Zero, Zero, Zero, [attrSpec], []) -> do
      whenDebugging $ liftIO $ putStrLn "    (one attribute)"
      attrScheme <- encodeAttributeScheme attrSpec
      return (Nothing, [attrScheme])
    (Zero, Zero, Zero, [], [attrGroup]) -> do
      whenDebugging $ liftIO $ putStrLn "    (one group)"
      attrScheme <- encodeAttributeScheme attrGroup
      return (Nothing, [attrScheme])
    (Zero, Zero, Zero, attrSpecs, attrGroups)
      | length attrSpecs > 0 || length attrGroups > 0 -> do
          whenDebugging $ liftIO $ putStrLn "     (attrs/groups)"
          let allAttr :: [Content]
              allAttr = attrSpecs ++ attrGroups
          attrSchemes <- mapM encodeAttributeScheme allAttr
          return (Nothing, [AttributeGroup Nothing Nothing attrSchemes ifDoc])
    (Zero, Zero, One ctnt, attrSpecs, _) -> do
      -- whenDebugging $
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $ "| Encoding complexType (case 3) EEEEEEEE"
        putStrLn $ "| CTNT " ++ ppContent ctnt
        putStrLn $ "| ATTRSPECS " ++ show attrSpecs
      error $ "TODO encodeSchemaItem > complexType > simpleContent"
        ++ ifAtLine ifLn
    (seqnce, cplxCtnt, simplCtnt, attrSpecs, attrGroups) -> do
      {- whenDebugging $ -}
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $ "| encodeSchemaItem > complexType > another separation case"
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| CTNTS' " ++
          (intercalate "\n    " $ map ppContent $ filter isElem contents)
        bLabelPrintln "| SEQ " seqnce
        putStrLn $ "| CPLXCTNT " ++
          (intercalate "\n    " $
           map ppContent $ filter isElem $ zomToList cplxCtnt)
        putStrLn $ "| SIMPLCTNT " ++
          (intercalate "\n    " $
           map ppContent $ filter isElem $ zomToList simplCtnt)
        putStrLn $ "| ATTRSPECS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem attrSpecs)
        putStrLn $ "| ATTRGROUPS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem attrGroups)
        putStrLn $ "| all CONTENTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem contents)
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
  [Attr] -> [Content] -> Content -> [Content] -> Maybe Line -> Maybe String
  -> XSDQ DataScheme
encodeComplexTypeScheme ats attrSpecs
                        (Elem (Element (QName tag _ _) ats' ctnts _))
                        ats'' l d =
  encodeComplexTypeSchemeElement (ats ++ ats') attrSpecs tag ctnts ats'' l d
encodeComplexTypeScheme ats attrSpecs s ats'' _l _d = do
  liftIO $ do
    putStrLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
    putStrLn $ "ATTRSPECS " ++ show attrSpecs
    putStrLn $ "S " ++ show s
    putStrLn $ "ATS'' " ++ (intercalate "\n    " $ map showContent ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeComplexTypeSchemeElement ::
  [Attr] -> [Content] -> String -> [Content] -> [Content]
  -> Maybe Line -> Maybe String
  -> XSDQ DataScheme
encodeComplexTypeSchemeElement ats attrSpecs "complexContent" ctnts ats'' ln d =
  case filter isElem ctnts of
    [ctnt] -> encodeComplexTypeScheme ats attrSpecs ctnt ats'' ln d
    _ -> error $ "Expected a single child for complexContent node"
encodeComplexTypeSchemeElement ats _ "sequence" ctnts ats'' ln d = do
  -- TODO Do something with the attrSpecs (second) argument.
  included <- encodeSchemaItems ctnts
  atrSpecs <- encodeSchemaItems ats''
  -- liftIO $ putStrLn ">>> encodeComplexTypeScheme"
  nameAttrQName <- pullAttrQName "name" ats
  return $ ComplexTypeScheme (Composing (filter nonSkip included) [])
                             atrSpecs nameAttrQName ln d
encodeComplexTypeSchemeElement ats _ "restriction" _ctnts _ats'' l d = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  case baseType of
    Nothing -> error "Attribute base required in <restriction> element"
    Just t -> return $ ComplexTypeScheme (ComplexRestriction t) [] nameAttr l d
encodeComplexTypeSchemeElement ats _ "extension" ctnts _ats'' l d = do
  nameAttr <- pullAttrQName "name" ats
  baseType <- pullAttrQName "base" ats
  included <- encodeSchemaItems ctnts
  case baseType of
    Nothing -> error "Attribute base required in <extension> element"
    Just t -> return $ ComplexTypeScheme (Extension t included) [] nameAttr l d
encodeComplexTypeSchemeElement ats attrSpecs tag ctnts ats'' _ _d = do
  liftIO $ do
    putStrLn "+------"
    putStrLn "| TODO encodeComplexTypeScheme > another case"
    putStrLn $ "| ATS "    ++ (intercalate "\n    " $ map showAttr ats)
    putStrLn $ "| ATTRSPECS " ++ show attrSpecs
    putStrLn $ "| TAG "       ++ show tag
    putStrLn $ "| CTNTS " ++ (intercalate "\n    " $ map showContent ctnts)
    putStrLn $ "| ATS'' " ++ (intercalate "\n    " $ map showContent ats'')
  error "TODO encodeComplexTypeScheme > another case"

encodeSimpleTypeByRestriction ::
  Maybe QName -> [Attr] -> Content -> XSDQ DataScheme
encodeSimpleTypeByRestriction -- Note ignoring ats
    ifName _ (Elem (Element (QName "restriction" _ _) ats' contents ln)) = do
  let ifDoc = getAnnotationDocFrom contents
  case pullAttr "base" ats' of
    Just base -> do
      baseQName <- decodePrefixedName base
      return $ SimpleTypeScheme ifName (SimpleRestriction baseQName) ln ifDoc
    Nothing -> error "restriction without base"
encodeSimpleTypeByRestriction ifNam ats s = do
  liftIO $ do
    putStrLn "+------"
    putStrLn "| TODO encodeSimpleTypeByRestriction > additional cases"
    putStrLn $ "| IFNAM " ++ show ifNam
    putStrLn $ "| ATS "   ++ (intercalate "\n    " $ map showAttr ats)
    case s of
      (Elem (Element _ _ _ (Just l))) ->
        putStrLn $ "| source line: " ++ show l
      _ -> return ()
    bLabelPrintln "| S " s
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

pullAttrQName :: String -> [Attr] -> XSDQ (Maybe QName)
pullAttrQName str attrs = mapM decodePrefixedName (pullAttr str attrs)
