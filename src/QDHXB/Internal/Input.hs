{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (inputSchemaItems) where

-- import System.Directory
import Language.Haskell.TH (newName, nameBase)
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
inputSchemaItems :: [Content] -> XSDQ [DataScheme]
inputSchemaItems items = do
  res <- inputSchemaItems' "Top" items
  -- dbgLn $ show res
  return res

inputSchemaItems' :: String -> [Content] -> XSDQ [DataScheme]
inputSchemaItems' outer items = do
  res <- mapM (\(s, i) ->
                 inputSchemaItem (outer ++ show i) s) $
              zip items ([1..] :: [Int])
  -- dbgLn $ show res
  return res

inputSchemaItem :: String -> Content -> XSDQ DataScheme
inputSchemaItem o e@(Elem (Element q a c l)) = do
  whenDebugging $ dbgPt $ "Encoding element " ++ showContent e
  res <- indenting $ inputElement q a c o l $ getAnnotationDocFrom c
  whenDebugging $ dbgBLabel "  Encoding result " res
  return res
inputSchemaItem _ (Text _) = do
  whenDebugging $ dbgPt "Dropping Text entry "
  return Skip
inputSchemaItem _ (CRef txt) = do
  whenDebugging $ dbgPt $ "Dropping CRef entry " ++ txt
  return Skip


inputElement ::
  QName -> [Attr] -> [Content] -> String -> Maybe Line -> Maybe String
  -> XSDQ DataScheme

inputElement (QName "element" _ _) ats content outer ln _d = do
  whenDebugging $ do
    dbgPt $ "inputElement for element tag"
    dbgLn $ "  outer tag " ++ outer
  included <- indenting $
    fmap (filter nonSkip) $ inputSchemaItems $
      filter isNonKeyNonNotationElem content
  typeQName <- pullAttrQName "type" ats
  nameQName <- pullAttrQName "name" ats
  refQName <- pullAttrQName "ref" ats
  let ifDoc = getAnnotationDocFrom content
      ifId = pullAttr "id" ats
  sub <- case included of
           [] -> return Nothing
           [x] -> return $ Just x
           _ -> do
             whenDebugging $ boxed $ do
               dbgLn $ "More than one subelement to <element>" ++ ifAtLine ln
               dbgBLabel "ATS " ats
               dbgBLabel "CONTENT " content
               dbgBLabel "INCLUDED " included
             error $ "More than one subelement to <element>" ++ ifAtLine ln
  dbgResult "Element inputElement result" $
    ElementScheme sub nameQName typeQName refQName ifId
             (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
             (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
             ln ifDoc

inputElement q@(QName "attribute" _ _) a c o l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute (o ++ "Attr") q a
                            (filter isNonKeyNonNotationElem c) l ifDoc
  return $ AttributeScheme scheme l ifDoc

inputElement q@(QName "attributeGroup" _ _) a c o l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute (o ++ "AtrGrp") q a
                            (filter isNonKeyNonNotationElem c) l ifDoc
  return $ AttributeScheme scheme l ifDoc


inputElement (QName "complexType" _ _) ats ctnts outer l d = do
  (pr, atspecs', atgrspecs') <- separateComplexContents ctnts l
  name <- pullAttrQName "name" ats
  case pr of
    Nothing -> do
      atrSpecs <- mapM (encodeAttributeScheme $ outer ++ "Cplx") $
                    atspecs' ++ atgrspecs'
      return $ ComplexTypeScheme (Composing [] atrSpecs) [] name l d
    Just (_, tag, _uri, _pfx, qn, ats', subctnts, _) -> case tag of
      "sequence" -> do
        ct <- encodeSequenceTypeScheme (outer ++ "Complex") subctnts
                                       (atspecs'++atgrspecs')
        return $ ComplexTypeScheme ct [] name l d
      "choice" -> do
        whenDebugging $
          dbgLn "inputElement > complexType > case \"choice\""
        error "inputElement > complexType > case \"choice\""
      "complexContent" -> do
        whenDebugging $ dbgLn
          "      inputElement > complexType > case \"complexContent\""
        (pr', _, _) <- separateComplexContents subctnts l
        case pr' of
          Just (_, _, _, _, sqn, sats', ssubctnts, ssline) ->
            inputElement sqn (ats ++ sats')
                         (filter isNonKeyNonNotationElem ssubctnts)
                         (outer ++ "Complex") ssline Nothing
          Nothing -> error $
            "Complex content must have primary subcontents" ++ ifAtLine l
        -- error "inputElement > complexType > case \"complexContent\""
        {-
        -- <complexContent>
        (Zero, One ctnt, Zero, attrSpecs, _) -> do
          whenDebugging $ dbgLn "(case 2) data scheme only"
          res <- encodeComplexTypeScheme ats [] ctnt attrSpecs ifLn d
          whenDebugging $ dbgBLabel "  Case result " res
          return (Just res, [])
        -}

      "simpleContent" -> do
        whenDebugging $ dbgLn
          "      inputElement > complexType > case \"simpleContent\""
        error "inputElement > complexType > case \"simpleContent\""
      _ -> boxed $ do
              dbgLn      "inputElement > complexType > another tag case"
              dbgBLabel "ATS " ats
              dbgBLabel "CTNTS " $ filter isElem ctnts
              dbgLn $    "OUTER " ++ show outer
              dbgLn $    "L " ++ show l
              dbgLn $    "D " ++ show d
              dbgLn      "-------"
              dbgBLabel "ATSPECS' " atspecs'
              dbgBLabel "ATGRSPECS' " atgrspecs'
              dbgBLabel "NAME " name
              dbgLn      "-------"
              dbgLn $    "TAG " ++ show tag
              dbgBLabel "QN " qn
              dbgBLabel "ATS' " ats'
              dbgBLabel "SUBCTNTS " $ filter isElem subctnts
              error "inputElement > complexType > unmatched"


inputElement (QName "simpleType" _ _) ats ctnts outer ifLn ifDoc = do
  let ctnts' = filter isElem ctnts
  whenDebugging $ do
    dbgPt "Input element is simpletype"
    dbgLn $ "  Outer name " ++ outer
  indenting $ case separateSimpleTypeContents ats ctnts' of
    (nam, One restr, Zero, Zero) -> do
      whenDebugging $ dbgPt "Subcase restr"
      qnam <- mapM decodePrefixedName nam
      res <- indenting $ encodeSimpleTypeByRestriction qnam
                           (maybe (outer ++ "Simple") (outer ++) nam) ats restr
      dbgResult "Subcase result" res
    (ifNam, Zero, One (Elem (Element (QName "union" _ _) _ cs' _)), Zero) -> do
      let outerUnion = outer ++ "Union"
          nam = maybe outerUnion id ifNam
      whenDebugging $ dbgPt "Subcase union"
      qnam <- decodePrefixedName nam
      alts <- indenting $
                inputSchemaItems' outerUnion $ filter isElem cs'
      dbgResult "Subcase result" $
        SimpleTypeScheme (Just qnam) (Union alts) ifLn ifDoc
    (ifNam, Zero, Zero,
     One (Elem (Element (QName "list" _ _) ats' ctnts'' _))) -> do
      whenDebugging $ dbgPt "Subcase list"
      itemTypeAttr <- pullAttrQName "itemType" ats'
      let simpleWithin = pullContent "simpleType" ctnts''
      indenting $ case (itemTypeAttr, simpleWithin) of
        (Nothing, Zero) -> error $
          "Simple type list without itemType attribute" ++ ifAtLine ifLn
        (Nothing, One node) -> do
          whenDebugging $ dbgPt "Subcase with included element type"
          tds <- indenting $ inputSchemaItem (outer ++ "List") node
          thisName <- case ifNam of
            Just n -> inDefaultNamespace n
            Nothing -> do
              let tdsLabel = labelOf tds
              case tdsLabel of
                Just n -> return $ withSuffix "Element" n
                Nothing -> do
                  fresh <- liftQtoXSDQ $ newName "List"
                  return $ QName (nameBase fresh) Nothing Nothing
          dbgResult "Subcase result" $
            SimpleTypeScheme (Just thisName) (List Nothing (Just tds))
                             ifLn ifDoc
        (Just itemType, Zero) -> do
          whenDebugging $ dbgPt "Subcase with element type reference"
          qnam <- case ifNam of
            Just n -> decodePrefixedName n
            Nothing -> return $ QName ("List_" ++ qName itemType)
                                      (qURI itemType) (qPrefix itemType)
          dbgResult "Subcase result" $
            SimpleTypeScheme (Just qnam)
                             (List (Just itemType) Nothing) ifLn ifDoc
        (x, y) -> do
          boxed $ do
            dbgLn "Disallowed subsubcase within list subcase"
            dbgBLabel "ATS " ats
            dbgBLabel "CTNTS' " ctnts'
            dbgLn $ "OUTER " ++ outer
            dbgBLabel "X " x
            dbgBLabel "Y " y
          error $ "Disallowed subcase within subcase list" ++ ifAtLine ifLn


    (ifName, zomRestr, zomUnion, zomList) -> do
      -- whenDebugging
      boxed $ do
        dbgLn "TODO inputElement > simpleType > another separation case"
        dbgBLabel "ATS " ats
        dbgBLabel "CTNTS' " ctnts'
        dbgLn $ "OUTER " ++ outer
        dbgLn $ "IFNAME " ++ show ifName
        dbgBLabel "ZOMRESTR " zomRestr
        dbgBLabel "ZOMUNION " zomUnion
        dbgBLabel "ZOMLIST " zomList
      error $ "TODO inputElement > simpleType > another separation case"
        ++ ifAtLine ifLn

inputElement (QName "annotation" _ _) _ _ _ _ _ = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ dbgPt $ "Dropping <annotation> element"
  return Skip

inputElement (QName tag _ _) _ _ _ l _d | tag=="include" || tag=="import" = do
  -- Skipping these documents for now
  dbgLn $ "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
  return Skip

inputElement (QName tagname _ _) _ _ _ _ _
  | tagname == "key" || tagname == "keyref" = do
  whenDebugging $ dbgPt $ "Dropping <" ++ tagname ++ "> entry "
  return Skip

inputElement (QName "sequence" _ _) ats ctnts outer ifLn ifDoc = do
  ifName <- pullAttrQName "name" ats
  whenDebugging $ dbgLn $
    maybe "- Sequence (unnamed)" (\n -> "- Sequence \"" ++ showQName n ++ "\"")
          ifName
  included <- indenting $ inputSchemaItems ctnts
  name <- useNameOrWrap ifName outer "Seq"
  dbgResult "Sequence result" $
    ComplexTypeScheme (Composing (filter nonSkip included) [])
                      [] (Just name) ifLn ifDoc

inputElement (QName "restriction" _ _) ats ctnts outer ifLn ifDoc = do
  whenDebugging $ dbgPt $ "Restriction, outer name " ++ outer
  let ifDoc' = getAnnotationDocFrom ctnts
  ifName <- pullAttrQName "name" ats
  case pullAttr "base" ats of
    Just base -> do
      baseQName <- decodePrefixedName base
      let thisName = case ifName of
            Just _ -> ifName
            Nothing -> Just $ QName (outer ++ "Restricted" ++ qName baseQName)
                                    (qURI baseQName) (qPrefix baseQName)
      dbgResult "Restriction result" $
        ComplexTypeScheme (ComplexRestriction baseQName) []
                          thisName ifLn (pickOrCombine ifDoc ifDoc')
    Nothing -> error "restriction without base"


inputElement (QName "extension" _ _) ats ctnts outer ifLn ifDoc = do
  whenDebugging $ dbgPt $ "Extension, outer name " ++ outer
  maybeBase <- pullAttrQName "base" ats
  let base = maybe (error $ "<extension> without base" ++ ifAtLine ifLn)
                id maybeBase
  ifName <- pullAttrQName "name" ats
  name <- useNameOrWrap ifName outer "Ext"
  (ext, newAttrs, newAttrGroups) <- separateComplexContents ctnts ifLn
  whenDebugging $ do
    dbgPt "Complex extension"
    dbgPt $ "outer name " ++ outer
    dbgBLabel "- base " base
    dbgBLabel "- ext " ext
    dbgBLabel "- newAttrs " newAttrs
    dbgBLabel "- newAttrGroups " newAttrGroups
  res <- case ext of
    Nothing -> do
      return $
        ComplexTypeScheme (Extension base []) [] (Just name) ifLn ifDoc
    Just (e,_,_,_,_,_,_,_) -> do
      e' <- indenting $ inputSchemaItem (outer ++ "Ext") e
      return $
        ComplexTypeScheme (Extension base [e']) [] (Just name) ifLn ifDoc
  whenDebugging $ dbgBLabel "  Extension result " res
  return res

inputElement (QName "group" _ _) ats ctnts outer ifLn ifDoc = do
  name <- pullAttrQName "name" ats
  ref <- pullAttrQName "ref" ats
  let subOuter = outer ++ "Group"
  let filtered = filter isFocusElem ctnts
  whenDebugging $ do
    dbgLn $ "inputElement case group" ++ ifAtLine ifLn
    dbgPt $ "For <group> schema"
      ++ maybe "(no line num)" (\x -> " at " ++ show x) ifLn
      ++ ":"
    dbgLn $ "  outer name " ++ outer
    dbgLn $ "  name from attributes " ++ maybe "(none)" qName name
    -- dbgLn $ "  filtered content " ++ show (map showContent filtered)
  indenting $ case filtered of
    (Elem (Element (QName "choice" _ _) attrs' ctnts' ifLn')):[] -> do
      whenDebugging $ dbgPt "choice subcase"
      ts <- indenting $ encodeChoiceTypeScheme name attrs' ctnts'
      dbgResult "Subcase result" $
        GroupScheme (nameOrRefOpt name ref) (Just ts) ifLn' ifDoc
    (Elem (Element (QName "sequence" _ _) _ats ctnts' _)):[] -> do
      whenDebugging $ do
        dbgPt "sequence subcase after (filter isElem ctnts)"
        indenting $ do
          dbgBLabel "- group attrs " ats
          dbgBLabel "- name " name
          dbgBLabel "- ctnts " $ filter isElem ctnts
          dbgBLabel "- ctnts' " $ filter isElem ctnts'
      seqn <- indenting $ encodeSequenceTypeScheme subOuter ctnts' []
      dbgResult "Subcase result" $
        GroupScheme (nameOrRefOpt name ref) (Just seqn) ifLn ifDoc
    (Elem (Element (QName "all" _ _) _ats _contents ifLn')):[] -> do
      whenDebugging $ dbgPt "all subcase"
      -- let ifDoc' = getAnnotationDocFrom contents
      boxed $ do
        dbgLn $
          "TODO inputElement > group with all" ++ ifAtLine ifLn'
        dbgLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
        dbgLn $ "NAME " ++ show (fmap showQName name)
        dbgLn $ "CTNTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO inputElement > group with all" ++ ifAtLine ifLn'
    _ -> do
      whenDebugging $ dbgPt "Default subcase is group of nothing"
      dbgResult "Subcase result" $
        GroupScheme (nameOrRefOpt name ref) Nothing ifLn ifDoc


inputElement (QName "choice" _ _) ats ctnts outer ifLn ifDoc = do
  whenDebugging $ dbgPt "For <choice> scheme:"
  -- whenDebugging $ do
  name <- pullAttrQName "name" ats
  ref <- pullAttrQName "ref" ats
  nameRef <- nameOrRefOptDft name ref $ outer ++ "Choice"
  let minOcc = decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats
      maxOcc = decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats
  whenDebugging $ do
    dbgPt $ "inputElement > choice" ++ ifAtLine ifLn
    dbgLn $ "-- minOccurs " ++ show minOcc
    dbgLn $ "-- maxOccurs " ++ show maxOcc
    dbgBLabel "-- ats " ats
    dbgBLabel "-- ctnts " $ filter isElem ctnts
  ts <- indenting $ encodeChoiceTypeScheme name ats ctnts
  dbgResult "Choice encoding" $ GroupScheme nameRef (Just ts) ifLn ifDoc

inputElement (QName "any" _ _) ats _ outer ifLn ifDoc = do
  whenDebugging $ dbgPt "For <any> scheme:"
  ifName <- pullAttrQName "name" ats
  name <- useNameOrWrap ifName outer "Any"
  -- whenDebugging $ do
  boxed $ do
    dbgLn $ "TODO <any>" ++ ifAtLine ifLn
    dbgLn $ "NAME " ++ showQName name
    dbgLn $ "OUTER " ++ outer
  dbgResult "Encoded as" $ UnprocessedXML (Just name) ifLn ifDoc

inputElement (QName "notation" _ _) _ _ _ _ _ = do
  whenDebugging $ dbgPt "For <notation> scheme:"
  -- whenDebugging $ do
  dbgResult "Encoded as" Skip

inputElement (QName tag _ _) ats ctnts outer ifLn _ifDoc = do
  -- whenDebugging $ do
  boxed $ do
    dbgLn $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn
    dbgLn $ "TAG " ++ show tag
    dbgBLabel "ATS " ats
    dbgBLabel "CTNTS " $ filter isElem ctnts
    dbgLn $ "OUTER " ++ outer
  error $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn


encodeSequenceTypeScheme ::
  String -> [Content] -> [Content] -> XSDQ ComplexTypeScheme
encodeSequenceTypeScheme outer subcontents attrSpecs = indenting $ do
  dbgLn $ "encodeSequenceTypeScheme outer=\"" ++ outer ++ "\""
  included <- indenting $ inputSchemaItems' (outer ++ "Seq") subcontents
  atrSpecs <- indenting $
    mapM (encodeAttributeScheme $ outer ++ "Seq") attrSpecs
  return $ Composing (filter nonSkip included) atrSpecs

encodeChoiceTypeScheme ::
  Maybe QName -> [Attr] -> [Content] -> XSDQ ComplexTypeScheme
encodeChoiceTypeScheme ifNam _attrs allCtnts = indenting $ do
  dbgLn $ "encodeChoiceTypeScheme"
  let ctnts = filter isElem allCtnts
  {-
  whenDebugging $ do
    dbgLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
    dbgLn $ "IFNAM " ++ show ifNam
    dbgLn $ "CTNTS " ++
        (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  -}
  contentSchemes <- indenting $ mapM (inputSchemaItem "X") ctnts
  return $ Choice ifNam contentSchemes


encodeAttributeScheme :: String -> Content -> XSDQ AttributeScheme
encodeAttributeScheme outer (Elem (Element q a c l)) = indenting $ do
  whenDebugging $ dbgBLabel "- Encoding attribute " q
  let ifDoc = getAnnotationDocFrom c
  res <- encodeAttribute (outer ++ "Elem") q a
                         (filter isNonKeyNonNotationElem c) l ifDoc
  whenDebugging $ dbgBLabel "  Encoding result " res
  return res
encodeAttributeScheme _o c = do
  dbgBLabel "** Nonattribute" c
  error $ "Illegal use of encodeAttributeScheme on\n" ++ showContent c

encodeAttribute ::
  String -> QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String
  -> XSDQ AttributeScheme
encodeAttribute _ (QName "attribute" _ _) ats [] _ d = indenting $ do
  typeQName <- pullAttrQName "type" ats
  refQName <- pullAttrQName "ref" ats
  nameQname <- pullAttrQName "name" ats
  return $ SingleAttribute (nameOrRefOpt nameQname refQName)
             (maybe Neither NameRef typeQName)
             (maybe "optional" id $ pullAttr "use" ats)
             d
encodeAttribute outer (QName "attribute" _ _) ats (st:sts) l d = indenting $ do
  typeQName <- pullAttrQName "type" ats
  case typeQName of
    Just n -> do
      error $ "Both named type " ++ showQName n
        ++ " and nested type spec given to attribute, " ++ showContent st
    Nothing -> do
      nameQName <- pullAttrQName "name" ats
      refQName <- pullAttrQName "ref" ats
      encodeAttributeWithNestedType (outer ++ "Attr")
                                    (nameOrRefOpt nameQName refQName)
                                    st sts
                                    (maybe "optional" id $ pullAttr "use" ats)
                                    l d
encodeAttribute o (QName "attributeGroup" _ _) ats ctnts _ d = indenting $ do
  name <- pullAttrQName "name" ats
  ref <- pullAttrQName "ref" ats
  let attrs = filterTagged "attribute" ctnts
      atGroups = filterTagged "attributeGroup" ctnts
  subcontents <- mapM (encodeAttributeScheme $ o ++ "Group") $
                   attrs ++ atGroups
  return $ AttributeGroup (nameOrRefOpt name ref) subcontents d
encodeAttribute outer (QName n _ _) a c _ _ = do
  boxed $ do
    dbgLn n
    dbgLn $ "OUTER " ++ outer
    dbgBLabel "ATTRS " a
    dbgBLabel "CTNTS " $ filter isElem c
  error $ "Can't use encodeAttribute with <" ++ n ++ ">"

encodeAttributeWithNestedType ::
  String -> NameOrRefOpt -> Content -> [Content] -> String
  -> Maybe Line -> Maybe String
  -> XSDQ AttributeScheme
encodeAttributeWithNestedType outer nameOrRef tySpec [] use _ d = do
  ds <- inputSchemaItem outer tySpec
  return $ SingleAttribute nameOrRef (Nested ds) use d
encodeAttributeWithNestedType _ _ tySpec (s:ss) _ _ _ = do
  boxed $ do
    dbgLn "Too many nested types for attribute"
    dbgBLabel "1st one " tySpec
    dbgBLabel "2nd one " s
    dbgBLabel "others " ss
  error "TODO Too many nested types for attribute"

type PrimaryBundle = (Content, String, Maybe String, Maybe String, QName,
                            [Attr], [Content], Maybe Line)

instance Blockable PrimaryBundle where
  block (_,_,_,_,q,a,c,_) = stackBlocks [
    labelBlock "name " $ block q,
    labelBlock "attrs " $ block a,
    labelBlock "subcontents " $ block $ filter isElem c]


-- | Separate the innards of a complexType element into:
--
--  1. The primary subcontents, if any
--
--  2. Attribute definitions
--
--  3. Attribute group definitions
--
separateComplexContents ::
  [Content] -> Maybe Line -> XSDQ (Maybe PrimaryBundle, [Content], [Content])
separateComplexContents contents ifLn =
  separateComplexContents' Nothing [] [] contents

  where separateComplexContents' ::
          Maybe PrimaryBundle -> [Content] -> [Content] -> [Content]
          -> XSDQ (Maybe PrimaryBundle, [Content], [Content])
        separateComplexContents' primary annAcc annGroupAcc [] =
          return (primary, reverse annAcc, reverse annGroupAcc)
        separateComplexContents' pr as ags (e:xs) =
          case e of
            Elem (Element q@(QName n u p) a c l) -> case n of
              "attribute" -> separateComplexContents' pr (e:as) ags xs
              "attributeGroup" -> separateComplexContents' pr (e:as) ags xs
              "annotation" -> separateComplexContents' pr as ags xs
              "anyAttribute" -> separateComplexContents' pr as ags xs
              -- TODO revisit anyAnnotation later --- key-value pairs?
              "documentation" -> separateComplexContents' pr as ags xs
              _ -> case pr of
                Nothing -> separateComplexContents'
                             (Just (e, n, u, p, q, a, c, l))
                             as ags xs
                Just (_, n', _, _, _, _, _, _) -> error $
                  "Multiple primary sub-elements " ++ n' ++ " and " ++ n
                  ++ " as complexType contents"
                  ++ maybe "" ((" at line " ++) . show) ifLn
            _ -> separateComplexContents' pr as ags xs

separateSimpleTypeContents ::
  [Attr] -> [Content] ->
    (Maybe String, ZeroOneMany Content, ZeroOneMany Content,
     ZeroOneMany Content)
separateSimpleTypeContents attrs cts =
  (pullAttr "name" attrs,
   pullContent "restriction" cts,
   pullContent "union" cts,
   pullContent "list" cts)


encodeSimpleTypeByRestriction ::
  Maybe QName -> String -> [Attr] -> Content -> XSDQ DataScheme
encodeSimpleTypeByRestriction -- Note ignoring ats
    ifName outer _ (Elem (Element (QName "restriction" _ _) ats' cs ln)) = do
  whenDebugging $ dbgPt $ "encode simple by restr, outer name " ++ outer
  let ifDoc = getAnnotationDocFrom cs
  case pullAttr "base" ats' of
    Just base -> do
      baseQName <- decodePrefixedName base
      let useName = maybe (Just $ withPrefix (outer ++ "Restr") baseQName)
                          (const ifName) ifName
                    -- TODO --- make sure this is in target namespace
      dbgResult "Encoding result" $
        SimpleTypeScheme useName (SimpleRestriction baseQName) ln ifDoc
    Nothing -> error "restriction without base"
encodeSimpleTypeByRestriction ifNam _ ats s = do
  boxed $ do
    dbgLn "TODO encodeSimpleTypeByRestriction > additional cases"
    dbgLn $ "IFNAM " ++ show ifNam
    dbgLn $ "ATS "   ++ (intercalate "\n    " $ map showAttr ats)
    case s of
      Elem (Element _ _ _ (Just l)) -> dbgLn $ "source line: " ++ show l
      _ -> return ()
    dbgBLabel "S " s
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
decodeMaybeIntOrUnbound1 = maybe (Just 1) decodeIntOrUnbound

pullAttrQName :: String -> [Attr] -> XSDQ (Maybe QName)
pullAttrQName str attrs = mapM decodePrefixedName (pullAttr str attrs)

ifAtLine :: Maybe Line -> String
ifAtLine ifLine = maybe "" (\line -> " at XSD line " ++ show line) ifLine

-- | Assemble a `NameOrRefOpt` value from two @(`Maybe` `QName`)@
-- values and a default `String` name.  The first argument corresponds
-- to a possible @name@ attribute; the second argument, to a possible
-- @ref@ attribute; the third, to a default name derived from the
-- context of the defining element in the source XSD.
nameOrRefOptDft :: Maybe QName -> Maybe QName -> String -> XSDQ NameOrRefOpt
nameOrRefOptDft (Just _) (Just _) _ =
  error "Cannot give both name and ref attributes"
nameOrRefOptDft (Just n) Nothing _ = return $ WithName n
nameOrRefOptDft Nothing (Just r) _ = return $ WithRef r
nameOrRefOptDft Nothing Nothing s = fmap WithName $ inDefaultNamespace s
