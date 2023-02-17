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
  (pr, atspecs', atgrspecs') <- separateComplexContents ctnts l
  name <- pullAttrQName "name" ats
  case pr of
    Nothing -> do
      atrSpecs <- mapM encodeAttributeScheme $ atspecs' ++ atgrspecs'
      return $ ComplexTypeScheme (Composing [] atrSpecs) [] name l d
    Just (tag, uri, pfx, qn, ats', subctnts, _) -> case tag of
      "sequence" -> do
        included <- encodeSchemaItems subctnts
        atrSpecs <- mapM encodeAttributeScheme $ atspecs' ++ atgrspecs'
        return $ ComplexTypeScheme (Composing included atrSpecs) [] name l d
      "choice" -> do
        whenDebugging $ liftIO $
          putStrLn "      encodeElement > complexType > case \"choice\""
        error "encodeElement > complexType > case \"choice\""
      "complexContent" -> do
        whenDebugging $ liftIO $ putStrLn
          "      encodeElement > complexType > case \"complexContent\""
        (pr', _, _) <- separateComplexContents subctnts l
        case pr' of
          Just (_, _, _, sqn, sats', ssubctnts, ssline) ->
            encodeElement sqn sats' ssubctnts ssline Nothing
          Nothing -> error $
            "Complex content must have primary subcontents" ++ ifAtLine l
        -- error "encodeElement > complexType > case \"complexContent\""
        {-
        -- <complexContent>
        (Zero, One ctnt, Zero, attrSpecs, _) -> do
          whenDebugging $ liftIO $ putStrLn "    (case 2) data scheme only"
          res <- encodeComplexTypeScheme ats [] ctnt attrSpecs ifLn d
          whenDebugging $ liftIO $ bLabelPrintln "      --> " res
          return (Just res, [])
        -}

      "simpleContent" -> do
        whenDebugging $ liftIO $ putStrLn
          "      encodeElement > complexType > case \"simpleContent\""
        error "encodeElement > complexType > case \"simpleContent\""
      _ -> do
        liftIO $ do
          putStrLn      "+--------"
          putStrLn      "| encodeElement > complexType > another tag case"
          bLabelPrintln "| ATS " ats
          bLabelPrintln "| CTNTS " $ filter isElem ctnts
          putStrLn $    "| L " ++ show l
          putStrLn $    "| D " ++ show d
          putStrLn      "+--------"
          bLabelPrintln "| ATSPECS' " atspecs'
          bLabelPrintln "| ATGRSPECS' " atgrspecs'
          bLabelPrintln "| NAME " name
          putStrLn      "+--------"
          putStrLn $    "| TAG " ++ show tag
          bLabelPrintln "| QN " qn
          bLabelPrintln "| ATS' " ats'
          bLabelPrintln "| SUBCTNTS " $ filter isElem subctnts
          putStrLn      "+--------"
        error "encodeElement > complexType > unmatched"
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
encodeElement (QName "restriction" _ _) ats ctnts ifLn ifDoc = do
  let ifDoc' = getAnnotationDocFrom ctnts
  ifName <- pullAttrQName "name" ats
  case pullAttr "base" ats of
    Just base -> do
      baseQName <- decodePrefixedName base
      return $
        ComplexTypeScheme (ComplexRestriction baseQName) [] ifName ifLn
                          (pickOrCombine ifDoc ifDoc')
    Nothing -> error "restriction without base"
encodeElement (QName "extension" _ _) ats ctnts ifLn _ifDoc = do
  -- whenDebugging $ do
  liftIO $ do
    putStrLn $ "+-------"
    putStrLn $
      "| TODO encodeSchemaItem > extension case" ++ ifAtLine ifLn
    putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
    putStrLn $ "| CTNTS "
      ++ (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
  error $ "TODO encodeSchemaItem > extension case" ++ ifAtLine ifLn
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

encodeAttributeDataScheme :: Content -> XSDQ DataScheme
encodeAttributeDataScheme e@(Elem (Element _ _ c l)) = do
  attSch <- encodeAttributeScheme e
  let ifDoc = getAnnotationDocFrom c
  return $ AttributeScheme attSch l ifDoc
encodeAttributeDataScheme e = do
  error "Non-element passed to encodeAttributeDataScheme"

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

-- | Separate the innards of a complexType element into:
--
--  1. The primary subcontents, if any
--
--  2. Attribute definitions
--
--  3. Attribute group definitions
--
separateComplexContents ::
  [Content] -> Maybe Line
  -> XSDQ (Maybe (String, Maybe String, Maybe String, QName,
                  [Attr], [Content], Maybe Line),
           [Content], [Content])
separateComplexContents contents ifLn =
  separateComplexContents' Nothing [] [] contents

  where separateComplexContents' ::
          Maybe (String, Maybe String, Maybe String, QName,
                 [Attr], [Content], Maybe Line)
          -> [Content] -> [Content] -> [Content]
          -> XSDQ (Maybe (String, Maybe String, Maybe String, QName,
                          [Attr], [Content], Maybe Line),
                   [Content], [Content])
        separateComplexContents' primary annAcc annGroupAcc [] =
          return (primary, reverse annAcc, reverse annGroupAcc)
        separateComplexContents' pr as ags (e:xs) =
          case e of
            Elem (Element q@(QName n u p) a c l) -> case n of
              "attribute" -> separateComplexContents' pr (e:as) ags xs
              "attributeGroup" -> separateComplexContents' pr (e:as) ags xs
              "annotation" -> separateComplexContents' pr as ags xs
              "documentation" -> separateComplexContents' pr as ags xs
              _ -> case pr of
                Nothing -> separateComplexContents'
                             (Just (n, u, p, q, a, c, l))
                             as ags xs
                Just (n', _, _, _, _, _, _) -> error $
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
