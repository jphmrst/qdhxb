{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.Input (inputSchemaItems) where

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
inputSchemaItems :: [Content] -> XSDQ [DataScheme]
inputSchemaItems items = do
  res <- mapM inputSchemaItem items
  -- liftIO $ putStrLn $ show res
  return res

inputSchemaItem :: Content -> XSDQ DataScheme
inputSchemaItem e@(Elem (Element q a c l)) = do
  whenDebugging $ liftIO $ putStrLn $ "> Encoding element " ++ showContent e
  res <- inputElement q a c l $ getAnnotationDocFrom c
  whenDebugging $ liftIO $ bLabelPrintln "    `--> " res
  return res
inputSchemaItem (Text _) = do
  whenDebugging $ liftIO $ putStrLn "> Dropping Text entry "
  return Skip
inputSchemaItem (CRef txt) = do
  whenDebugging $ liftIO $ putStrLn $ "> Dropping CRef entry " ++ txt
  return Skip

inputElement ::
  QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String -> XSDQ DataScheme
inputElement (QName "element" _ _) ats content ln _d = do
  included <- inputSchemaItems $ filter isElem content
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
inputElement q@(QName "attribute" _ _) a c l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute q a c l ifDoc
  return $ AttributeScheme scheme l ifDoc
inputElement q@(QName "attributeGroup" _ _) a c l _d = do
  let ifDoc = getAnnotationDocFrom c
  scheme <- encodeAttribute q a c l ifDoc
  return $ AttributeScheme scheme l ifDoc
inputElement (QName "complexType" _ _) ats ctnts l d = do
  (pr, atspecs', atgrspecs') <- separateComplexContents ctnts l
  name <- pullAttrQName "name" ats
  case pr of
    Nothing -> do
      atrSpecs <- mapM encodeAttributeScheme $ atspecs' ++ atgrspecs'
      return $ ComplexTypeScheme (Composing [] atrSpecs) [] name l d
    Just (_, tag, uri, pfx, qn, ats', subctnts, _) -> case tag of
      "sequence" -> do
        ct <- encodeSequenceTypeScheme subctnts (atspecs'++atgrspecs')
        return $ ComplexTypeScheme ct [] name l d
      "choice" -> do
        whenDebugging $ liftIO $
          putStrLn "      inputElement > complexType > case \"choice\""
        error "inputElement > complexType > case \"choice\""
      "complexContent" -> do
        whenDebugging $ liftIO $ putStrLn
          "      inputElement > complexType > case \"complexContent\""
        (pr', _, _) <- separateComplexContents subctnts l
        case pr' of
          Just (_, _, _, _, sqn, sats', ssubctnts, ssline) ->
            inputElement sqn sats' ssubctnts ssline Nothing
          Nothing -> error $
            "Complex content must have primary subcontents" ++ ifAtLine l
        -- error "inputElement > complexType > case \"complexContent\""
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
          "      inputElement > complexType > case \"simpleContent\""
        error "inputElement > complexType > case \"simpleContent\""
      _ -> do
        liftIO $ do
          putStrLn      "+--------"
          putStrLn      "| inputElement > complexType > another tag case"
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
        error "inputElement > complexType > unmatched"
inputElement (QName "simpleType" _ _) ats ctnts ifLn ifDoc = do
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
         alts <- inputSchemaItems $ filter isElem cs'
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
          "| TODO inputSchemaItem > simpleType > another separation case"
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| CTNTS' "
          ++ (intercalate "\n    " $ map showContent ctnts')
        putStrLn $ "| IFNAME " ++ show ifName
        putStrLn $ "| ZOMRESTR " ++ (show $ zomToList zomRestr)
        putStrLn $ "| ZOMUNION " ++ (show $ zomToList zomUnion)
        putStrLn $ "| ZOMLIST "  ++ (show zomList)
      error $ "TODO inputElement > simpleType > another separation case"
        ++ ifAtLine ifLn
inputElement (QName "annotation" _ _) _ _ _ _ = do
  -- We do nothing with documentation and other annotations; currently
  -- there is no way to pass Haddock docstrings via the TH API.
  whenDebugging $ liftIO $ putStrLn $ "> Dropping <annotation> element"
  return Skip
inputElement (QName tag _ _) _ _ l _d | tag=="include" || tag=="import" = do
  -- Skipping these documents for now
  liftIO $ putStrLn $
    "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
  return Skip
inputElement (QName tagname _ _) _ _ _ _
  | tagname == "key" || tagname == "keyref" = do
  whenDebugging $ liftIO $ putStrLn $ "  - Dropping <" ++ tagname ++ "> entry "
  return Skip
inputElement (QName "sequence" _ _) ats ctnts ifLn ifDoc = do
  ifName <- pullAttrQName "name" ats
  included <- inputSchemaItems ctnts
  return $ ComplexTypeScheme (Composing included []) [] ifName ifLn ifDoc
inputElement (QName "restriction" _ _) ats ctnts ifLn ifDoc = do
  let ifDoc' = getAnnotationDocFrom ctnts
  ifName <- pullAttrQName "name" ats
  case pullAttr "base" ats of
    Just base -> do
      baseQName <- decodePrefixedName base
      return $
        ComplexTypeScheme (ComplexRestriction baseQName) [] ifName ifLn
                          (pickOrCombine ifDoc ifDoc')
    Nothing -> error "restriction without base"
inputElement (QName "extension" _ _) ats ctnts ifLn ifDoc = do
  maybeBase <- pullAttrQName "base" ats
  let base = maybe (error $ "<extension> without base" ++ ifAtLine ifLn)
                id maybeBase
  ifName <- pullAttrQName "name" ats
  (ext, newAttrs, newAttrGroups) <- separateComplexContents ctnts ifLn
  whenDebugging $ liftIO $ do
    putStrLn "      - Complex extension"
    bLabelPrintln "        - base " base
    bLabelPrintln "        - ext " ext
    bLabelPrintln "        - newAttrs " newAttrs
    bLabelPrintln "        - newAttrGroups " newAttrGroups
  case ext of
    Nothing -> do
      return $
        ComplexTypeScheme (Extension base []) [] ifName ifLn ifDoc
    Just (e,_,_,_,_,_,_,_) -> do
      ext <- inputSchemaItem e
      return $
        ComplexTypeScheme (Extension base [ext]) [] ifName ifLn ifDoc

inputElement (QName "group" _ _) ats ctnts ifLn ifDoc = do
  whenDebugging $ liftIO $ putStrLn "  - For <group> schema:"
  name <- pullAttrQName "name" ats
  case filter isElem ctnts of
    (Elem (Element (QName "choice" _ _) attrs' ctnts' ifLn')):[] -> do
      ts <- encodeChoiceTypeScheme name attrs' ctnts'
      return $ GroupScheme name (Just ts) ifLn' ifDoc
    (Elem (Element (QName "sequence" _ _) _ats ctnts ifLn')):[] -> do
      seq <- encodeSequenceTypeScheme ctnts []
      return $ GroupScheme name (Just seq) ifLn ifDoc
      liftIO $ do
        putStrLn $ "+--------"
        putStrLn $ "| Case 1 after (filter isElem ctnts)"
        putStrLn $ "| group ATTRS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| sequence ATTRS " ++ (intercalate "\n    " $
                                           map showAttr _ats)
        putStrLn $ "| NAME " ++ show (fmap showQName name)
        putStrLn $ "| CTNTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO inputSchemaItem > group with sequence" ++ ifAtLine ifLn'
    (Elem (Element (QName "all" _ _) _ats _contents ifLn')):[] -> do
      -- let ifDoc' = getAnnotationDocFrom contents
      liftIO $ do
        putStrLn $ "+-------"
        putStrLn $
          "| TODO inputSchemaItem > group with all" ++ ifAtLine ifLn'
        putStrLn $ "| ATS " ++ (intercalate "\n    " $ map showAttr ats)
        putStrLn $ "| NAME " ++ show (fmap showQName name)
        putStrLn $ "| CTNTS " ++
          (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
      error $ "TODO inputSchemaItem > group with all" ++ ifAtLine ifLn'
    _ -> do
      liftIO $ putStrLn $ "  - Default is group of nothing"
      return $ GroupScheme name Nothing ifLn ifDoc
inputElement (QName tag _ _) ats ctnts ifLn _ifDoc = do
  -- whenDebugging $ do
  liftIO $ do
    putStrLn "+-------"
    putStrLn $
      "| TODO inputSchemaItem > another Element case" ++ ifAtLine ifLn
    putStrLn $ "| TAG " ++ show tag
    bLabelPrintln "| ATS " ats
    bLabelPrintln "| CTNTS " $ filter isElem ctnts
  error $ "TODO inputSchemaItem > another Element case" ++ ifAtLine ifLn

encodeSequenceTypeScheme :: [Content] -> [Content] -> XSDQ ComplexTypeScheme
encodeSequenceTypeScheme subcontents attrSpecs = do
  included <- inputSchemaItems subcontents
  atrSpecs <- mapM encodeAttributeScheme attrSpecs
  return $ Composing included atrSpecs

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
  contentSchemes <- mapM inputSchemaItem ctnts
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
