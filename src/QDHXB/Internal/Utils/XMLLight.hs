
-- | Utilities based on the @XMLLight@ library.
module QDHXB.Internal.Utils.XMLLight (
  pullAttr, pullAttrFrom, pullContent, pullContentFrom,
    pullCRef, pullCRefContent, pullCRefOf,
    getAnnotationDoc, getAnnotationDocFrom,
    filterTagged, isElem, isNonKeyElem, isNonKeyNonNotationElem, isTagged,

    __loadElement, loadElementName,
    withPrefix, withSuffix, withNamePrefix, withNameSuffix)
where
import Language.Haskell.TH (mkName, Name)
import System.IO
import Text.XML.Light.Input
import Text.XML.Light.Types
import QDHXB.Internal.Utils.Misc
import QDHXB.Internal.Utils.ZeroOneMany

-- | Retrieve the named attribute value from a list of `Attr`
-- bindings.
pullAttr :: String -> [Attr] -> Maybe String
pullAttr _ [] = Nothing
pullAttr nam ((Attr (QName nam' _ _) val) : ats) =
  if nam == nam' then Just val else pullAttr nam ats

-- | Retrieve the named attribute value from a single content element.
pullAttrFrom :: String -> Content -> Maybe String
pullAttrFrom name (Elem (Element _ attrs _ _)) = pullAttr name attrs
pullAttrFrom _ (Text _) = Nothing
pullAttrFrom _ (CRef _) = Nothing

-- | Retrieve the named attribute value from a single content element.
pullCRef :: Content -> Maybe String
pullCRef (Text (CData _ str _)) = Just str
-- pullCRef (CRef str) = Just str
pullCRef _ = Nothing

-- |Attempt to retrieve a
-- @\<annotation>\<documentation>...\<\/documentation>\<\/annotation>@
-- string contained within the given node's subcontents.  Any
-- leading/trailing whitespace will be pruned from the result.
getAnnotationDoc :: Content -> Maybe String
getAnnotationDoc c = case zomToList $ pullContentFrom "annotation" c of
  [] -> Nothing
  ann:_ -> case zomToList $ pullContentFrom "documentation" ann of
    [] -> Nothing
    doc:_ -> fmap chomp $ pullCRef doc

-- |Attempt to retrieve a
-- @\<annotation>\<documentation>...\<\/documentation>\<\/annotation>@
-- string from the given list of subcontents.  Any leading/trailing
-- whitespace will be pruned from the result.
getAnnotationDocFrom :: [Content] -> Maybe String
getAnnotationDocFrom cs = case zomToList $ pullContent "annotation" cs of
  [] -> Nothing
  ann:_ -> case zomToList $ pullContentFrom "documentation" ann of
    [] -> Nothing
    doc:_ -> fmap chomp $ pullCRefContent "documentation" doc

-- | Retrieve the named attribute value from a single content element.
pullCRefContent :: String -> Content -> Maybe String
pullCRefContent cname (Elem (Element (QName n _ _) _ [sub] _)) | cname == n
  = pullCRef sub
pullCRefContent _ _ = Nothing

-- | Retrieve the named attribute value from a single content element.
pullCRefOf :: Content -> Maybe String
pullCRefOf (Elem (Element (QName n _ _) _ [sub] _)) = pullCRef sub
pullCRefOf _ = Nothing

-- | Retrieve XML contents with the given name.
pullContent :: String -> [Content] -> ZeroOneMany Content
pullContent _ [] = Zero
pullContent nam (c@(Elem (Element (QName n _ _) _ _ _)) : cs) | nam == n =
  case pullContent nam cs of
    Zero -> One c
    One c' -> Many [c, c']
    Many cs' -> Many $ c : cs'
pullContent nam (_:cs) = pullContent nam cs

-- | Retrieve XML contents with the given name from the contained
-- elements of the given element.
pullContentFrom :: String -> Content -> ZeroOneMany Content
pullContentFrom name (Elem (Element _ _ contents _)) =
  pullContent name contents
pullContentFrom _ (Text _) = Zero
pullContentFrom _ (CRef _) = Zero

-- | Return the `Content` nodes which are elements with the given tag.
filterTagged :: String -> [Content] -> [Content]
filterTagged s = filter $ isTagged s

-- | Predicate testing whether a piece of XML `Content` has the given
-- tag.
isTagged :: String -> Content -> Bool
isTagged t (Elem (Element (QName t' _ _) _ _ _)) | t == t' = True
isTagged _ _ = False

-- | Predicate testing whether a piece of XML `Content` is an `Elem`.
isElem :: Content -> Bool
isElem (Elem _) = True
isElem _ = False

-- | Predicate testing whether a piece of XML `Content` is an
-- `Element` which is not a @<key>@.
isNonKeyElem :: Content -> Bool
isNonKeyElem (Elem (Element (QName "key" _ _) _ _ _)) = False
isNonKeyElem (Elem (Element _ _ _ _)) = True
isNonKeyElem _ = False

-- | Predicate testing whether a piece of XML `Content` is an
-- `Element` which is not a @<key>@.
isNonKeyNonNotationElem :: Content -> Bool
isNonKeyNonNotationElem (Elem (Element (QName "key" _ _) _ _ _)) = False
isNonKeyNonNotationElem (Elem (Element (QName "notation" _ _) _ _ _)) = False
isNonKeyNonNotationElem (Elem (Element _ _ _ _)) = True
isNonKeyNonNotationElem _ = False

-- | Using the given decoder for an XMLLight `Content` structure,
-- extract a decoded value from an XML file.
__loadElement :: (Content -> a) -> String -> IO a
__loadElement decoder xmlFile = do
  xml <- fmap parseXML $ readFile' xmlFile
  case filter isElem xml of
    ((Elem (Element (QName "?xml" _ _) _ _ _)) : ds) ->
      case ds of
        e:[] -> return $ decoder e
        _:_  -> error "Expected a single top-level element, found multiple"
        _    -> error "Missing top-level element"
    [e] -> return $ decoder e
    _ -> error "No elements in XML file"

-- | Template Haskell `Name` for the `QDHXB.Expansions.__loadElement`
-- re-export of the `__loadElement` method.
loadElementName :: Name
loadElementName = mkName "QDHXB.Expansions.__loadElement"

-- | Return a new `QName` with the given `String` prepended to the
-- original `qName`.
withPrefix :: String -> QName -> QName
withPrefix prefix (QName name u p) = QName (prefix ++ name) u p

-- | Return a new `QName` with the given `String` appended to the
-- original `qName`.
withSuffix :: String -> QName -> QName
withSuffix suffix (QName name u p) = QName (name ++ suffix) u p

-- | Return a new `QName` with the `qName` of the first `QName`
-- prepended to the `qName` of the second `QName`.
withNamePrefix :: QName -> QName -> QName
withNamePrefix prefix (QName name u p) = QName (qName prefix ++ name) u p

-- | Return a new `QName` with the `qName` of the first `QName`
-- appended to the `qName` of the second `QName`.
withNameSuffix :: QName -> QName -> QName
withNameSuffix suffix (QName name u p) = QName (name ++ qName suffix) u p
