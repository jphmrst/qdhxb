
-- | Utilities based on the @XMLLight@ library.
module QDHXB.Utils.XMLLight (
  -- * Top-level separation of an XSD file into its primary sections
  partitionXMLForms, getCoreContent,
  -- * Extracting parts of `Content` and other document representations
  -- ** Attributes
  pullAttr, pullAttrFrom,
  -- ** `Content` lists
  pullContent, getAnnotationDocFrom, filterTagged,
  -- ** Subcomponents of a single `Content`
  pullContentFrom, pullCRef, pullCRefContent, pullCRefOf,
  getAnnotationDoc,
  -- ** Loading from a file
  __loadElement, __loadContent,
  -- * Introspection on names
  loadElementName,
  -- ** Predictates on `Content`s
  isElem, isNonKeyElem, isNonKeyNonNotationElem,
  isFocusElem, isTagged,
  -- * Manipulating `QName`s
  withPrefix, withSuffix, withNamePrefix, withNameSuffix, qnFirstToUpper,
  inSameNamspace,
  -- * Decoding a `QName`
  stringToQName, contextfreeStringToQName)
where
import Data.List (find)
import Data.List.Split (splitOn)
import Language.Haskell.TH (mkName, Name)
import System.IO
import Control.Monad.Except
import Text.XML.Light.Input
import Text.XML.Light.Types
import QDHXB.Utils.Misc
import QDHXB.Utils.Namespaces
import QDHXB.Utils.TH (firstToUpper)
import QDHXB.Utils.ZeroOneMany

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
getAnnotationDoc :: MonadError e m => Content -> m (Maybe String)
getAnnotationDoc c = do
  annContents <- zomToList $ pullContentFrom "annotation" c
  case annContents of
    [] -> return $ Nothing
    ann:_ -> do
      docContents <- zomToList $ pullContentFrom "documentation" ann
      case docContents of
        [] -> return $ Nothing
        doc:_ -> return $ fmap chomp $ pullCRef doc

-- |Attempt to retrieve a
-- @\<annotation>\<documentation>...\<\/documentation>\<\/annotation>@
-- string from the given list of subcontents.  Any leading/trailing
-- whitespace will be pruned from the result.
getAnnotationDocFrom :: MonadError e m => [Content] -> m (Maybe String)
getAnnotationDocFrom cs = do
  annContents <- zomToList $ pullContent "annotation" cs
  case annContents of
    [] -> return Nothing
    ann:_ -> do
      docContents <- zomToList $ pullContentFrom "documentation" ann
      case docContents of
        [] -> return Nothing
        doc:_ -> return $ fmap chomp $ pullCRefContent doc

-- | Retrieve the named attribute value from a single content element.
pullCRefContent :: Content -> Maybe String
pullCRefContent (Elem (Element (QName _ _ _) _ [sub] _)) = pullCRef sub
pullCRefContent _ = Nothing

-- | Retrieve the named attribute value from a single content element.
pullCRefOf :: Content -> Maybe String
pullCRefOf (Elem (Element (QName _ _ _) _ [sub] _)) = pullCRef sub
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

-- | Predicate testing whether a piece of XML `Content` is an
-- `Element` which is not a @<key>@.
isFocusElem :: Content -> Bool
isFocusElem (Elem (Element (QName "key" _ _) _ _ _)) = False
isFocusElem (Elem (Element (QName "notation" _ _) _ _ _)) = False
isFocusElem (Elem (Element (QName "annotation" _ _) _ _ _)) = False
isFocusElem (Elem (Element _ _ _ _)) = True
isFocusElem _ = False

-- | Using the given decoder for an XMLLight `Content` structure,
-- extract a decoded value from an XML file.
__loadContent :: String -> IO Content
__loadContent xmlFile = do
  xml <- fmap parseXML $ readFile' xmlFile
  case filter isElem xml of
    ((Elem (Element (QName "?xml" _ _) _ _ _)) : ds) ->
      case ds of
        e:[] -> return e
        _:_  -> error "Expected a single top-level element, found multiple"
        _    -> error "Missing top-level element"
    [e] -> return e
    _ -> error "No elements in XML file"

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

-- | Return a new `QName` with the given `String` prepended to the
-- original `qName`.
qnFirstToUpper :: QName -> QName
qnFirstToUpper (QName name u p) = QName (firstToUpper name) u p

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

-- | Divide the list of top-level `Content` items into the interesting
-- groups for an XSD document:
--  (1) Leading <? ... ?> tags,
--  (2) The top-level <schema> block, if it exists, and
--  (3) Anything else.
partitionXMLForms :: [Content] -> ([Content],  Maybe Content, [Content])
partitionXMLForms = partition_forms [] Nothing []
  where partition_forms ::
          [Content] -> Maybe Content -> [Content] -> [Content]
          -> ([Content],  Maybe Content, [Content])
        partition_forms leads main other [] = (reverse leads, main, other)
        partition_forms leads main other
                        (c@(Elem (Element (QName ('?':_) _ _) _ _ _)):cs) =
          partition_forms (c:leads) main other cs
        partition_forms leads Nothing other
                        (c@(Elem (Element (QName "schema" _ _) _ _ _)):cs) =
          partition_forms leads (Just c) other cs
        partition_forms _ (Just _) _
                        (Elem (Element (QName "schema" _ _) _ _ _):_) =
          error "Multiple top-level <schema> blocks"
        partition_forms leads main other (c:cs) =
          partition_forms leads main (c:other) cs

-- | Find the core `Content` corresponding to an XML scheme in the
-- list of `Content` returned from parsing an XSD file.
getCoreContent :: [Content] -> Content
getCoreContent cs = case partitionXMLForms cs of
  (_, Nothing, _) -> error "Expected top-level <schema> element"
  (_, Just c, _) -> c

-- | Build a new `QName` in the same namespace as an existing `QName`.
inSameNamspace :: String -> QName -> QName
inSameNamspace str (QName _ url pfx) = QName str url pfx

-- | Convert a `String` to a `QName` given the current `Namespaces`.
stringToQName :: Namespaces -> String -> QName
stringToQName ns s = case splitOn ":" s of
  [] -> error "Cannot make a QName from an empty string"
  [_] -> QName s Nothing Nothing
  [pfx, core] -> QName core (fmap snd $ find ((==pfx) . fst) ns) $ Just pfx
  _ -> error $ "Too many colons in string for QName: \"" ++ s ++ "\""

-- | Convert a `String` to a `QName` with reference to no `Namespaces`
-- lookup.
contextfreeStringToQName :: String -> QName
contextfreeStringToQName s = case splitOn ":" s of
  [] -> error "Cannot make a QName from an empty string"
  [_] -> QName s Nothing Nothing
  [pfx, core] -> QName core Nothing $ Just pfx
  _ -> error $ "Too many colons in string for QName: \"" ++ s ++ "\""
