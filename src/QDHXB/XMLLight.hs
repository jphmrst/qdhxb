{-# LANGUAGE TemplateHaskell #-}

-- | Utilities based on the @XMLLight@ library.
module QDHXB.XMLLight (pullAttr, pullAttrFrom, pullContent, pullContentFrom,
                       pullCRef, pullCRefContent,
                       isElem,
                       ZeroOneMany(Zero, One, Many),
                       zomToList, zappend, lzappend,
                       loadElement)
where
import System.IO
import Text.XML.Light.Input
import Text.XML.Light.Types

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

-- | Retrieve the named attribute value from a single content element.
pullCRefContent :: String -> Content -> Maybe String
pullCRefContent cname (Elem (Element (QName n _ _) _ [sub] _)) | cname == n
  = pullCRef sub
pullCRefContent _ _ = Nothing

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
pullContentFrom name (Elem (Element _ _ contents _)) = pullContent name contents
pullContentFrom _ (Text _) = Zero
pullContentFrom _ (CRef _) = Zero

-- | Predicate testing whether a piece of XML `Content` is an `Elem`.
isElem :: Content -> Bool
isElem (Elem _) = True
isElem _ = False

-- | Explicit tagging of whether a list has zero, one, or more than
-- one elements.
data ZeroOneMany a = Zero -- ^ Zero elements
  | One a    -- ^ One element
  | Many [a] -- ^ More then one element
  deriving Show

-- | Convert a `ZeroOneMany` type to a list type.
zomToList :: ZeroOneMany a -> [a]
zomToList Zero = []
zomToList (One m) = [m]
zomToList (Many ms) = ms

-- | Append together two `ZeroOneMany` values.
zappend :: ZeroOneMany a -> ZeroOneMany a -> ZeroOneMany a
zappend Zero m = m
zappend m Zero = m
zappend (One s) (One t) = Many [s, t]
zappend (One s) (Many ts) = Many $ s : ts
zappend (Many ss) (One t) = Many $ ss ++ [t]
zappend (Many ss) (Many ts) = Many $ ss ++ ts

-- | Append together a list values and a `ZeroOneMany` value as
-- another `ZeroOneMany` value.
lzappend :: [a] -> ZeroOneMany a -> ZeroOneMany a
lzappend [] m = m
lzappend [s] Zero = One s
lzappend ms Zero = Many ms
lzappend [s] (One t) = Many [s, t]
lzappend ms  (One t) = Many $ ms ++ [t]
lzappend [s] (Many ns) = Many $ s : ns
lzappend ms  (Many ns) = Many $ ms ++ ns

-- | Using the given decoder for an XMLLight `Content` structure,
-- extract a decoded value from an XML file.
loadElement :: (Content -> a) -> String -> IO a
loadElement decoder xmlFile = do
  xml <- fmap parseXML $ readFile' xmlFile
  case filter isElem xml of
    ((Elem (Element (QName "?xml" _ _) _ _ _)) : ds) ->
      case ds of
        e:[] -> return $ decoder e
        _:_  -> error "Expected a single top-level element, found multiple"
        _    -> error "Missing top-level element"
    _ -> error "Missing <?xml> element"

