{-# LANGUAGE TemplateHaskell #-}

-- | Utilities based on the @XMLLight@ library.
module QDHXB.XMLLight (pullAttr, pullContent, isElem,
                       ZeroOneMany(Zero, One, Many),
                       zomToList, zappend, lzappend)
where
import Text.XML.Light.Types

-- | Retrieve the named attribute value from a list of `Attr`
-- bindings.
pullAttr :: String -> [Attr] -> Maybe String
pullAttr _ [] = Nothing
pullAttr nam ((Attr (QName nam' _ _) val) : ats) =
  if nam == nam' then Just val else pullAttr nam ats

-- | Retrieve XML contents with the given name.
pullContent :: String -> [Content] -> ZeroOneMany Content
pullContent _ [] = Zero
pullContent nam (c@(Elem (Element (QName n _ _) _ _ _)) : cs) | nam == n =
  case pullContent nam cs of
    Zero -> One c
    One c' -> Many [c, c']
    Many cs' -> Many $ c : cs'
pullContent nam (_:cs) = pullContent nam cs

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

