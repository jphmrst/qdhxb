
-- | Dealing with namespaces
module QDHXB.Internal.Utils.Namespaces
  (Namespaces, decodePrefixed, decodeAttrsForNamespaces)
where
import Data.List (stripPrefix)
import Text.XML.Light.Types
import Text.XML.Light.Proc (lookupAttrBy)

-- | Association list from namespace prefixes to the underlying URLs.
type Namespaces = [(String, String)]

-- | Utility for decoding attributes which are
decodePrefixed ::
  Maybe String -> Maybe String -> Namespaces -> [Namespaces] -> String -> QName
decodePrefixed defaultPfx defaultUri [] [] s = QName s defaultUri defaultPfx
decodePrefixed dp du ((p,u):ns) nss s =
  case stripPrefix (p ++ ":") s of
    Nothing -> decodePrefixed dp du ns nss s
    Just val -> QName val (Just u) (Just p)
decodePrefixed dp du [] (ns:nss) s = decodePrefixed dp du ns nss s

-- | Extract namespace declarations from the attributes of
-- (presumably) an XSD <schema> element.
decodeAttrsForNamespaces :: [Attr] -> (Namespaces, Maybe String)
decodeAttrsForNamespaces attrs =
  let defaultNS = lookupAttrBy (\q -> qName q == targetNamespaceStr &&
                                      case qURI q of
                                        Nothing -> True
                                        Just uri -> uri == xsdUrlStr) attrs
  in (extractNamespacePrefixes attrs, defaultNS)

extractNamespacePrefixes :: [Attr] -> [(String, String)]
extractNamespacePrefixes [] = []
extractNamespacePrefixes ((Attr (QName keyName _ keyPrefixM) val):attrs) =
  let rest = extractNamespacePrefixes attrs
  in case keyPrefixM of
       Just p | p == xmlnsStr -> (keyName, val) : rest
       _ -> rest

targetNamespaceStr :: String
targetNamespaceStr = "targetNamespace"

xsdUrlStr :: String
xsdUrlStr = "http://www.w3.org/2001/XMLSchema"

xmlnsStr :: String
xmlnsStr = "xmlns"
