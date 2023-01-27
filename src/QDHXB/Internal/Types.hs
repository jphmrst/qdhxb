
-- | Internal representation of flattened XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  Reference(ElementRef, AttributeRef {-, ComplexTypeRef -} ),
  Definition(SimpleTypeDefn, AttributeDefn, SequenceDefn, ElementDefn),
  AttributeUsage(Forbidden, Optional, Required), stringToAttributeUsage,
  pprintDefns'
  ) where

import Data.List (intercalate)
import Text.XML.Light.Types

-- | A reference to an XSD element.
data Reference =
  ElementRef QName (Maybe Int) (Maybe Int)
  -- ^ A named element type, possibly with numeric instance bounds.
  | AttributeRef QName AttributeUsage
  -- ^ The name of an attribute.
{-
  | ComplexTypeRef String
  -- ^ The name of a complex type.
-}
  deriving Show

-- | The actual definition of an XSD element.
data Definition =
  ElementDefn QName QName
  -- ^ Defining an element to be of a particular type.
  | AttributeDefn QName QName
  -- ^ Defining the type of an attribute to be the same as another.
  | SimpleTypeDefn QName QName
  -- ^ Defining one type to have the same structure as another.
  | SequenceDefn String [Reference]
  -- ^ Define a complex type as a sequence of subelements.
  deriving Show

-- | Enumeration encoding the valid values of the XSD attribute
-- definition's "usage" attribute.
data AttributeUsage = Forbidden | Optional | Required
  deriving (Eq, Show)

-- | Convert a `String` to an `AttributeUsage` value.
stringToAttributeUsage :: String -> AttributeUsage
stringToAttributeUsage "forbidden" = Forbidden
stringToAttributeUsage "required"  = Required
stringToAttributeUsage _ = Optional

-- | Display a list of `Definition` in more human-readable text.
pprintDefns' :: String -> [Definition] -> String
pprintDefns' ind ds = intercalate ("\n" ++ ind) $ map show ds
