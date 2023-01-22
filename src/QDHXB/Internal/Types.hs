
-- | Internal representation of flattened XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  Reference(ElementRef, AttributeRef,  ComplexTypeRef),
  Definition(SimpleTypeDefn, AttributeDefn, SequenceDefn),
  AttributeUsage(Forbidden, Optional, Required), stringToAttributeUsage
  ) where

-- | A reference to an XSD element.
data Reference =
  ElementRef String (Maybe Int) (Maybe Int)
  -- ^ A named element type, possibly with numeric instance bounds.
  | AttributeRef String AttributeUsage
  -- ^ The name of an attribute.
  | ComplexTypeRef String
  -- ^ The name of a complex type.
  deriving Show

-- | The actual definition of an XSD element.
data Definition =
  SimpleTypeDefn String String
  -- ^ Defining one element to have the same structure as another.
  | AttributeDefn String String
  -- ^ Defining the type of an attribute to be the same as another.
  | SequenceDefn String [Reference]
  -- ^ Define an element to contain a sequence of subelements.
  deriving Show

data AttributeUsage = Forbidden | Optional | Required
  deriving (Eq, Show)

stringToAttributeUsage :: String -> AttributeUsage
stringToAttributeUsage "forbidden" = Forbidden
stringToAttributeUsage "required"  = Required
stringToAttributeUsage _ = Optional

