
-- | Internal representation of flattened XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  Reference(ElementRef, AttributeRef {-, ComplexTypeRef -} ),
  Definition(SimpleSynonymDefn, AttributeDefn, SequenceDefn, ElementDefn),
  AttributeUsage(Forbidden, Optional, Required), stringToAttributeUsage,
  pprintDefns'
  ) where

import Data.List (intercalate)
import Text.XML.Light.Types
import Text.XML.Light.Output (showQName)
import QDHXB.Internal.Utils.BPP

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

instance Blockable Reference where
  block (ElementRef name ifLower ifUpper) = stringToBlock $
    "ElementRef " ++ showQName name
    ++ case ifLower of
         Nothing -> " no lower bound"
         Just n -> " lower bound=" ++ show n
    ++ case ifUpper of
         Nothing -> " no upper bound"
         Just n -> " upper bound=" ++ show n
  block (AttributeRef name usage) = stringToBlock $
    "AttributeRef " ++ showQName name ++ " usage=" ++ show usage
instance VerticalBlockList Reference


-- | The actual definition of an XSD element.
data Definition =
  ElementDefn QName QName
  -- ^ Defining an element to be of a particular type.
  | AttributeDefn QName QName
  -- ^ Defining the type of an attribute to be the same as another.
  | SimpleSynonymDefn QName QName
  -- ^ Defining one type to have the same structure as another.
  | SequenceDefn String [Reference]
  -- ^ Define a complex type as a sequence of subelements.
  deriving Show

instance Blockable Definition where
  block (ElementDefn n t) = stringToBlock $
    "ElementDefn " ++ showQName n ++ " :: " ++ showQName t
  block (AttributeDefn n t) = stringToBlock $
    "AttributeDefn " ++ showQName n ++ " :: " ++ showQName t
  block (SimpleSynonymDefn n t) = stringToBlock $
    "SimpleSynonymDefn " ++ showQName n ++ " :: " ++ showQName t
  block (SequenceDefn n rs) = stackBlocks $
    (stringToBlock $ "SequenceDefn " ++ n) : map block rs
instance VerticalBlockList Definition

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
