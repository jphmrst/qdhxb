{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Internal representation of flattened XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  Reference(..),
  AttributeDefn(..),
  Definition(..),
  AttributeUsage(Forbidden, Optional, Required),
  stringToAttributeUsage, pprintDefns'
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

-- | Definition of an attribute or group type.
data AttributeDefn =
  SingleAttributeDefn -- ^ Defining a single attribute
      QName   -- ^ ifType
      String  -- ^ use mode: prohibited, optional (default), required
  | AttributeGroupDefn -- ^ Defining a group of attributes
      [QName] -- ^ names of included attributes and attribute groups
  deriving Show

instance Blockable AttributeDefn where
  block (SingleAttributeDefn t m) = stringToBlock $
    "Single " ++ showQName t ++ " (" ++ m ++ ")"
  block (AttributeGroupDefn ds) =
    labelBlock "Group " $ stackBlocks $ map (stringToBlock . showQName) ds

-- | The actual definition of an XSD element.
data Definition =
  ElementDefn QName QName
  -- ^ Defining an element to be of a particular type.
  | AttributeDefn QName AttributeDefn
    -- ^ Defining the attributes and groups.
  | SimpleSynonymDefn QName QName
    -- ^ Defining one type to have the same structure as another.
  | SequenceDefn QName [Reference] [AttributeDefn]
    -- ^ Define a complex type as a sequence of subelements.
  | UnionDefn
    -- ^ Define a simple type as a union of other simple types.
      QName
      -- ^ Name of the type
      [(QName, QName)]
      -- ^ (Constructor name, type name) of each element of the union.
  | ListDefn
    -- ^ Define a simple type as a list of another simple type.
      QName
      -- ^ Name of the list type
      QName
      -- ^ Name of the element type
  deriving Show

instance Blockable Definition where
  block (ElementDefn n t) = stringToBlock $
    "ElementDefn " ++ showQName n ++ " :: " ++ showQName t
  block (AttributeDefn n sp) =
    labelBlock ("Attribute " ++ showQName n ++ " ") $ block sp
  block (SimpleSynonymDefn n t) = stringToBlock $
    "SimpleSynonymDefn " ++ showQName n ++ " :: " ++ showQName t
  block (SequenceDefn n rs ats) = stackBlocks $
    (stringToBlock $ "SequenceDefn " ++ qName n) : map block rs ++ map block ats
  block (UnionDefn n ns) = stackBlocks $
    (stringToBlock $ "UnionDefn " ++ showQName n)
    : map (indent "  " . uncurry horizontalPair) ns
  block (ListDefn n t) = stringToBlock $
    "ListDefn " ++ showQName n ++ " :: [" ++ showQName t ++ "]"
instance VerticalBlockList Definition
instance VerticalBlockablePair QName [Definition]
instance VerticalBlockList (QName, [Definition])

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
