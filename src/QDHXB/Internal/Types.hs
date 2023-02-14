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
  ElementRef
  -- ^ A named element type, possibly with numeric instance bounds.
      QName (Maybe Int) (Maybe Int) (Maybe Line)
  | AttributeRef
    -- ^ The name of an attribute.
       QName AttributeUsage
{-
  | ComplexTypeRef String
  -- ^ The name of a complex type.
-}
  deriving Show

instance Blockable Reference where
  block (ElementRef name ifLower ifUpper _) = stringToBlock $
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
      AttributeUsage -- ^ use mode: prohibited, optional (default), required
  | AttributeGroupDefn -- ^ Defining a group of attributes
      [QName] -- ^ names of included attributes and attribute groups
       -- TODO Should be (QName, AttributeUsage)
  deriving Show

instance Blockable AttributeDefn where
  block (SingleAttributeDefn t m) = stringToBlock $
    "Single " ++ showQName t ++ " (" ++ show m ++ ")"
  block (AttributeGroupDefn ds) =
    labelBlock "Group " $ stackBlocks $ map (stringToBlock . showQName) ds

-- | The actual definition of an XSD element.
data Definition =
  ElementDefn
  -- ^ Defining an element to be of a particular type.
      QName QName
      (Maybe Line) -- ^ ifLine
      (Maybe String) -- ^ Documentation string, if available
  | AttributeDefn
    -- ^ Defining the attributes and groups.
        QName AttributeDefn
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | SimpleSynonymDefn
    -- ^ Defining one type to have the same structure as another.
        QName QName
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | SequenceDefn
    -- ^ Define a complex type as a sequence of subelements.
        String [Reference]
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | UnionDefn
    -- ^ Define a simple type as a union of other simple types.
        QName
        -- ^ Name of the type
        [(QName, QName)]
        -- ^ (Constructor name, type name) of each element of the union.
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | ListDefn
    -- ^ Define a simple type as a list of another simple type.
        QName
        -- ^ Name of the list type
        QName
        -- ^ Name of the element type
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  deriving Show

instance Blockable Definition where
  block (ElementDefn n t _ dm) =
    (stringToBlock $
     "ElementDefn " ++ showQName n ++ " :: " ++ showQName t)
    `stack2` (stringToBlock $ case dm of
                 Nothing -> "  no doc"
                 Just d -> "  doc=\"" ++ d ++ "\"")
  block (AttributeDefn n sp _ln _d) =
    labelBlock ("Attribute " ++ showQName n ++ " ") $ block sp
  block (SimpleSynonymDefn n t _ _d) = stringToBlock $
    "SimpleSynonymDefn " ++ showQName n ++ " :: " ++ showQName t
  block (SequenceDefn n rs _ _) = stackBlocks $
    (stringToBlock $ "SequenceDefn " ++ n) : map (indent "  " . block) rs
  block (UnionDefn n ns _ _) = stackBlocks $
    (stringToBlock $ "UnionDefn " ++ showQName n)
    : map (indent "  " . uncurry horizontalPair) ns
  block (ListDefn n t _ _) = stringToBlock $
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
