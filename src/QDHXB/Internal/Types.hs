{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

-- | Internal representation of flattened XSD elements.
module QDHXB.Internal.Types (
  -- * The representation types
  Reference(..), referenceQName, referenceBase,
  AttributeDefn(..),
  Definition(..),
  AttributeUsage(Forbidden, Optional, Required),
  stringToAttributeUsage, pprintDefns'
  ) where

import Data.List (intercalate)
import Text.XML.Light.Types
import Text.XML.Light.Output (showQName)
import Language.Haskell.TH (Exp, Type)
import QDHXB.Internal.Utils.BPP

-- | A reference to an XSD element.
data Reference =
  ElementRef     -- ^ A named element type, possibly with numeric
                 -- instance bounds.
      QName       -- ^ Name of the element.
      (Maybe Int)  -- ^ Lower bound of occurences
      (Maybe Int)   -- ^ Upper bound of occurences.
      (Maybe Line)   -- ^ Source code line.
  | AttributeRef    -- ^ Reference to an attribute.
      QName          -- ^ Name of the attribute.
      AttributeUsage  -- ^ Whether the attribute is required.
  | TypeRef       -- ^ Reference to a type.
      QName        -- ^ Name of the type.
      (Maybe Int)   -- ^ Lower bound of occurences.
      (Maybe Int)    -- ^ Upper bound of occurences.
      (Maybe Line)    -- ^ Source code line.
      (Maybe String)   -- ^ Documentation of the referenced type.
{-
  | ComplexTypeRef String
  -- ^ The name of a complex type.
-}
  deriving Show

instance Blockable Reference where
  block (ElementRef name ifLower ifUpper _) = stringToBlock $
    "ElementRef " ++ showQName name
    ++ maybe " no lower bound" ((" lower bound=" ++) . show) ifLower
    ++ maybe " no upper bound" ((" upper bound=" ++) . show) ifUpper
  block (AttributeRef name usage) = stringToBlock $
    "AttributeRef " ++ showQName name ++ " usage=" ++ show usage
  block (TypeRef name ifLower ifUpper _ _) = stringToBlock $
    "TypeRef " ++ showQName name
    ++ maybe " no lower bound" ((" lower bound=" ++) . show) ifLower
    ++ maybe " no upper bound" ((" upper bound=" ++) . show) ifUpper

-- | Enumeration encoding the valid values of the XSD attribute
-- definition's "usage" attribute.
data AttributeUsage = Forbidden | Optional | Required
  deriving (Eq, Show)

verticalBlockList [t|Reference|]

-- | Return the `QName` of the entity described in a `Reference`.
referenceBase :: Reference -> QName
referenceBase (ElementRef base _ _ _) = base
referenceBase (AttributeRef base _) = base
referenceBase (TypeRef base _ _ _ _) = base

-- | Definition of an attribute or group type.
data AttributeDefn =
  SingleAttributeDefn -- ^ Defining a single attribute
      QName   -- ^ ifType
      AttributeUsage -- ^ use mode: prohibited, optional (default), required
  | AttributeGroupDefn -- ^ Defining a group of attributes
      [QName] -- ^ names of included attributes and attribute groups
       -- TODO Should be (QName, AttributeUsage) --- and update uses
       -- in module Generate (xsdDeclToHaskell, AttributeDefn case)
  deriving Show

instance Blockable AttributeDefn where
  block (SingleAttributeDefn t m) = stringToBlock $
    "Single " ++ showQName t ++ " (" ++ show m ++ ")"
  block (AttributeGroupDefn ds) =
    labelBlock "AttributeGroup " $ stackBlocks $
      map (stringToBlock . showQName) ds

-- | The actual definition of an XSD element.
data Definition =
  ElementDefn
  -- ^ Defining an element to be of a particular type.
      QName -- ^ The element tag name
      QName -- ^ The underlying type
      (Maybe Line) -- ^ ifLine
      (Maybe String) -- ^ Documentation string, if available
  | AttributeDefn
    -- ^ Defining the attributes and groups.
        QName AttributeDefn
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | SimpleSynonymDefn
    -- ^ Defining one simple type to have the same structure as
    -- another.
        QName QName
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | ComplexSynonymDefn
    -- ^ Defining one complex type to have the same structure as
    -- another.
        QName -- ^ The new type
        QName -- ^ The old synonym
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
{-
  | ElementTypeDecl
    -- ^ Associating an element tag with an underlying type.  The type
    -- may be simple or complex, and may not have been discovered when
    -- this declaration is made, so we defer the determination of
    -- simple-vs.-complex until .
        QName -- ^ The element name
        QName -- ^ The name of the associated type
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
-}
  | SequenceDefn
    -- ^ Define a complex type as a sequence of subelements.
        QName [Reference]
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
  | ChoiceDefn
    -- ^ Define a complex type as a tagged union of other types.
        QName
        -- ^ Name of the type
        [(QName, Reference)]
        -- ^ (Constructor name, type reference) of each element of the
        -- union.
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | ExtensionDefn
    -- ^ Define a type the extension of one type with additional
    -- contexts/attributes
        QName
        -- ^ Name of the extension
        Reference
        -- ^ Base type
        [Reference]
        -- ^ Additional content
        (Maybe Line) -- ^ ifLine
        (Maybe String) -- ^ Documentation string, if available
  | GroupDefn
    -- ^ Define a type for extending another type with additional
    -- contexts/attributes
        QName
        -- ^ Name of the group
        Reference
        -- ^ Included type
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
  | BuiltinDefn -- ^ Built-in names added to the environment during
                -- configuration
      QName -- ^ Type being defined
      String -- ^ String name of the corresponding Haskell type
      Type -- ^ Corresponding Haskell type
      (Exp -> Exp) -- ^ Expression builder for decoding the `String`
                   -- representation of the value in the XSD file.

instance Blockable Definition where
  block (ElementDefn n t _ dm) =
    (stringToBlock $
     "ElementDefn " ++ showQName n ++ " :: " ++ showQName t)
    `stack2` (stringToBlock $
                maybe "  no doc" (\d -> "  doc=\"" ++ d ++ "\"") dm)
  block (AttributeDefn n sp _ _) =
    labelBlock ("Attribute " ++ showQName n ++ " ") $ block sp
  block (SimpleSynonymDefn n t _ _) = stringToBlock $
    "SimpleSynonymDefn " ++ showQName n ++ " :: " ++ showQName t
  block (ComplexSynonymDefn n t _ _) = stringToBlock $
    "ComplexSynonymDefn " ++ showQName n ++ " :: " ++ showQName t
  block (SequenceDefn qn rs _ _) = stackBlocks $
    (labelBlock "SequenceDefn " $ block qn) : map (indent "  " . block) rs
  block (UnionDefn n ns _ _) = stackBlocks $
    (stringToBlock $ "UnionDefn " ++ showQName n)
    : map (indent "  " . uncurry horizontalPair) ns
  block (ChoiceDefn n ns _ _) = stackBlocks $
    (stringToBlock $ "ChoiceDefn " ++ showQName n)
    : map (indent "  " . uncurry horizontalPair) ns
  block (ExtensionDefn n base exts _ _) = stackBlocks $
    (labelBlock "ExtensionDefn " $ block n)
    : (labelBlock "  base " $ block base)
    : map (indent "  " . block) exts
  block (ListDefn n t _ _) = stringToBlock $
    "ListDefn " ++ showQName n ++ " :: [" ++ showQName t ++ "]"
  block (GroupDefn n t _ _) =
    labelBlock ("GroupDefn " ++ showQName n ++ " == ") $ block t
  block (BuiltinDefn qn n _ _) =
    stringToBlock $ "BuiltinDefn " ++ qName qn ++ " for " ++ n
verticalBlockList [t|Definition|]
verticalBlockablePair [t|QName|] [t|Reference|]
verticalBlockList [t|(QName, Reference)|]
verticalBlockablePair [t|QName|] [t|[Definition]|]
verticalBlockablePair [t|[Definition]|] [t|Reference|]
verticalBlockablePair [t|[Definition]|] [t|[Reference]|]
verticalBlockList [t|(QName, [Definition])|]

-- | Convert a `String` to an `AttributeUsage` value.
stringToAttributeUsage :: String -> AttributeUsage
stringToAttributeUsage "forbidden" = Forbidden
stringToAttributeUsage "required"  = Required
stringToAttributeUsage _ = Optional

-- | Display a list of `Definition` in more human-readable text.
pprintDefns' :: String -> [Definition] -> String
pprintDefns' ind ds = intercalate ("\n" ++ ind) $ map bpp ds

-- | Extract the QName behind a `Reference`.
referenceQName :: Reference -> QName
referenceQName (TypeRef q _ _ _ _) = q
referenceQName (ElementRef q _ _ _) = q
referenceQName (AttributeRef q _) = q
