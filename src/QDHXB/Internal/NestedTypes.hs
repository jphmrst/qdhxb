{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  SimpleTypeScheme(..),
  ComplexTypeScheme(..),
  AttributeScheme(..),
  DataScheme(..),
  nonSkip, labelOf
) where

import Text.XML.Light.Types (QName)
import Text.XML.Light.Output
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.XMLLight (withPrefix)

-- | Further details about @simpleType@ and @simpleContents@ XSD
-- elements.
data SimpleTypeScheme =
  Synonym -- ^ One type which is just the same as another
    QName -- ^ Base type
  | SimpleRestriction -- ^ One type with certain values excluded.
      QName -- ^ Base type
  | Union -- ^ A type defined as a collection (union) of values from
          -- simple data types.
      [DataScheme] -- ^ Constituent types
  | List -- ^ Space-delimited list of simple types
      (Maybe QName) -- ^ Type of list elements
  deriving Show

instance Blockable SimpleTypeScheme where
  block (Synonym t) = labelBlock "== " $ block t
  block (SimpleRestriction r) = labelBlock "SimpleRestriction " $ block r
  block (Union ds) = labelBlock "Union " $ block ds
  block (List t) = labelBlock "List " $ block t
instance VerticalBlockList SimpleTypeScheme
instance VerticalBlockList (QName, DataScheme)
instance VerticalBlockablePair QName DataScheme

-- | Further details about @complexType@ and @complexContents@ XSD
-- elements.
data ComplexTypeScheme =
  Composing -- ^ Assembling a type from multiple constituents ---
            -- <sequence>, <attributegroup>, etc
    [DataScheme] -- ^ List of associated sub-elements
    [AttributeScheme] -- ^ List of associated attributes
  | ComplexRestriction -- ^ One type with certain values excluded.
    QName -- ^ Base type
  | Extension -- ^ One type extended with additional elements.
    QName -- ^ Base type
    [DataScheme] -- ^ Additional elements
  | Choice (Maybe QName) -- ^ name
           [DataScheme]  -- ^ contents
  deriving Show

instance Blockable ComplexTypeScheme where
  block (Composing ds as) =
    (stringToBlock "Sequence")
    `stack2` labelBlock "  - subelements " (block ds)
    `stack2` labelBlock "  - attributes " (block as)
  block (ComplexRestriction r) = Block ["SimpleRestriction " ++ show r]
  block (Extension base ds) =
    (stringToBlock $ "Extension " ++ show base)
    `stack2` indent "  " (block ds)
  block (Choice base ds) =
    (stringToBlock $ "Choice " ++ show base)
    `stack2` indent "  " (block ds)
instance VerticalBlockList ComplexTypeScheme

-- | Details of attributes
data AttributeScheme =
  SingleAttribute (Maybe QName) -- ^ ifName
                  (Maybe QName) -- ^ ifRef
                  (Maybe QName) -- ^ ifType
                  String -- ^ use mode: prohibited, optional
                         -- (default), required
  | AttributeGroup (Maybe QName) -- ^ ifName
                   (Maybe QName) -- ^ ifRef
                   [AttributeScheme]  -- ^ included attributes and
                                      -- attribute groups
  deriving Show

instance Blockable AttributeScheme where
  block (SingleAttribute ifName ifRef ifType mode) =
    stringToBlock "single name="
    `follow` block ifName
    `follow` stringToBlock " ref="
    `follow` block ifRef
    `follow` stringToBlock " type="
    `follow` block ifType
    `follow` stringToBlock " mode="
    `follow` stringToBlock mode
  block (AttributeGroup ifName ifRef attrs) =
    stringToBlock "group name="
    `follow` block ifName
    `follow` stringToBlock " ref="
    `follow` block ifRef
    `follow` stringToBlock " "
    `stack2` (indent "  " $ block attrs)
instance VerticalBlockList AttributeScheme

-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  Skip
  | ElementScheme [DataScheme] -- ^ contents
                  (Maybe QName) -- ^ ifName
                  (Maybe QName) -- ^ ifType
                  (Maybe QName) -- ^ ifRef
                  (Maybe Int) -- ^ ifMin
                  (Maybe Int) -- ^ ifMax
  | AttributeScheme AttributeScheme -- ^ Single vs. group
  | ComplexTypeScheme ComplexTypeScheme -- ^ typeDetail
                      [DataScheme] -- ^ addlAttrs
                      (Maybe QName) -- ^ ifName
  | SimpleTypeScheme (Maybe QName) -- ^ ifName
                     SimpleTypeScheme -- ^ Details
  | Group (Maybe QName) -- ^ name
          (Maybe ComplexTypeScheme) -- ^ contents
  deriving Show

--  block Skip =
--  block (ElementScheme ctnts ifName ifType ifRef ifMin ifMax) =
--  block (AttributeScheme ifName ifType ifRef usage) =
--  block (ComplexTypeScheme form attrs ifName) =
--  block (SimpleTypeScheme name detail) =
--  block (Group base typeScheme) =

instance Blockable DataScheme where
  block Skip = Block ["Skip"]
  block (ElementScheme ctnts ifName ifType ifRef ifMin ifMax) =
    stack2 (stringToBlock ("ElementScheme name="
                           ++ (case ifName of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")
                           ++ " type="
                           ++ (case ifType of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")
                           ++ " ref="
                           ++ (case ifRef of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")
                           ++ " min="
                           ++ (case ifMin of
                                 Nothing -> "undef"
                                 Just s  -> show s)
                           ++ " max="
                           ++ (case ifMax of
                                 Nothing -> "undef"
                                 Just s  -> show s)))
           (indent "  " $ block ctnts)

  block (AttributeScheme s) = labelBlock "AttributeScheme " $ block s

  block (ComplexTypeScheme form attrs ifName) =
    (stringToBlock $ "ComplexTypeScheme name="
                     ++ (case ifName of
                           Nothing -> "undef"
                           Just s  -> "\"" ++ show s ++ "\""))
    `stack2` (indent "  " $ block form)
    `stack2` (indent "  " $ block attrs)

  block (SimpleTypeScheme name detail) =
    labelBlock "SimpleTypeScheme " $
      stackBlocks [
        block name,
        labelBlock "scheme " $ block detail
        ]

  block (Group base (Just ts)) = Block [
    "Group " ++ show base ++ " with contents",
    "  " ++ show ts
    ]
  block (Group base Nothing) = stringToBlock $
    "Group " ++ show base ++ " with no contents"
instance VerticalBlockList DataScheme

-- | Try to find a name for this `DataScheme`.
labelOf :: DataScheme -> Maybe QName
labelOf Skip = Nothing
labelOf (ElementScheme _ (Just name) _ _ _ _) = Just name
labelOf (ElementScheme _ _ (Just typ) _ _ _) = Just typ
labelOf (ElementScheme cs _ _ _ _ _) = makeFirst cs
  where makeFirst [] = Nothing
        makeFirst (d:ds) = case labelOf d of
                             Nothing -> makeFirst ds
                             Just r -> Just r
labelOf (AttributeScheme (SingleAttribute j@(Just _) _ _ _)) = j
labelOf (AttributeScheme (SingleAttribute _ _ j@(Just _) _)) = j
labelOf (AttributeScheme (SingleAttribute _ j@(Just _) _ _)) = j
labelOf (AttributeScheme (AttributeGroup j@(Just _) _ _)) = j
labelOf (AttributeScheme (AttributeGroup _ j@(Just _) _)) = j
labelOf (AttributeScheme _) = Nothing
labelOf (ComplexTypeScheme _ _ j@(Just _)) = j
labelOf (ComplexTypeScheme (Composing _ds _as) _attrs _) = Nothing
labelOf (ComplexTypeScheme (ComplexRestriction r) _attrs _) = Just r
labelOf (ComplexTypeScheme (Extension base _ds) _attrs _) = Just base
labelOf (ComplexTypeScheme (Choice base _ds) _attrs _) = base
labelOf (SimpleTypeScheme j@(Just _) _) = j
labelOf (SimpleTypeScheme _ (Synonym t)) = Just t
labelOf (SimpleTypeScheme _ (SimpleRestriction r)) = Just r
labelOf (SimpleTypeScheme _ (Union _ds)) = Nothing
labelOf (SimpleTypeScheme _ (List t)) = fmap (withPrefix "List") t
labelOf (Group base _n) = base

-- | Predicate returning `False` on `Skip` values
nonSkip :: DataScheme -> Bool
nonSkip Skip = False
nonSkip _ = True
