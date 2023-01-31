
-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  SimpleTypeScheme(..),
  ComplexTypeScheme(..),
  DataScheme(..),
  nonSkip
) where

import Text.XML.Light.Types (QName)
import Text.XML.Light.Output
import QDHXB.Internal.Utils.BPP

data SimpleTypeScheme =
  Synonym -- ^ One type which is just the same as another
    QName -- ^ Base type
  | SimpleRestriction -- ^ One type with certain values excluded.
      QName -- ^ Base type
  | Union -- ^ A type defined as a collection (union) of values from
          -- simple data types.
      [DataScheme] -- ^ Constituent types
  deriving Show

instance Blockable SimpleTypeScheme where
  block (Synonym t) = labelBlock "== " $ block t
  block (SimpleRestriction r) = labelBlock "SimpleRestriction " $ block r
  block (Union ds) = labelBlock "Union " $ block ds
instance VerticalBlockList SimpleTypeScheme

-- | Representation of certain definitions of one XSD type based on
-- another type.
data ComplexTypeScheme =
  Sequence -- ^ The <sequence> complex type.
    [DataScheme] -- ^ List of associated definitions
  | ComplexRestriction -- ^ One type with certain values excluded.
    QName -- ^ Base type
  | Extension -- ^ One type extended with additional elements.
    QName -- ^ Base type
    [DataScheme] -- ^ Additional elements
  | Choice (Maybe QName) -- ^ name
           [DataScheme]  -- ^ contents
  deriving Show

instance Blockable ComplexTypeScheme where
  block (Sequence ds) =
    (stringToBlock "Sequence") `stack2` indent "  " (block ds)
  block (ComplexRestriction r) = Block ["SimpleRestriction " ++ show r]
  block (Extension base ds) =
    (stringToBlock $ "Extension " ++ show base)
    `stack2` indent "  " (block ds)
  block (Choice base ds) =
    (stringToBlock $ "Choice " ++ show base)
    `stack2` indent "  " (block ds)
instance VerticalBlockList ComplexTypeScheme

-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  Skip
  | ElementScheme [DataScheme] -- ^ contents
                  (Maybe QName) -- ^ ifName
                  (Maybe QName) -- ^ ifType
                  (Maybe QName) -- ^ ifRef
                  (Maybe Int) -- ^ ifMin
                  (Maybe Int) -- ^ ifMax
  | AttributeScheme (Maybe QName) -- ^ ifName
                    (Maybe QName) -- ^ ifType
                    (Maybe QName) -- ^ ifRef
                    String -- ^ use mode: prohibited, optional
                           -- (default), required
  | ComplexTypeScheme ComplexTypeScheme -- ^ typeDetail
                      [DataScheme] -- ^ addlAttrs
                      (Maybe QName) -- ^ ifName
  | SimpleTypeScheme QName -- ^ name
                     SimpleTypeScheme -- ^ Details
  | Group (Maybe QName) -- ^ name
          (Maybe ComplexTypeScheme) -- ^ contents
  deriving Show

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

  block (AttributeScheme ifName ifType ifRef usage) =
    stringToBlock $
      "AttributeScheme name="
      ++ (case ifName of
             Nothing -> "undef"
             Just s  -> "\"" ++ show s ++ "\"")
      ++ " type="
      ++ (case ifType of
             Nothing -> "undef"
             Just s  -> "\"" ++ show s ++ "\"")
      ++ " ref="
      ++ (case ifRef of
             Nothing -> "undef"
             Just s  -> "\"" ++ show s ++ "\"")
      ++ " usage=\"" ++ usage ++ "\""

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

-- | Predicate returning `False` on `Skip` values
nonSkip :: DataScheme -> Bool
nonSkip Skip = False
nonSkip _ = True

