{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  NameOrRefOpt(..), nameOrRefOpt,
  SimpleTypeScheme(..),
  ComplexTypeScheme(..),
  AttributeScheme(..),
  DataScheme(..),
  QNameOr(..),
  nonSkip, labelOf
) where

import Text.XML.Light.Types (QName, Line, qName)
import Text.XML.Light.Output
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.XMLLight (withPrefix)

-- | A sort of variation of `Maybe` with two `Just` forms, for schema
-- which allow either a @name@ or a @ref@ attribute, but not both, and
-- possibly neither.
data NameOrRefOpt =
  WithName QName  -- ^ Case for having a @name@ but no @ref@
  | WithRef QName -- ^ Case for having a @ref@  but no @name@
  | WithNeither   -- ^ Case for neither
  deriving Show

instance Blockable NameOrRefOpt where
  block (WithName n) = labelBlock "name=" $ block n
  block (WithRef r)  = labelBlock "ref="  $ block r
  block (WithNeither) = stringToBlock "(neither)"

nameOrRefOpt :: Maybe QName -> Maybe QName -> NameOrRefOpt
nameOrRefOpt (Just n) (Just r) =
  error "Cannot give both name and ref attributes"
nameOrRefOpt (Just n) Nothing = WithName n
nameOrRefOpt Nothing (Just r) = WithRef r
nameOrRefOpt Nothing Nothing = WithNeither

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
      (Maybe DataScheme) -- ^ Constituent type
  deriving Show

instance Blockable SimpleTypeScheme where
  block (Synonym t) = labelBlock "== " $ block t
  block (SimpleRestriction r) = labelBlock "SimpleRestriction " $ block r
  block (Union ds) = labelBlock "Union " $ block ds
  block (List t Nothing) = labelBlock "List " $ block t
  block (List Nothing t) = labelBlock "List " $ block t
  block (List r t) = stackBlocks [
    stringToBlock "List with both reference and subelement",
    labelBlock "  reference " $ block r,
    labelBlock "  element " $ block t
    ]
instance VerticalBlockList SimpleTypeScheme
instance VerticalBlockList (QName, DataScheme)
instance VerticalBlockablePair QName DataScheme

-- |This type is used in places where either a named reference or a
-- nested specification might be used.
data QNameOr =
  NameRef -- ^ Used when a reference by name to a specification
          -- defined elsewhere is provided.
  QName -- ^ The given name.
  | Nested -- ^ Used when a nested specification is provided.
    DataScheme -- ^ The nested spec.
  | Neither -- ^ Used when neither a name nor a nested specification
            -- is given.
  deriving Show

instance Blockable QNameOr where
  block (NameRef qn) = block qn
  block (Nested ds) = block ds
  block (Neither) = stringToBlock "(neither)"


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
    (stringToBlock "Composing")
    `stack2` labelBlock "  - subelements " (block ds)
    `stack2` labelBlock "  - attributes " (block as)
  block (ComplexRestriction r) = Block ["ComplexRestriction " ++ show r]
  block (Extension base ds) =
    (stringToBlock $ "Extension " ++ show base)
    `stack2` indent "  " (block ds)
  block (Choice base ds) =
    (stringToBlock $ "Choice " ++ show base)
    `stack2` indent "  " (block ds)
instance VerticalBlockList ComplexTypeScheme


-- | Details of attributes
data AttributeScheme =
  SingleAttribute NameOrRefOpt -- ^ Name or reference, or neither
                  QNameOr -- ^ ifType
                  String -- ^ use mode: prohibited, optional
                         -- (default), required
                  (Maybe String) -- ^ idDoc
  | AttributeGroup NameOrRefOpt -- ^ Name or reference, or neither
                   [AttributeScheme]  -- ^ included attributes and
                                      -- attribute groups
                   (Maybe String) -- ^ idDoc
  deriving Show

instance Blockable AttributeScheme where
  block (SingleAttribute nameOrRef ifType mode _d) =
    stringToBlock "single attr "
    `follow` block nameOrRef
    `stack2` stringToBlock "  type="
    `follow` block ifType
    `follow` stringToBlock " mode="
    `follow` stringToBlock mode
  block (AttributeGroup nameRef attrs _d) =
    stringToBlock "group "
    `follow` block nameRef
    `follow` stringToBlock " "
    `stack2` (indent "  " $ block attrs)
instance VerticalBlockList AttributeScheme


-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  Skip
  | ElementScheme (Maybe DataScheme) -- ^ contents
                  (Maybe QName) -- ^ ifName
                  (Maybe QName) -- ^ ifType
                  (Maybe QName) -- ^ ifRef
                  (Maybe String) -- ^ ifId
                  (Maybe Int) -- ^ ifMin
                  (Maybe Int) -- ^ ifMax
                  (Maybe Line) -- ^ ifLine
                  (Maybe String) -- ^ ifDocumentation
  | AttributeScheme AttributeScheme -- ^ Single vs. group
                    (Maybe Line) -- ^ ifLine
                    (Maybe String) -- ^ ifDocumentation
  | ComplexTypeScheme ComplexTypeScheme -- ^ typeDetail
                      [DataScheme] -- ^ addlAttrs
                      (Maybe QName) -- ^ ifName
                      (Maybe Line) -- ^ ifLine
                      (Maybe String) -- ^ ifDocumentation
  | SimpleTypeScheme (Maybe QName) -- ^ ifName
                     SimpleTypeScheme -- ^ Details
                     (Maybe Line) -- ^ ifLine
                     (Maybe String) -- ^ ifDocumentation
  | GroupScheme NameOrRefOpt -- ^ name or reference, or possibly neither
                (Maybe ComplexTypeScheme) -- ^ contents
                (Maybe Line) -- ^ ifLine
                (Maybe String) -- ^ ifDocumentation
  | UnprocessedXML (Maybe QName) -- ^ name
                   (Maybe Line) -- ^ ifLine
                   (Maybe String) -- ^ ifDocumentation
  deriving Show

--  block Skip =
--  block (ElementScheme ctnts ifName ifType ifRef ifId ifMin ifMax ifLine ifDoc) =
--  block (AttributeScheme ifName ifType ifRef usage ifLine ifDoc) =
--  block (ComplexTypeScheme form attrs ifName ifLine ifDoc) =
--  block (SimpleTypeScheme name detail ifDoc) =
--  block (GroupScheme base ref typeScheme ifLine ifDoc) =
--  block (UnprocessedXML ifName ifLine ifDoc) =


instance Blockable DataScheme where
  block Skip = Block ["Skip"]
  block (ElementScheme ctnts ifName ifType ifRef ifId ifMin ifMax _ifLine ifDoc) =
    stackBlocks [
      stringToBlock ("ElementScheme name="
                           ++ (case ifName of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")
                           ++ " type="
                           ++ (case ifType of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")),
      indent "  " $ stringToBlock ("ref="
                           ++ (case ifRef of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ showQName s ++ "\"")
                           ++ " id="
                           ++ (case ifId of
                                 Nothing -> "undef"
                                 Just s  -> "\"" ++ s ++ "\"")
                           ++ " min="
                           ++ (case ifMin of
                                 Nothing -> "undef"
                                 Just s  -> show s)
                           ++ " max="
                           ++ (case ifMax of
                                 Nothing -> "undef"
                                 Just s  -> show s)),
        indent "  " $ block ctnts,
        case ifDoc of
          Nothing -> indent "  " $ stringToBlock "no doc"
          Just doc -> stringToBlock $ "doc=\"" ++ doc ++ "\""
      ]

  block (AttributeScheme s _ _) = labelBlock "AttributeScheme " $ block s

  block (ComplexTypeScheme form attrs ifName _ln _d) =
    (labelBlock "ComplexTypeScheme name=" $ block ifName)
    `stack2` (indent "  " $ block form)
    `stack2` (indent "  " $ block attrs)

  block (SimpleTypeScheme name detail _ln _d) =
    stackBlocks [
      labelBlock "SimpleTypeScheme " $ block name,
      labelBlock "  scheme " $ block detail
      ]

  block (GroupScheme (WithName nam) ts _ln _d) =
    stringToBlock ("Group " ++ showQName nam ++ " with contents")
    `stack2` (labelBlock "  " $ block ts)
  block (GroupScheme (WithRef ref) _ _ln _d) = stringToBlock $
    "Group reference " ++ qName ref
  block (GroupScheme WithNeither ts _ln _d) = stringToBlock $
    "Group unnamed no-ref " ++ show ts
  block (UnprocessedXML ifName _ln _doc) = stringToBlock $
    "Unprocessed XML" ++ case ifName of
                           Just n -> " \"" ++ showQName n ++ "\""
                           Nothing -> ""
instance VerticalBlockList DataScheme


-- | Try to find a name for this `DataScheme`.
labelOf :: DataScheme -> Maybe QName
labelOf Skip = Nothing
labelOf (ElementScheme _ _ (Just name) _ _ _ _ _ _) = Just name
labelOf (ElementScheme _ _ _ (Just typ) _ _ _ _ _) = Just typ
labelOf (ElementScheme (Just sub) _ _ _ _ _ _ _ _) = labelOf sub
labelOf (ElementScheme _ _ _ _ _ _ _ _ _) = Nothing
labelOf (AttributeScheme (SingleAttribute (WithName n) _ _ _) _ _) = Just n
labelOf (AttributeScheme (SingleAttribute _ (NameRef qn) _ _) _ _) = Just qn
labelOf (AttributeScheme (SingleAttribute _ (Nested d) _ _) _ _) = labelOf d
labelOf (AttributeScheme (SingleAttribute (WithRef r) _ _ _) _ _) = Just r
labelOf (AttributeScheme (AttributeGroup (WithName n) _ _) _ _) = Just n
labelOf (AttributeScheme (AttributeGroup (WithRef r) _ _) _ _) = Just r
labelOf (AttributeScheme _ _ _) = Nothing
labelOf (ComplexTypeScheme _ _ j@(Just _) _ _) = j
labelOf (ComplexTypeScheme (Composing _ds _as) _attrs _ _ _) = Nothing
labelOf (ComplexTypeScheme (ComplexRestriction r) _attrs _ _ _) = Just r
labelOf (ComplexTypeScheme (Extension base _ds) _attrs _ _ _) = Just base
labelOf (ComplexTypeScheme (Choice base _ds) _attrs _ _ _) = base
labelOf (SimpleTypeScheme j@(Just _) _ _ _) = j
labelOf (SimpleTypeScheme _ (Synonym t) _ _) = Just t
labelOf (SimpleTypeScheme _ (SimpleRestriction r) _ _) = Just r
labelOf (SimpleTypeScheme _ (Union _ds) _ _) = Nothing
labelOf (SimpleTypeScheme _ (List t@(Just _) _) _ _) =
  fmap (withPrefix "List") t
labelOf (SimpleTypeScheme _ (List _ (Just t)) _ _) =
  fmap (withPrefix "List") (labelOf t)
labelOf (SimpleTypeScheme _ (List _ _) _ _) = Nothing
labelOf (UnprocessedXML n _ _) = n
labelOf (GroupScheme (WithName n) _n _l _d) = Just n
labelOf (GroupScheme (WithRef n) _n _l _d) = Just n
labelOf (GroupScheme WithNeither _n _l _d) = Nothing

-- | Predicate returning `False` on `Skip` values
nonSkip :: DataScheme -> Bool
nonSkip Skip = False
nonSkip _ = True
