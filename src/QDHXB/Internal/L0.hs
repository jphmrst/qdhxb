{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-} -- For apiFunctions

-- | Manual intermediate representation for nested XSD structures as
-- read in from a file to be translated into Haskell types and
-- functions.  This representation is used internally only, to
-- bootstrap a more complete IR from the XSD specification.
module QDHXB.Internal.L0 (
  -- * Containers for different alternative values
  -- ** A name, a reference, or neither
  NameOrRefOpt(..), nameOrRefOpt,
  -- ** A name, or a nested (possibly anonymous) specification
  QNameOr(..),
  -- * Type and attribute specification details
  -- ** Simple types
  SimpleTypeScheme(..),
  -- ** Complex types
  ComplexTypeScheme(..),
  -- ** Attributes
  AttributeScheme(..),
  -- * Main AST
  DataScheme(..), nonSkip, labelOf,
  -- * API functions with this intermediate representtion
  qdhxb, qdhxb'
) where

import Language.Haskell.TH (newName, nameBase, Q, Dec)
import Data.List (intercalate)
import Control.Monad.IO.Class
import Text.Read (readMaybe)
import Text.XML.Light.Types
import Text.XML.Light.Output
import QDHXB.Options
import QDHXB.Internal.API
import QDHXB.Internal.AST
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ
import QDHXB.Utils.Misc (pickOrCombine, ifAtLine, applyFst)
import QDHXB.Utils.BPP
import QDHXB.Utils.TH (firstToUpper)
import QDHXB.Utils.XMLLight
import QDHXB.Utils.ZeroOneMany

import QDHXB.Internal.Debugln

maybeqname_to_stringlist :: Maybe QName -> [String]
maybeqname_to_stringlist (Just qn) = [qName qn]
maybeqname_to_stringlist _ = []

maybe_to_list :: Maybe a -> [a]
maybe_to_list (Just qn) = [qn]
maybe_to_list _ = []



-- | A sort of variation of `Maybe` with two `Just` forms, for schema
-- which allow either a @name@ or a @ref@ attribute, but not both, and
-- possibly neither.
data NameOrRefOpt =
  WithName QName  -- ^ Case for having a @name@ but no @ref@
  | WithRef QName -- ^ Case for having a @ref@  but no @name@
  | WithNeither   -- ^ Case for neither
  deriving Show

nameOrRefOptToMaybeString :: NameOrRefOpt -> Maybe String
nameOrRefOptToMaybeString (WithName qn) = Just $ qName qn
nameOrRefOptToMaybeString (WithRef qn)  = Just $ qName qn
nameOrRefOptToMaybeString (WithNeither) = Nothing

-- | Assemble a `NameOrRefOpt` value from two @(`Maybe` `QName`)@
-- values.  The first argument corresponds to a possible @name@
-- attribute, and the second argument corresponds to a possible @ref@
-- attribute.
nameOrRefOpt :: Maybe QName -> Maybe QName -> NameOrRefOpt
nameOrRefOpt (Just _) (Just _) =
  error "Cannot give both name and ref attributes"
nameOrRefOpt (Just n) Nothing = WithName n
nameOrRefOpt Nothing (Just r) = WithRef r
nameOrRefOpt Nothing Nothing = WithNeither

-- | Apply a substitution to a `NameOrRefOpt`.
subst_NameOrRefOpt :: Substitutions -> NameOrRefOpt -> MaybeUpdated NameOrRefOpt
subst_NameOrRefOpt _ n@WithNeither = Same n
subst_NameOrRefOpt [] n = Same n
subst_NameOrRefOpt ((q1,q2):_) (WithRef q)  | qName q == q1 =
  Updated $ WithRef $ inSameNamspace q2 q
subst_NameOrRefOpt ((q1,q2):_) (WithName q) | qName q == q1 =
  Updated $ WithName $ inSameNamspace q2 q
subst_NameOrRefOpt (_:substs) n = subst_NameOrRefOpt substs n

nameonly_to_stringlist :: NameOrRefOpt -> [String]
nameonly_to_stringlist (WithName qn) = [qName qn]
nameonly_to_stringlist _ = []

instance Blockable NameOrRefOpt where
  block (WithName n) = labelBlock "name=" $ block n
  block (WithRef r)  = labelBlock "ref="  $ block r
  block (WithNeither) = stringToBlock "(neither)"


-- | Further details about @simpleType@ and @simpleContents@ XSD
-- elements.
data SimpleTypeScheme =
  Synonym -- ^ One type which is just the same as another
      QName -- ^ Base type
  | SimpleRestriction -- ^ One type with certain values excluded.
      QName -- ^ Base type
  | Union -- ^ A type defined as a collection (union) of values from
          -- simple data types.
      [DataScheme] -- ^ Constituent types given as nested schemes
      [QName] -- ^ Constituent types given by name in the
              -- @memberTypes@ attribute
  | List -- ^ Space-delimited list of simple types
      (Maybe QName) -- ^ Type of list elements
      (Maybe DataScheme) -- ^ Constituent type
  deriving Show

{-
Synonym baseType
SimpleRestriction baseType
Union dss typeQNames
List ifElemTypeQName ifNestedTypeDS
-}

-- | Apply `Substitutions` to the given `SimpleTypeScheme`.
subst_sts :: Substitutions -> SimpleTypeScheme -> MaybeUpdated SimpleTypeScheme
subst_sts substs sts@(Synonym baseType) =
  let baseType' = substQName substs baseType
  in assembleIfUpdated [Upd baseType'] sts $ Synonym $ resultOnly baseType'
subst_sts substs sts@(SimpleRestriction baseType) =
  let baseType' = substQName substs baseType
  in assembleIfUpdated [Upd baseType'] sts $
       SimpleRestriction $ resultOnly baseType'
subst_sts substs sts@(Union dss typeQNames) =
  let dss' = hoistUpdate $ map (applySubstitutionsTo substs) dss
      typeQNames' = hoistUpdate $ map (substQName substs) typeQNames
  in assembleIfUpdated [Upd dss', Upd typeQNames'] sts $
       Union (resultOnly dss') (resultOnly typeQNames')
subst_sts substs sts@(List ifElemTypeQName ifNestedTypeDS) =
  let ifElemTypeQName' = hoistUpdate $ fmap (substQName substs) ifElemTypeQName
      ifNestedTypeDS' = hoistUpdate $
        fmap (applySubstitutionsTo substs) ifNestedTypeDS
  in assembleIfUpdated [Upd ifElemTypeQName', Upd ifNestedTypeDS'] sts $
       List (resultOnly ifElemTypeQName') (resultOnly ifNestedTypeDS')

-- | Local version of `ensureUniqueInternalNames` for
-- `SimpleTypeScheme`s.
unique_internals_sts :: SimpleTypeScheme -> XSDQ (MaybeUpdated SimpleTypeScheme)
unique_internals_sts sts@(Synonym _) = do
  dbgLn unique 3 $
    "unique_internals_sts for " ++ bpp sts ++ " --- no change"
  return $ Same sts
unique_internals_sts sts@(SimpleRestriction _) = do
  dbgLn unique 3 $
    "unique_internals_sts for " ++ bpp sts ++ " --- no change"
  return $ Same sts
unique_internals_sts sts@(Union dss typeQNames) = do
  dbgLn unique 3 $ "unique_internals_sts for union"
  dss' <- indenting $ fmap hoistUpdate $ mapM ensureUniqueInternalNames dss
  dbgResult unique 3 "unique_internals_sts on union: " $
    assembleIfUpdated [Upd dss'] sts $ Union (resultOnly dss') typeQNames
unique_internals_sts sts@(List ifElemTypeQName (Just ds)) = do
  dbgLn unique 3 $
    "unique_internals_sts for " ++ bpp sts ++ " --- no change"
  ds' <- ensureUniqueNames1 ds
  return $ assembleIfUpdated [Upd ds'] sts $
    List ifElemTypeQName (Just $ resultOnly ds')
unique_internals_sts sts@(List _ Nothing) = do
  dbgLn unique 3 $
    "unique_internals_sts for List with no nested DS --- no change to STS"
  return $ Same sts

instance Blockable SimpleTypeScheme where
  block (Synonym t) = labelBlock "== " $ block t
  block (SimpleRestriction r) = labelBlock "SimpleRestriction " $ block r
  block (Union ds ns) = labelBlock "Union " (block ds `stack2` block ns)
  block (List t Nothing) = labelBlock "List " $ block t
  block (List Nothing t) = labelBlock "List " $ block t
  block (List r t) = stackBlocks [
    stringToBlock "List with both reference and subelement",
    labelBlock "  reference " $ block r,
    labelBlock "  element " $ block t
    ]


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

-- | Apply a substitution to a `QNameOr`.
substQNameOr :: Substitutions -> QNameOr -> MaybeUpdated QNameOr
substQNameOr _ n@Neither = Same n
substQNameOr ss qo@(Nested ds) =
  let ds' = applySubstitutionsTo ss ds
  in assembleIfUpdated [Upd ds'] qo $ Nested $ resultOnly ds'
substQNameOr ss qo@(NameRef qn) =
  let qn' = substQName ss qn
  in assembleIfUpdated [Upd qn'] qo $ NameRef $ resultOnly qn'

-- | Local version of `ensureUniqueInternalNames` for `QNameOr`s.
unique_internals_QNameOr :: QNameOr -> XSDQ (MaybeUpdated QNameOr)
unique_internals_QNameOr qno@(NameRef _) = return $ Same qno
unique_internals_QNameOr qno@(Nested ds) = do
  ds' <- indenting $ ensureUniqueNames1 ds
  return $ assembleIfUpdated [Upd ds'] qno $ Nested (resultOnly ds')
unique_internals_QNameOr qno@Neither = return $ Same qno

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
  | Group NameOrRefOpt -- ^ name or reference (or neither)
          (Maybe DataScheme)  -- ^ contents
          (Maybe Int) -- ^ ifMin
          (Maybe Int) -- ^ ifMax
  deriving Show

{- Composing dss attrSchs
ComplexRestriction qn
Extension qn dss
Choice ifQn dss
Group nameOrRef ifDS ifMin ifMax
-}

instance Blockable ComplexTypeScheme where
  block (Composing ds as) =
    (stringToBlock "Composing")
    `stack2` labelBlock "  - subelements " (block ds)
    `stack2` labelBlock "  - attributes " (block as)
  block (ComplexRestriction r) = Block ["ComplexRestriction " ++ show r]
  block (Extension base ds) =
    (stringToBlock $ "Extension " ++ showQName base)
    `stack2` indent "  " (block ds)
  block (Choice base ds) =
    (stringToBlock $ "Choice " ++ maybe "[unnamed]" showQName base)
    `stack2` indent "  " (block ds)
  block (Group nr contents ifMin ifMax) =
    (labelBlock "Group " $ block nr)
    `stack2` (labelBlock "  min=" $ block ifMin)
    `stack2` (labelBlock "  max=" $ block ifMax)
    `stack2` (indent "  " $ block contents)


-- | Apply `Substitutions` to a `ComplexTypeScheme`.
subst_cts ::
  Substitutions -> ComplexTypeScheme -> MaybeUpdated ComplexTypeScheme
subst_cts ss cts@(Composing dss attrSchs) =
  let attrSchs' = hoistUpdate $ map (subst_attr_scheme ss) attrSchs
      dss' = hoistUpdate $ map (applySubstitutionsTo ss) dss
  in assembleIfUpdated [Upd attrSchs', Upd dss'] cts $
       Composing (resultOnly dss') (resultOnly attrSchs')
subst_cts ss cts@(ComplexRestriction qn) =
  let qn' = substQName ss qn
  in assembleIfUpdated [Upd qn'] cts $ ComplexRestriction (resultOnly qn')
subst_cts ss cts@(Extension qn dss) =
  let qn' = substQName ss qn
      dss' = hoistUpdate $ map (applySubstitutionsTo ss) dss
  in assembleIfUpdated [Upd qn', Upd dss'] cts $
       Extension (resultOnly qn') (resultOnly dss')
subst_cts ss cts@(Choice ifQn dss) =
  let ifQn' = hoistUpdate $ fmap (substQName ss) ifQn
      dss' = hoistUpdate $ map (applySubstitutionsTo ss) dss
  in assembleIfUpdated [Upd ifQn', Upd dss'] cts $
       Choice (resultOnly ifQn') (resultOnly dss')
subst_cts ss cts@(Group nameOrRef ifDS ifMin ifMax) =
  let nameOrRef' = subst_NameOrRefOpt ss nameOrRef
      ifDS' = hoistUpdate $ fmap (applySubstitutionsTo ss) ifDS
  in assembleIfUpdated [Upd nameOrRef', Upd ifDS'] cts $
       Group (resultOnly nameOrRef') (resultOnly ifDS') ifMin ifMax

-- | Local version of `ensureUniqueInternalNames` for
-- `ComplexTypeScheme`.
unique_internals_cts ::
  ComplexTypeScheme -> XSDQ (MaybeUpdated ComplexTypeScheme)
unique_internals_cts cts@(Composing dss attrSchs) = do
  dbgLn unique 3 "unique_internals_cts for Composing"

  -- TODO --- Pull attribute class names here and generate
  -- substitutions for duplicates?  Or:

  indenting $ do
    dss' <- ensureUniqueNames' dss
    attrSchs' <- fmap hoistUpdate $ mapM unique_internals_attr_scheme attrSchs
    return $ assembleIfUpdated [Upd dss', Upd attrSchs'] cts $
      Composing (resultOnly dss') (resultOnly attrSchs')
unique_internals_cts cts@(ComplexRestriction _) = return $ Same cts
unique_internals_cts cts@(Extension qn dss) = do
  dss' <- indenting $ ensureUniqueNames' dss
  return $ assembleIfUpdated [Upd dss'] cts $ Extension qn $ resultOnly dss'
unique_internals_cts cts@(Choice ifQn dss) = do
  dss' <- indenting $ ensureUniqueNames' dss
  return $ assembleIfUpdated [Upd dss'] cts $ Choice ifQn $ resultOnly dss'
unique_internals_cts cts@(Group nameOrRef ifDS ifMin ifMax) = do
  ifDS' <- maybe (return $ Same ifDS)
                 (indenting . fmap (fmap Just) . ensureUniqueNames1)
                 ifDS
  return $ assembleIfUpdated [Upd ifDS'] cts $
    Group nameOrRef (resultOnly ifDS') ifMin ifMax


-- | Details of attributes
data AttributeScheme =
  SingleAttribute NameOrRefOpt -- ^ Name or reference, or neither
                  (Maybe String) -- ^ Base name for Haskell type
                                 -- translation.
                  QNameOr -- ^ ifType
                  String -- ^ use mode: prohibited, optional
                         -- (default), required
                  (Maybe String) -- ^ Documentation, typically from an
                                 -- @annotation@ element, if
                                 -- provided.
  | AttributeGroup NameOrRefOpt -- ^ Name or reference, or neither
                   [AttributeScheme]  -- ^ included attributes and
                                      -- attribute groups
                   (Maybe String) -- ^ Documentation, typically from
                                  -- an @annotation@ element, if
                                  -- provided.
  deriving Show

{-
SingleAttribute nameOrRef ifHName ifTypeOr mode ifDoc
AttributeGroup nameOrRef attrSchs ifDoc
-}

instance Blockable AttributeScheme where
  block (SingleAttribute nameOrRef ifHName ifType mode _d) =
    stringToBlock "single attr "
    `follow` block nameOrRef
    `follow` stringToBlock ", Haskell class "
    `follow` block ifHName
    `stack2` stringToBlock "  type="
    `follow` block ifType
    `follow` stringToBlock " mode="
    `follow` stringToBlock mode
  block (AttributeGroup nameRef attrs _d) =
    stringToBlock "group "
    `follow` block nameRef
    `follow` stringToBlock " "
    `stack2` (indent "  " $ block attrs)

instance Blockable [AttributeScheme] where block = verticalBlockListFn


attributeSchemeQname :: AttributeScheme -> Maybe QName
attributeSchemeQname (SingleAttribute (WithName qn) _ _ _ _) = Just qn
attributeSchemeQname (SingleAttribute (WithRef qn) _ _ _ _) = Just qn
attributeSchemeQname (SingleAttribute (WithNeither) _ _ _ _) = Nothing
attributeSchemeQname (AttributeGroup (WithName qn) _ _) = Just qn
attributeSchemeQname (AttributeGroup (WithRef qn) _ _) = Just qn
attributeSchemeQname (AttributeGroup (WithNeither) _ _) = Nothing

subst_attr_scheme ::
  Substitutions -> AttributeScheme -> MaybeUpdated AttributeScheme
subst_attr_scheme ss aspec@(SingleAttribute nameOrRef ifhn ifType mode ifDoc) =
  let nameOrRef' = subst_NameOrRefOpt ss nameOrRef
      ifType' = substQNameOr ss ifType
      ifhn' = hoistUpdate $ fmap (substString ss) ifhn
  in assembleIfUpdated [Upd nameOrRef', Upd ifType', Upd ifhn'] aspec $
       SingleAttribute nameOrRef (resultOnly ifhn') (resultOnly ifType')
                       mode ifDoc
subst_attr_scheme ss aspec@(AttributeGroup nameOrRef schemes ifDoc) =
  let nameOrRef' = subst_NameOrRefOpt ss nameOrRef
      schemes' = hoistUpdate $ map (subst_attr_scheme ss) schemes
  in assembleIfUpdated [Upd nameOrRef', Upd schemes'] aspec $
       AttributeGroup (resultOnly nameOrRef') (resultOnly schemes') ifDoc

unique_internals_attr_scheme ::
  AttributeScheme -> XSDQ (MaybeUpdated AttributeScheme)
  -- TODO substitute in ifhn?
unique_internals_attr_scheme attrsc@(SingleAttribute nameOrRef ifhn ifType
                                                     mode ifDoc) = do
  dbgLn unique 3 $ "unique_internals for attr " ++ show nameOrRef

  -- Check whether the implementation class name should be changed
  ifhn' <- case ifhn of
    Just hn -> do
      dbgLn unique 3 $ "Checking name " ++ hn
      indenting $ do
        substs <- indenting $ makeNeededSubstitutions [hn]
        dbgLn unique 3 $ "substs " ++ show substs
        dbgResult unique 3 "Name is " $
          hoistUpdate $ fmap (substString substs) ifhn
    Nothing -> return $ Same ifhn
  dbgBLabel unique 3 "ifhn' " ifhn'

  -- Recur on the type
  ifType' <- indenting $ unique_internals_QNameOr ifType
  dbgBLabel unique 3 "ifType' " ifType'

  -- Put the result together
  dbgResult unique 3 "[uias] returns" $
    assembleIfUpdated [Upd ifType', Upd ifhn'] attrsc $
      SingleAttribute nameOrRef (resultOnly ifhn') (resultOnly ifType')
                      mode ifDoc
unique_internals_attr_scheme ag@(AttributeGroup nameOrRef attrDefs ifDoc) = do
  dbgLn unique 3 $ "unique_internals for attr group " ++ show nameOrRef
  attrDefs' <- indenting $ fmap hoistUpdate $
    mapM unique_internals_attr_scheme attrDefs
  return $ assembleIfUpdated [Upd attrDefs'] ag $
    AttributeGroup nameOrRef (resultOnly attrDefs') ifDoc

bound_names_attrsch :: AttributeScheme -> [String]
bound_names_attrsch (SingleAttribute _ ifHName _ _ _) = maybe_to_list ifHName
bound_names_attrsch (AttributeGroup nameOrRef attrSchs _) =
  nameonly_to_stringlist nameOrRef ++ concat (map bound_names_attrsch attrSchs)


-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  Skip -- ^ Placeholder for an empty tree
  | ElementScheme -- ^ An @<element>@ specification
                  (Maybe DataScheme) -- ^ Nested specification of the
                                     -- element type, or @Nothing@ if
                                     -- omitted.
                  (Maybe QName) -- ^ Qualified name of the element
                                -- (from the @name@ attribute), if one
                                -- is given.
                  (Maybe QName) -- ^ Qualified name of the type of the
                                -- element (from the @type@
                                -- attribute), if one is given.
                  (Maybe QName) -- ^ Qualified name of the defining
                                -- reference of the element (from the
                                -- @ref@ attribute), if one is given.
                  (Maybe String) -- ^ ifId
                  (Maybe QName) -- ^ Name by which this element is
                                -- associated internally.  This name
                                -- will be maintained as unique, and
                                -- will be renamed if needed.
                  (Maybe Int) -- ^ ifMin
                  (Maybe Int) -- ^ ifMax
                  Bool -- ^ isAbstract
                  (Maybe Line) -- ^ Associated line number in the
                               -- defining file, if returned from the
                               -- underlying XML parser.
                  (Maybe String) -- ^ Documentation, typically from an
                                 -- @annotation@ element, if
                                 -- provided.
  | AttributeScheme -- ^ Attributes and attribute groups
                    AttributeScheme -- ^ Single vs. group
                    String -- ^ `String` name of implementing class
                    (Maybe Line) -- ^ Associated line number in the
                                 -- defining file, if returned from
                                 -- the underlying XML parser.
                    (Maybe String) -- ^ Documentation, typically from
                                   -- an @annotation@ element,
                                   -- if provided.
  | CTS -- ^ Other complex type definitions
        ComplexTypeScheme -- ^ typeDetail
        [AttributeScheme] -- ^ addlAttrs
        {- TODO --- Go back and populate this field -}
        (Maybe QName) -- ^ ifName
        {- TODO? String -- ^ `String` name of implementing class --- but maybe
           not for XSD-internal type names? -}
        (Maybe Line) -- ^ Associated line number in the defining file,
                     -- if returned from the underlying XML parser.
        (Maybe String) -- ^ Documentation, typically from an
                       -- @annotation@ element, if provided.
  | STS -- ^ One of the various simple type definitions
        (Maybe QName) -- ^ ifName
        {- TODO? String -- ^ `String` name of implementing class --- but maybe
           not for XSD-internal type names? -}
        SimpleTypeScheme -- ^ Details
        (Maybe Line) -- ^ Associated line number in the defining file,
                     -- if returned from the underlying XML parser.
        (Maybe String) -- ^ Documentation, typically from an
                       -- @annotation@ element, if provided.
  | GroupScheme -- ^ A @<group>@ element.
                NameOrRefOpt -- ^ name or reference, or possibly neither
                (Maybe ComplexTypeScheme) -- ^ contents
                {- TODO String -- ^ Implementation type name -}
                (Maybe Line) -- ^ Associated line number in the
                             -- defining file, if returned from the
                             -- underlying XML parser.
                (Maybe String) -- ^ Documentation, typically from an
                               -- @annotation@ element, if provided.
  -- `DataScheme` continues


  | ChoiceScheme -- ^ A @<choice>@ element.
                 NameOrRefOpt -- ^ name or reference, or possibly neither
                 (Maybe ComplexTypeScheme) -- ^ contents
                 {- TODO String -- ^ Implementation type name -}
                 (Maybe Line) -- ^ Associated line number in the
                              -- defining file, if returned from the
                              -- underlying XML parser.
                 (Maybe String) -- ^ Documentation, typically from an
                                -- @annotation@ element, if
                                -- provided.
  | UnprocessedXML -- ^ When raw XML is dropped in
                   (Maybe QName) -- ^ name
                   (Maybe Line) -- ^ Associated line number in the
                                -- defining file, if returned from the
                                -- underlying XML parser.
                   (Maybe String) -- ^ Documentation, typically from
                                  -- an @annotation@ element,
                                  -- if provided.
  deriving Show

{-
Skip ->
(ElementScheme ctnts ifName ifType ifRef ifId tagName ifMin ifMax isAbst _ _) ->
(AttributeScheme (SingleAttribute nameOrRef ifHName ifType mode ifDoc) impl _ _) ->
(AttributeScheme (AttributeGroup nameOrRef attrDefs _) impl _ _) ->
(CTS form attrs ifName _ _) ->
(STS name (Synonym base) _ _) ->
(STS name (SimpleRestriction base) _ _) ->
(STS name (Union nesteds members) _ _) ->
(STS name (List ifElemType ifNested) _ _) ->
(GroupScheme nameOrRef typeScheme _ _) ->
(ChoiceScheme nameOrRef typeScheme _ _) ->
(UnprocessedXML ifName _ _) ->
-}

quoteShowQName :: QName -> String
quoteShowQName s = "\"" ++ showQName s ++ "\""
quoteString :: String -> String
quoteString s = "\"" ++ s ++ "\""


-- | Try to find a name for this `DataScheme`.
labelOf :: DataScheme -> Maybe QName
labelOf Skip = Nothing
labelOf (ElementScheme _ _ (Just name) _ _ _ _ _ _ _ _) = Just name
labelOf (ElementScheme _ _ _ (Just typ) _ _ _ _ _ _ _) = Just typ
labelOf (ElementScheme (Just sub) _ _ _ _ _ _ _ _ _ _) = labelOf sub
labelOf (ElementScheme _ _ _ _ _ _ _ _ _ _ _) = Nothing
labelOf (AttributeScheme (SingleAttribute (WithName n) _ _ _ _) _ _ _) = Just n
labelOf (AttributeScheme (SingleAttribute _ _ (NameRef qn) _ _) _ _ _) = Just qn
labelOf (AttributeScheme (SingleAttribute _ _ (Nested d) _ _) _ _ _) = labelOf d
labelOf (AttributeScheme (SingleAttribute (WithRef r) _ _ _ _) _ _ _) = Just r
labelOf (AttributeScheme (AttributeGroup (WithName n) _ _) _ _ _) = Just n
labelOf (AttributeScheme (AttributeGroup (WithRef r) _ _) _ _ _) = Just r
labelOf (AttributeScheme _ _ _ _) = Nothing
labelOf (CTS _ _ j@(Just _) _ _) = j
labelOf (CTS (Composing _ds _as) _ _ _ _) = Nothing
labelOf (CTS (ComplexRestriction r) _ _ _ _) = Just r
labelOf (CTS (Extension base _ds) _ _ _ _) = Just base
labelOf (CTS (Choice base _ds) _ _ _ _) = base
labelOf (CTS (Group (WithName n) _ _ _) _ _ _ _) = Just n
labelOf (CTS (Group _ _ _ _) _ _ _ _) = Nothing
labelOf (STS j@(Just _) _ _ _) = j
labelOf (STS _ (Synonym t) _ _) = Just t
labelOf (STS _ (SimpleRestriction r) _ _) = Just r
labelOf (STS _ (Union _ds _ns) _ _) = Nothing
labelOf (STS _ (List t@(Just _) _) _ _) =
  fmap (withPrefix "List") t
labelOf (STS _ (List _ (Just t)) _ _) =
  fmap (withPrefix "List") (labelOf t)
labelOf (STS _ (List _ _) _ _) = Nothing
labelOf (UnprocessedXML n _ _) = n
labelOf (GroupScheme (WithName n) _n _l _d) = Just n
labelOf (GroupScheme (WithRef n) _n _l _d) = Just n
labelOf (GroupScheme WithNeither _n _l _d) = Nothing
labelOf (ChoiceScheme (WithName n) _n _l _d) = Just n
labelOf (ChoiceScheme (WithRef n) _n _l _d) = Just n
labelOf (ChoiceScheme WithNeither _n _l _d) = Nothing

-- | Predicate returning `False` on `Skip` values
nonSkip :: DataScheme -> Bool
nonSkip Skip = False
nonSkip _ = True


instance Blockable DataScheme where
  block Skip = Block ["Skip"]
  block (ElementScheme ctnts ifName ifType ifRef ifId tagName
                       ifMin ifMax isAbstract _ifLine ifDoc) =
    stackBlocks [
      stringToBlock ("ElementScheme name="
                       ++ maybe "undef" quoteShowQName ifName
                       ++ " tag " ++ maybe "undef" quoteShowQName tagName
                       ++ if isAbstract then " abstract" else ""
                       ++ " type="
                       ++ maybe "undef" quoteShowQName ifType),
      indent "  " $ stringToBlock ("ref="
                           ++ maybe "undef" quoteShowQName ifRef
                           ++ " id="
                           ++ maybe "undef" quoteString ifId
                           ++ " min="
                           ++ maybe "undef" show ifMin
                           ++ " max="
                           ++ maybe "undef" show ifMax),
        indent "  " $ block ctnts,
        case ifDoc of
          Nothing -> indent "  " $ stringToBlock "no doc"
          Just doc -> stringToBlock $ "doc=\"" ++ doc ++ "\""
      ]

  block (AttributeScheme s i _ _) =
    (labelBlock "AttributeScheme " $ block s)
    `follow` (labelBlock " as " $ block i)

  block (CTS form attrs ifName _ln _d) =
    (labelBlock "CTS name=" $ block ifName)
    `stack2` (indent "  " $ block form)
    `stack2` (indent "  " $ block attrs)

  block (STS name detail _ln _d) =
    stackBlocks [
      labelBlock "STS " $ block name,
      labelBlock "  scheme " $ block detail
      ]

  block (GroupScheme (WithName nam) ts _ln _d) =
    stringToBlock ("Group " ++ showQName nam ++ " with contents")
    `stack2` (labelBlock "  " $ block ts)
  block (GroupScheme (WithRef ref) _ _ln _d) = stringToBlock $
    "Group reference " ++ qName ref
  block (GroupScheme WithNeither ts _ln _d) = stringToBlock $
    "Group unnamed no-ref " ++ bpp ts

  block (ChoiceScheme (WithName nam) ts _ln _d) =
    stringToBlock ("Choice " ++ showQName nam ++ " with contents")
    `stack2` (labelBlock "  " $ block ts)
  block (ChoiceScheme (WithRef ref) _ _ln _d) = stringToBlock $
    "Choice reference " ++ qName ref
  block (ChoiceScheme WithNeither ts _ln _d) = stringToBlock $
    "Choice unnamed no-ref " ++ bpp ts

  block (UnprocessedXML ifName _ln _doc) = stringToBlock $
    "Unprocessed XML" ++ maybe "" quoteShowQName ifName

instance Blockable [DataScheme] where block = verticalBlockListFn


newtype PrimaryBundle = PB (
  Content,       -- ^ Corresponding XML fragment
  String,        -- ^ Tag
  Maybe String,  -- ^ URI
  Maybe String,  -- ^ Prefix
  QName,         -- ^ `QName` name
  [Attr],        -- ^ Attributes as a list of `Attr`s
  [Content],     -- ^ Subcontents
  Maybe Line
  )

instance Blockable PrimaryBundle where
  block (PB (_,_,_,_,q,a,c,_)) = stackBlocks [
    labelBlock "name " $ block q,
    labelBlock "attrs " $ block a,
    labelBlock "subcontents " $ block $ filter isElem c]


instance AST DataScheme where

  -- | Traverse a single `DataScheme` to collect the top-level bound
  -- names.
  getBoundNameStringsFrom ast = case ast of
    Skip -> []
    (ElementScheme _ ifNam _ _ _ _ _ _ _ _ _) -> maybeqname_to_stringlist ifNam
    (AttributeScheme scheme impl _ _) -> impl : bound_names_attrsch scheme
    (CTS _ _ ifName _ _) -> maybeqname_to_stringlist ifName
    (STS ifName _ _ _) -> maybeqname_to_stringlist ifName
    (GroupScheme nameOrRef _ _ _) -> nameonly_to_stringlist nameOrRef
    (ChoiceScheme nameOrRef _ _ _) -> nameonly_to_stringlist nameOrRef
    (UnprocessedXML ifName _ _) -> maybeqname_to_stringlist ifName

  -- | Apply the given substitutions to the given AST.
  applySubstitutionsTo substs ast = case ast of
    Skip -> Same ast
    (ElementScheme ifCtnt ifName ifType ifRef ifId
                   ifTag ifMin ifMax isAbstract ifLn ifDoc) ->
      let ifCtnt' = hoistUpdate $ fmap (applySubstitutionsTo substs) ifCtnt
          sq = substQName substs
          ifType' = hoistUpdate $ fmap sq ifType
          ifName' = hoistUpdate $ fmap sq ifName
          ifRef' = hoistUpdate $ fmap sq ifRef
      in assembleIfUpdated [Upd ifCtnt', Upd ifName', Upd ifType', Upd ifRef']
                           ast $
           ElementScheme (resultOnly ifCtnt') (resultOnly ifName')
                         (resultOnly ifType') (resultOnly ifRef') ifId ifTag
                         ifMin ifMax isAbstract ifLn ifDoc
    (AttributeScheme aspec impl ifLn ifDoc) ->
      let aspec' = subst_attr_scheme substs aspec
          impl' = substString substs impl
      in assembleIfUpdated [Upd aspec', Upd impl'] ast $
           AttributeScheme (resultOnly aspec') (resultOnly impl') ifLn ifDoc
    (CTS form attrs ifName ifLn ifDoc) ->
      let form' = subst_cts substs form
          attrs' = hoistUpdate $ map (subst_attr_scheme substs) attrs
          ifName' = hoistUpdate $ fmap (substQName substs) ifName
      in assembleIfUpdated [Upd form', Upd attrs', Upd ifName'] ast $
           CTS (resultOnly form') (resultOnly attrs')
                             (resultOnly ifName') ifLn ifDoc
    (STS name sts ifLn ifDoc) ->
      let name' = hoistUpdate $ fmap (substQName substs) name
          sts' = subst_sts substs sts
      in assembleIfUpdated [Upd name', Upd sts'] ast $
           STS (resultOnly name') (resultOnly sts') ifLn ifDoc
    (GroupScheme nameOrRef typeScheme ifLn ifDoc) ->
      let nameOrRef' = subst_NameOrRefOpt substs nameOrRef
          typeScheme' = hoistUpdate $ fmap (subst_cts substs) typeScheme
      in assembleIfUpdated [Upd nameOrRef', Upd typeScheme'] ast $
           GroupScheme (resultOnly nameOrRef') (resultOnly typeScheme')
                       ifLn ifDoc
    (ChoiceScheme nameOrRef typeScheme ifLn ifDoc) ->
      let nameOrRef' = subst_NameOrRefOpt substs nameOrRef
          typeScheme' = hoistUpdate $ fmap (subst_cts substs) typeScheme
      in assembleIfUpdated [Upd nameOrRef', Upd typeScheme'] ast $
           ChoiceScheme (resultOnly nameOrRef') (resultOnly typeScheme')
                        ifLn ifDoc
    (UnprocessedXML ifName ifLn ifDoc) ->
      let ifName' = hoistUpdate $ fmap (substQName substs) ifName
      in assembleIfUpdated [Upd ifName'] ast $
           UnprocessedXML (resultOnly ifName') ifLn ifDoc


  -- | Rename any nonunique hidden names within the scope of the given
  -- @ast@, but not defined at top-level.  Used by
  -- `QDHXB.Internal.AST.ensureUniqueNames`.  This is a case over the
  -- structure of the @ast@ type, and applying `ensureUniqueNames` or
  -- `ensureUniqueNames1` to recursively-held lists of ASTs.
  ensureUniqueInternalNames dss@Skip = return $ Same dss
  ensureUniqueInternalNames dss@((ElementScheme ifDS ifName ifType ifRef ifId
                                                ifTag ifMin ifMax isAbstract
                                                ifLn ifDoc)) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for " ++ debugSlug dss
    ifDS' <- maybe (return $ Same ifDS)
                   (fmap (fmap Just) . ensureUniqueNames1) ifDS
    return $ assembleIfUpdated [Upd ifDS'] dss $
               ElementScheme (resultOnly ifDS') ifName ifType ifRef ifId
                             ifTag ifMin ifMax isAbstract ifLn ifDoc
  ensureUniqueInternalNames ats@(AttributeScheme scheme impl ifLn ifDoc) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for " ++ debugSlug ats
    scheme' <- indenting $ unique_internals_attr_scheme scheme
    return $ assembleIfUpdated [Upd scheme'] ats $
      AttributeScheme (resultOnly scheme') impl ifLn ifDoc

  ensureUniqueInternalNames dss@(CTS form attrs ifName ifLn ifDoc) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for CTS " ++ debugSlug dss
    indenting $ do
      dbgLn unique 3 $ "Calling unique_internals_cts on the form"
      form' <- indenting $ unique_internals_cts form
      dbgBLabel unique 3 "form' " form'
      dbgLn unique 3 $ "Calling unique_internals_attr_scheme on each attr"
      attrs' <- indenting $
        fmap hoistUpdate $ mapM unique_internals_attr_scheme attrs
      dbgBLabel unique 3 "attrs' " attrs'
      dbgResult unique 3 "[eUIN] result" $
        assembleIfUpdated [Upd form', Upd attrs'] dss $
          CTS (resultOnly form') (resultOnly attrs') ifName ifLn ifDoc

  ensureUniqueInternalNames dss@(STS name sts ifLn ifDoc) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for " ++ debugSlug dss
    sts' <- indenting $ unique_internals_sts sts
    return $ assembleIfUpdated [Upd sts'] dss $
      STS name (resultOnly sts') ifLn ifDoc
  ensureUniqueInternalNames dss@(GroupScheme nameOrRef ifCts ifLn ifDoc) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for " ++ debugSlug dss
    cts' <- case ifCts of
              Just cts -> do
                cts'' <- indenting $ unique_internals_cts cts
                return $ fmap Just cts''
              Nothing -> return $ Same ifCts
    return $ assembleIfUpdated [Upd cts'] dss $
      GroupScheme nameOrRef (resultOnly cts') ifLn ifDoc
  ensureUniqueInternalNames dss@(ChoiceScheme nameOrRef ifCts ifLn ifDoc) = do
    dbgLn unique 3 $ "ensureUniqueInternalNames for " ++ debugSlug dss
    cts' <- case ifCts of
              Just cts -> do
                cts'' <- indenting $ unique_internals_cts cts
                return $ fmap Just cts''
              Nothing -> return $ Same ifCts
    return $ assembleIfUpdated [Upd cts'] dss $
      ChoiceScheme nameOrRef (resultOnly cts') ifLn ifDoc
  ensureUniqueInternalNames dss@(UnprocessedXML _ _ _) = return $ Same dss


  -- | Converting `DataScheme`s to `Definition`s.
  flatten = fmap concat . mapM flattenSchemaItem . filter nonSkip
    where

      flattenSchemaItem :: DataScheme -> XSDQ [Definition]
      {-# INLINE flattenSchemaItem #-}
      flattenSchemaItem s = do
        dbgBLabel flattening 1 "[fSI] from " s
        results <- indenting $ flattenSchemaItem' s
        dbgResult flattening 1 "Result [fSI]:" results

      flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
      flattenSchemaItem' Skip = return []

      flattenSchemaItem' (ElementScheme contents ifName ifType ifRef _ifId
                                        ifTag ifMin ifMax abst l ifDoc) = do
        dbgLn flattening 1 $ "[fSI'] Relaying to flattenElementSchemeItem"
        flattenElementSchemeItem contents ifName ifType ifRef ifTag ifMin ifMax
                                 abst l ifDoc

      flattenSchemaItem' (AttributeScheme
                          (SingleAttribute (WithName nam) (Just hnam)
                                           (NameRef typ) m d')
                          _impl l d) = do
        dbgLn flattening 1 $ "[fSI'] Flattening single attribute" ++ ifAtLine l
        let attrDefn =
              AttributeDefn nam nam
                (SingleAttributeDefn typ (stringToAttributeUsage m) hnam)
                l (pickOrCombine d d')
        fileNewDefinition attrDefn
        dbgResult flattening 1 "Flattened [fSI'] to" [attrDefn]

      flattenSchemaItem' (AttributeScheme (SingleAttribute (WithRef _) _ _ _ _)
                                          _impl ln _d) = do
        return $ error $ "[fSI'] Reference in attribute" ++ ifAtLine ln

      flattenSchemaItem' (AttributeScheme
                          (SingleAttribute (WithName nam) _ (Nested ds)
                                           use innerDoc)
                          _impl l outerDoc) = do
        dbgLn flattening 1 $ "[fSI'] attribute with nested type" ++ ifAtLine l
        (defs, ref) <- flattenSchemaRef ds
        let qn = referenceQName ref
        let attrDefn =
              AttributeDefn nam nam
                (SingleAttributeDefn qn (stringToAttributeUsage use)
                                     (qName nam))
                l (pickOrCombine innerDoc outerDoc)
        fileNewDefinition attrDefn
        dbgResult flattening 1 "Flattened [fSI'] to" $ defs ++ [attrDefn]

      flattenSchemaItem' (AttributeScheme (AttributeGroup nr cs _) _i l d) = do
        dbgLn flattening 1 $
          "[fSI'] Relaying to flattenAttributeGroupItem for" ++ ifAtLine l ++ " "
        flattenAttributeGroupItem nr cs l d

      flattenSchemaItem' (CTS cts ats ifNam l d) = do
        dbgLn flattening 1 $
          "[fSI'] Relaying to flattenComplexTypeScheme" ++ ifAtLine l
        flattenComplexTypeScheme cts ats ifNam l d

      flattenSchemaItem' s@(STS (Just n) (Synonym base) ln d) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening simple synonym" ++ ifAtLine ln ++ " ") s
        indenting $ do
          let tyDefn = SimpleSynonymDefn n base ln d
          fileNewDefinition tyDefn
          dbgResult flattening 1 "Flattened [fSI'] to" [tyDefn]

      -- TODO Insert cases of SimpleRestriction that we /can/ handle in the
      -- types here

      flattenSchemaItem' sts@(STS (Just nam)
                                               (SimpleRestriction base)
                                               ln d) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening simple restriction" ++ ifAtLine ln ++ " ") sts
        indenting $ do
          let tyDefn = SimpleSynonymDefn nam base ln d
          addTypeDefn nam tyDefn
          dbgResult flattening 1 "Flattened [fSI'] to" $ [ tyDefn ]

      flattenSchemaItem' sts@(STS (Just nam) (Union alts ns)
                                               ln d) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening simple union" ++ ifAtLine ln ++ " ") sts
        let nameUnnamed :: QName -> DataScheme -> DataScheme
            nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifId
                                         ifTag ifMin ifMax isAbstract l ifDoc) =
              ElementScheme ctnts (Just q) ifType ifRef ifId
                            ifTag ifMin ifMax isAbstract l ifDoc
            nameUnnamed q (AttributeScheme
                           (SingleAttribute (WithRef _) _ ifType usage d')
                           impl ln' d'') =
              AttributeScheme (SingleAttribute (WithName q) (Just $ qName q)
                                               ifType usage d')
                impl ln' (pickOrCombine d d'')
            nameUnnamed q (CTS form attrs Nothing l d') =
              CTS form attrs (Just q) l d'
            nameUnnamed q (STS Nothing detail ln' d') =
              STS (Just q) detail ln' d'
            nameUnnamed _ b = b

            pullNestedLabel :: DataScheme -> XSDQ ((QName, QName), [Definition])
            pullNestedLabel ds = do
              nameSuffix <- case labelOf ds of
                              Just q -> return q
                              Nothing -> do
                                freshName <- getNextCapName
                                freshQName <- decodePrefixedName freshName
                                return freshQName
              let name = withSuffix (firstToUpper $ qName nameSuffix) nam
              let d' = nameUnnamed name ds
              let typeName = maybe (error "Should not find anonymous decl") id $
                               labelOf d'
              defns <- flattenSchemaItem d'
              return ((name, typeName), defns)

            pullRefLabel :: QName -> (QName, QName)
            pullRefLabel qn = (withSuffix (firstToUpper $ qName qn) nam, qn)

        labelledAlts <- mapM pullNestedLabel alts
        -- dbgBLabel flattening 1 "- labelledAlts " labelledAlts
        let (laNames, defnss) = unzip labelledAlts
            defns = concat defnss
        dbgBLabel flattening 1 "- laNames " laNames
          -- dbgBLabel flattening 1 "- defnss " defnss
        let fromMemberList = map pullRefLabel ns
        dbgBLabel flattening 1 "- fromMemberList " fromMemberList
        let rawPairs = laNames ++ fromMemberList
        -- Rename constructors
        renamedPairs <- mapM (\(x,y) -> do
                                 x' <- applyConstructorRenames $ qName x
                                 return (inSameNamspace x' x,y))
                             rawPairs
        let uDef = UnionDefn nam renamedPairs ln d
        dbgBLabel flattening 1 "- uDef " uDef
        fileNewDefinition uDef
        dbgResult flattening 1 "Flattened [fSI'] to" $ defns ++ [uDef]


      flattenSchemaItem' s@(STS (Just nam)
                             (List (Just elemTyp) Nothing)
                             ln d) = do
        dbgBLabel flattening 1
            ("[fSI'] Flattening simple list with referenced element type "
             ++ ifAtLine ln) s
        indenting $ do
          let lDef = ListDefn nam elemTyp ln d
          fileNewDefinition lDef
          dbgResult flattening 1 "Flattened [fSI'] to" [lDef]

      flattenSchemaItem' s@(STS (Just nam)
                                             (List Nothing (Just inlineTyp))
                                             ln d) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening simple list with inline element" ++ ifAtLine ln ++ " ") s
        indenting $ do
          (subdefs, subref) <- flattenSchemaRef inlineTyp
          let lDef = ListDefn nam (referenceQName subref) ln d
          fileNewDefinition lDef
          dbgResult flattening 1 "Flattened [fSI'] to" $ subdefs ++ [lDef]

      flattenSchemaItem' gs@(GroupScheme (WithName name) (Just cts) ln doc) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening group scheme with name and present content"
           ++ ifAtLine ln ++ " ") gs
        -- let typeSchemeName = withSuffix "GroupContent" name
        defs <- indenting $ flattenComplexTypeScheme cts []
                              (Just name {- typeSchemeName -} ) ln doc

        -- TODO The bounds are hardcoded here, but should be detected in the
        -- Input and carried forward.
        let defn = GroupDefn name
                     (TypeRef (name {- typeSchemeName -})
                              (Just 1) (Just 1) Nothing Nothing) ln doc
        fileNewDefinition defn
        -- We do not actually generate anything from a GroupDefn, so the
        -- `defn` does not go into the list of definitions which become
        -- Haskell code.  But we do store the GroupDefn to look up as a
        -- group against its name.

        dbgResult flattening 1 "Flattened [fSI'] to" $ defs -- ++ [defn]

      flattenSchemaItem' gs@(GroupScheme (WithRef ref) Nothing ln _doc) = do
        dbgBLabel flattening 1
          ("[fSI'] Flattening group scheme with reference and no content" ++ ifAtLine ln ++ " ") gs
        boxed $ do
          dbgLn flattening 1
            "TODO flattenSchemaItem' group with ref, no contents"
          dbgLn flattening 1 $ "REF " ++ qName ref
          dbgLn flattening 1 $ "LN " ++ show ln
        error $ "TODO flatten group with ref "
          ++ qName ref
          ++ ", no contents, at "
          ++ maybe "(no XSD line num)" show ln

      flattenSchemaItem' s = do
        dbgLn flattening 1 $ "[fSI'] missed case"
        boxed $ do
          dbgLn flattening 1 $ "TODO flattenSchemaItem' missed case"
          dbgBLabel flattening 1 "ARG " s
        error $ show $ labelBlock "TODO another flatten case: " $ block s


      flattenAttributeGroupItem ::
        NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
        -> XSDQ [Definition]
      flattenAttributeGroupItem nro cs l d = case nro of
        WithName name -> flattenWithName name
        WithRef name -> flattenWithName name
        _ -> boxed $ do
          dbgLn flattening 1 $ "TODO [fAGI] missed case"
          dbgBLabel flattening 1 "NAMEREF " nro
          dbgBLabel flattening 1 "CONTENTS " cs
          dbgLn flattening 1 $ "LN " ++ show l
          error "TODO flattenAttributeGroupItem unmatched"
        where flattenWithName n = do
                dbgLn flattening 1 $ "[fAGI] Flattening attribute group item" ++ ifAtLine l
                let csNames = map grabNameAndUsage cs
                defs <- indenting $ flattenAttributes cs
                let attrDefn = AttributeDefn n n
                                   (AttributeGroupDefn csNames $ qName n) l d
                fileNewDefinition attrDefn
                let res = defs ++ [attrDefn]
                return res

      flattenComplexTypeScheme ::
        ComplexTypeScheme -> [AttributeScheme] -> Maybe QName
        -> Maybe Line -> Maybe String
        -> XSDQ [Definition]

      flattenComplexTypeScheme c@(Composing cts ats0) ats (Just nam) l d = do
        dbgBLabel flattening 1 ("[fCTS] Complex composition at" ++ ifAtLine l ++ " ") c
        (defs, refs) <- indenting $
          musterComplexSequenceComponents (filter nonSkip cts) (ats0 ++ ats) nam
            -- TODO DOC possible to add a docstring here?

        let tyDefn = SequenceDefn nam refs l d
        fileNewDefinition tyDefn
        -- whenAnyDebugging $ do
        --   recheck <- isKnownType nam
        --   dbgLn flattening 1 $ "- Have set " ++ qName nam
        --            ++ " to be known; rechecked as " ++ show recheck
        dbgResult flattening 1 "Flattened [fCTS] to" $ defs ++ [ tyDefn ]

      flattenComplexTypeScheme c@(ComplexRestriction base) _ (Just nam) l d = do
        dbgBLabel flattening 1
          ("[fCTS] Complex restriction" ++ ifAtLine l ++ " ") c
        let defn = ComplexSynonymDefn nam base l d
        fileNewDefinition defn
        dbgResult flattening 1 "Flattened [fCTS] to" $ [defn]

      flattenComplexTypeScheme e@(Extension base ds) ats (Just nam) l d = do
        dbgBLabel flattening 1
          ("[fCTS] Complex extension" ++ ifAtLine l ++ " ") e
        (defs, refs) <- indenting $ flattenSchemaRefs ds
        dbgBLabel flattening 3 "- defs from flattenSchemaRefs" defs
        dbgBLabel flattening 3 "- refs from flattenSchemaRefs" refs
        (defs', refs') <- indenting $ flattenSchemaAttributeRefs ats
        dbgBLabel flattening 3 "- defs' from flattenSchemaAttributeRefs" defs'
        dbgBLabel flattening 3 "- refs' from flattenSchemaAttributeRefs" refs'
        let defn =
              ExtensionDefn nam (TypeRef base (Just 1) (Just 1) l d)
                                (refs ++ refs') l d
        fileNewDefinition defn
        dbgResult flattening 1 "Flattened [fCTS] to" $ defs ++ defs' ++ [defn]

      flattenComplexTypeScheme c@(Choice ifName contents) _ ifOuter ln doc = do
        dbgBLabel flattening 1 ("[fCTS] Choice" ++ ifAtLine ln ++ " ") c
        dbgLn flattening 1 $ "- Line " ++ show ln
        dbgBLabel flattening 5 "- contents " contents
        let name = maybe (maybe (QName "???" Nothing Nothing) id ifOuter)
                         id ifName
        (defs, refs) <- indenting $ flattenSchemaRefs contents
        dbgBLabel flattening 3 "- defs " defs
        dbgBLabel flattening 3 "- refs " refs
        let labelledRefs = zipWith getLabelledDisjunct refs contents
            defn = ChoiceDefn name labelledRefs ln doc
        addElementType name name
        fileNewDefinition defn
        dbgResult flattening 1 "Flattened [fCTS] to" $
          defs ++ [defn]

      flattenComplexTypeScheme (Group (WithName n) (Just ctnt) ifMin ifMax)
                               ats ifName l d = do
        (flatCtnt, ctntRef) <- flattenSchemaRef ctnt
        boxed $ do
          dbgLn flattening 1 $ "TODO [fCTS] Group/WithName case"
          dbgLn flattening 1 $ "Group:"
          dbgBLabel flattening 3 ". WithName N " n
          dbgBLabel flattening 3 ". CTNT " $ ctnt
          dbgBLabel flattening 3 ". IFMIN " ifMin
          dbgBLabel flattening 3 ". IFMAX " ifMax
          dbgBLabel flattening 3 "ATS " ats
          dbgBLabel flattening 3 "IFNAME " ifName
          dbgLn flattening 3 $ "L " ++ show l
          dbgBLabel flattening 3 "FLATCTNT " $ flatCtnt
        let defn = GroupDefn n ctntRef l d
        fileNewDefinition defn
        dbgResult flattening 1 "Flattened [fCTS] to" $ flatCtnt ++ [defn]

      flattenComplexTypeScheme (Group (WithRef r) Nothing ifMin ifMax)
                               ats (Just name) l d = do
        dbgLn flattening 1 $ "[fCTS] Group/WithRef case" ++ ifAtLine l
        (defs, refs) <- indenting $ musterAttributesForComplexSequence [] [
          GroupRef r ifMin ifMax l d
          ] ats
        let tyDefn = SequenceDefn name refs l d
        fileNewDefinition tyDefn
        dbgResult flattening 1 "Flattened [fCTS] to" $ defs ++ [tyDefn]

      flattenComplexTypeScheme (Group WithNeither (Just ctnt) ifMin ifMax)
                               ats (Just name) l d = do
        dbgLn flattening 1 $ "[fCTS] Group/WithNeither case" ++ ifAtLine l
        (flatCtnt, ctntRef) <- flattenSchemaRef ctnt
        boxed $ do
          dbgLn flattening 1 "TODO flattenComplexTypeScheme Group case"
          dbgLn flattening 1 "Group:"
          dbgBLabel flattening 1 ". CTNT " $ ctnt
          dbgBLabel flattening 1 ". IFMIN " ifMin
          dbgBLabel flattening 1 ". IFMAX " ifMax
          dbgBLabel flattening 1 "ATS " ats
          dbgBLabel flattening 1 "NAME " name
          dbgLn flattening 1 $ "L " ++ show l
          dbgBLabel flattening 1 "FLATCTNT " $ flatCtnt
        let defn = GroupDefn name ctntRef l d
        fileNewDefinition defn
        dbgResult flattening 1 "Flattened [fCTS] to" $ flatCtnt ++ [defn]

      flattenComplexTypeScheme cts ats ifName ln _ = do
        boxed $ do
          dbgLn flattening 1 $ "TODO [fCTS] flattenComplexTypeScheme missed case"
          dbgBLabel flattening 1 "CTS " cts
          dbgBLabel flattening 1 "ATS " ats
          dbgBLabel flattening 1 "IFNAME " ifName
          dbgLn flattening 1 $ "LN " ++ show ln
        error "TODO flattenComplexTypeScheme missed case"

      getLabelledDisjunct :: Reference -> DataScheme -> (QName, Reference)
      getLabelledDisjunct ref ds = (maybe (referenceQName ref) id $ labelOf ds,
                                    ref)

      flattenElementSchemeItem ::
        Maybe DataScheme -> Maybe QName -> Maybe QName -> Maybe QName
        -> Maybe QName -> Maybe Int -> Maybe Int
        -> Bool -> Maybe Line -> Maybe String
        -> XSDQ [Definition]
      flattenElementSchemeItem Nothing (Just nam) (Just typ) Nothing (Just tag)
                               _ _ _ ln ifDoc = do
        dbgLn flattening 1 $ "[fESI] With name/type" ++ ifAtLine ln
        indenting $ flattenWithNameTypeOnly nam typ tag ln ifDoc
      flattenElementSchemeItem (Just Skip) (Just nam) (Just typ) _ (Just tag)
                               _ _ _ ln ifDoc = do
        dbgLn flattening 1 $ "[fESI] Enclosing skip" ++ ifAtLine ln
        indenting $ flattenWithNameTypeOnly nam typ tag ln ifDoc
      flattenElementSchemeItem (Just (STS _ ts ln d))
                               ifName@(Just nam) Nothing Nothing (Just tag)
                                _ _ _ l ifDoc = do
        dbgLn flattening 1 $ "[fESI] Enclosing simple type scheme" ++ ifAtLine l
        flatTS <- flattenSchemaItem' $ STS ifName ts ln d
        -- TODO This will change when we switch ElementDefn to the new
        -- name/tag idea.
        let elemDefn = ElementDefn nam nam (qName tag) ln ifDoc
        fileNewDefinition elemDefn
        dbgResult flattening 1 "Flattened [fESI] to " $ flatTS ++ [elemDefn]
      flattenElementSchemeItem (Just (CTS ts attrs ifCTSName l d))
                               ifElementName@(Just nam) Nothing Nothing
                               (Just tag) _ _ _ ln ifDoc = do
        dbgLn flattening 1 $
          "[fESI] Enclosing complex type scheme" ++ ifAtLine ln
        let typeName = maybe nam id ifCTSName
        let ifTypeName = maybe ifElementName Just ifCTSName
        dbgLn flattening 1
          "Flattening element scheme enclosing complex type scheme"
        flatTS <- flattenSchemaItem' $ CTS ts attrs ifTypeName l d
        -- TODO This will change when we switch ElementDefn to the new
        -- name/tag idea.
        let elemDefn = ElementDefn nam typeName (qName tag) l ifDoc
        fileNewDefinition elemDefn
        dbgResult flattening 1 "Flattened [fESI] to " $ flatTS ++ [elemDefn]
      flattenElementSchemeItem Nothing ifName@(Just _)
                               Nothing Nothing t@(Just _) ifMax ifMin
                               True ifLine ifDoc = do
        dbgLn flattening 1 $
          "[fESI] Abstract with name but no contents/type --- relay with any"
            ++ ifAtLine ifLine
        anyType <- anyTypeQName
        flattenElementSchemeItem Nothing ifName (Just anyType) Nothing
                                 t ifMax ifMin True ifLine ifDoc

      flattenElementSchemeItem content ifName ifType ifRef ifTag ifMin ifMax
                               _ l _ifDoc = do
        boxed $ do
          dbgLn flattening 1 $ "[fESI] flattenElementSchemeItem" ++ ifAtLine l
          dbgBLabel flattening 1 "CONTENT " content
          dbgBLabel flattening 1 "IFNAME " ifName
          dbgBLabel flattening 1 "IFTYPE " ifType
          dbgBLabel flattening 1 "IFREF " ifRef
          dbgBLabel flattening 1 "IFTAG " ifTag
          dbgLn flattening 1 $ "IFMIN " ++ show ifMin
          dbgLn flattening 1 $ "IFMAX " ++ show ifMax
        error "Unmatched case for flattenElementSchemeItem"

      flattenWithNameTypeOnly ::
        QName -> QName -> QName -> Maybe Line -> Maybe String
        -> XSDQ [Definition]
      flattenWithNameTypeOnly nam typ impl ln ifDoc = do
        dbgLn flattening 1 $ "flattenWithNameTypeOnly"
        -- TODO This will change when we switch ElementDefn to the new
        -- name/tag idea.
        let elemDefn = ElementDefn nam typ (qName impl) ln ifDoc
        fileNewDefinition elemDefn
        dbgResult flattening 1 "Flattened [fWNTO] to " [ elemDefn ]


      musterComplexSequenceComponents ::
        [DataScheme] ->  [AttributeScheme] -> QName
        -> XSDQ ([Definition], [Reference])
      musterComplexSequenceComponents steps ats _ = do
        dbgLn flattening 1 $ "musterComplexSequenceComponents"
        dbgBLabel flattening 1 "- STEPS " steps
        dbgBLabel flattening 1 "- ATS " ats
        (otherDefs, refs) <- indenting $ flattenSchemaRefs steps
        dbgBLabel flattening 1 "- OTHERDEFS " otherDefs
        dbgBLabel flattening 1 "- REFS " refs
        dbgLn flattening 1 $ "Relaying to musterAttributesForComplexSequence"
        musterAttributesForComplexSequence otherDefs refs ats

      musterAttributesForComplexSequence ::
        [Definition] -> [Reference] ->  [AttributeScheme]
        -> XSDQ ([Definition], [Reference])
      musterAttributesForComplexSequence defs refs ats = do
        dbgLn flattening 1 $ "musterAttributesForComplexSequence"
        dbgBLabel flattening 1 "- DEFS " defs
        dbgBLabel flattening 1 "- REFS " refs
        dbgBLabel flattening 1 "- ATS " ats
        (atsDefs, atsRefs) <- indenting $ flattenSchemaAttributeRefs ats
        dbgResult flattening 1 "Result [mAFCS]:" $
          (defs ++ atsDefs, atsRefs ++ refs)

      grabNameAndUsage :: AttributeScheme -> (QName, AttributeUsage)
      grabNameAndUsage (SingleAttribute (WithName n) _ _ useStr _) =
        (n, stringToAttributeUsage useStr)
      grabNameAndUsage (SingleAttribute (WithRef n) _ _ useStr _) =
        (n, stringToAttributeUsage useStr)
      grabNameAndUsage (SingleAttribute WithNeither _ (NameRef t) useStr _) =
        (t, stringToAttributeUsage useStr)
      grabNameAndUsage (AttributeGroup (WithName n) _ _) = (n, Optional)
      grabNameAndUsage (AttributeGroup (WithRef n) _ _) = (n, Optional)
      grabNameAndUsage a = error $ "No useable name in " ++ show a

      flattenSchemaAttributeRefs ::
        [AttributeScheme] -> XSDQ ([Definition], [Reference])
      flattenSchemaAttributeRefs ass = do
        dbgLn flattening 1
          "[flattenSchemaAttributeRefs] processing each by [fSchAR]"
        defsRefs <- indenting $ mapM flattenSchemaAttributeRef ass
        dbgLn flattening 1 $ "returned from (mapM flattenSchemaAttributeRef)"
        dbgResult flattening 1 "Result [fSAR]:" $
          applyFst concat $ unzip defsRefs

      flattenSchemaAttributeRef ::
        AttributeScheme -> XSDQ ([Definition], Reference)
      flattenSchemaAttributeRef r@(SingleAttribute nr hn t m d) = do
        dbgBLabel flattening 1 "[fSchAR -> fSngAR] " r
        flattenSingleAttributeRef nr hn t m Nothing d
      flattenSchemaAttributeRef r@(AttributeGroup nameRef cs d) = do
        dbgBLabel flattening 1 "[fSchAR -> fAGR] " r
        flattenAttributeGroupRef nameRef cs Nothing d

      flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
      flattenSchemaRefs ds = do
        dbgLn flattening 1 "[flattenSchemaRefs]"
        dbgResultM flattening 1 "Result [fSR]:" $ indenting $
          fmap (applyFst concat) $ fmap unzip $ mapM flattenSchemaRef ds


      flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
      flattenSchemaRef (ElementScheme c ifName ifType ifRef _ifId _ifTag
                                      ifLower ifUpper _isAbstract ln ifDoc) = do
        dbgLn flattening 1 $ "[fSR -> flattenElementSchemeRef]"
        flattenElementSchemeRef c ifName ifType ifRef ifLower ifUpper ln ifDoc
      flattenSchemaRef (AttributeScheme (SingleAttribute nr hn t m d') _i l d) = do
        dbgLn flattening 1 $ "[fSR -> flattenSingleAttributeRef]"
        flattenSingleAttributeRef nr hn t m l (pickOrCombine d d')
      flattenSchemaRef (AttributeScheme (AttributeGroup nameRef cs _) _i l d) = do
        dbgLn flattening 1 $ "[fSR -> flattenAttributeGroupRef]"
        flattenAttributeGroupRef nameRef cs l d
      flattenSchemaRef c@(CTS _ _ (Just n) ifLine ifDoc) = do
        dbgBLabel flattening 1 "[fSR] CTS " c
        defns <- indenting $ flattenSchemaItem c
        dbgResult flattening 1 "Flattened [fSR.CTS] to" $
          (defns, TypeRef n (Just 1) (Just 1) ifLine ifDoc)
      flattenSchemaRef s@(STS (Just n) _ ifLine ifDoc) = do
        dbgBLabel flattening 1 "[fSR] STS " s
        defns <- indenting $ flattenSchemaItem s
        dbgResult flattening 1 "Flattened [fSR.STS] to" $
          (defns, TypeRef n (Just 1) (Just 1) ifLine ifDoc)

      flattenSchemaRef gs@(GroupScheme (WithRef ref) _ifCtnts ifLn ifDoc) = do
        dbgBLabel flattening 1 "[fSR] GS-WR " gs
        dbgResult flattening 1 "Flattened [fSR.GS-WR] to" $
          ([], GroupRef ref (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef gs@(GroupScheme (WithName name) (Just sub)
                                       ifLn ifDoc) = do
        dbgBLabel flattening 1 "[fSR] GS-WN " gs
        defns <- indenting $
          flattenComplexTypeScheme sub [] (Just name) ifLn ifDoc
        dbgResult flattening 1 "Flattened [fSR.GS-WN] to" $
          (defns, GroupRef name (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef (GroupScheme WithNeither (Just cts) ifLn _ifDoc) = do
        boxed $ do
          dbgLn flattening 1 $ "[fSR] GroupScheme"
          dbgBLabel flattening 1 "CTS " cts
          dbgLn flattening 1 $ "IFLN " ++ maybe "(none)" show ifLn
        error $ "TODO flattenSchemaRef > GroupScheme with no name/reference"

      flattenSchemaRef gs@(ChoiceScheme (WithRef ref) _ifCtnts ifLn ifDoc) = do
        dbgBLabel flattening 1 "[fSR] CS-WR " gs
        dbgResult flattening 1
          "Flattened [fSR.CS-WR, just converting to type reference] to" $
            ([], TypeRef ref (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef gs@(ChoiceScheme (WithName name) (Just sub)
                                        ifLn ifDoc) = do
        dbgBLabel flattening 1 "[fSR] CS-WN " gs
        defns <- indenting $
          flattenComplexTypeScheme sub [] (Just name) ifLn ifDoc
        dbgResult flattening 1 "Flattened [fSR.GS-WN] to" $
          (defns, TypeRef name (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef (ChoiceScheme WithNeither (Just cts) ifLn _ifDoc) = do
        boxed $ do
          dbgLn flattening 1 $ "[fSR] ChoiceScheme"
          dbgBLabel flattening 1 "CTS " cts
          dbgLn flattening 1 $ "IFLN " ++ maybe "(none)" show ifLn
        error $ "TODO flattenSchemaRef > ChoiceScheme with no name/reference"

      flattenSchemaRef (UnprocessedXML _ ifLn ifDoc) = do
        dbgResult flattening 1
          "Flattened [fSR.UNPROC] to" ([], RawXML ifLn ifDoc)

      flattenSchemaRef s = do
        boxed $ do
          dbgLn flattening 1 $ "[fSR] flattenSchemaRef"
          dbgBLabel flattening 1 "arg " s
        error $ "TODO flattenSchemaRef > additional case:"


      flattenAttributeGroupRef ::
        NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      flattenAttributeGroupRef n@(WithName name) contents l d = do
        dbgLn flattening 1 $ "[fAGR] WithName "
        refs <- indenting $ flattenAttributeGroupItem n contents l d
        dbgResult flattening 1 (showQName name ++ " [fAGR] flattened to") $
          (refs, AttributeRef name Optional)
      flattenAttributeGroupRef (WithRef ref) [] _ln _d = do
        dbgLn flattening 1 $ "[fAGR] WithRef "
        dbgResult flattening 1 (showQName ref ++ " [fAGR] flattened to") $
          ([], AttributeRef ref Optional)
      flattenAttributeGroupRef nameRef contents _ln _d = do
        boxed $ do
          dbgLn flattening 1 $ "flattenAttributeGroupRef"
          dbgBLabel flattening 1 "NAMEREF " nameRef
          dbgBLabel flattening 1 "CONTENTS " contents
        error $ "TODO flattenAttributeGroupRef > unmatched"

      flattenSingleAttributeRef ::
        NameOrRefOpt -> Maybe String
        -> QNameOr -> String -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      flattenSingleAttributeRef (WithRef ref) _ Neither useStr _ _ = do
        dbgLn flattening 1 $ "[fSAR] WithRef+Neither "
        let res = AttributeRef ref (stringToAttributeUsage useStr)
        dbgResult flattening 1
          (showQName ref ++ " [fSAR] flattened to") ([], res)
      flattenSingleAttributeRef (WithName nam) (Just hn) (NameRef t) m l d = do
        dbgLn flattening 1 $ "[fSAR] WithRef+NameRef "
        let defn = AttributeDefn nam nam
                     (SingleAttributeDefn t (stringToAttributeUsage m) hn)
                     l d
            ref = AttributeRef nam (stringToAttributeUsage m)
        fileNewDefinition defn
        dbgResult flattening 1
          (showQName nam ++ " [fSAR] flattened to") ([defn], ref)
      flattenSingleAttributeRef (WithName nam) (Just hn) (Nested t) m l d = do
        dbgLn flattening 2 $ "[fSAR] Nested with given hn"
        (nDefns, nRef) <- flattenSchemaRef t
        dbgBLabel flattening 3 "NREF :: Reference = " nRef
        dbgBLabel flattening 3 "NDEFNS :: [Definition] = " nDefns
        typeQName <- case nRef of
          TypeRef tqn _ _ _ _ -> return tqn
          els -> do
            boxed $ do
              dbgLn flattening 1
                "[fSAR] Nested NON-TYPE, given Haskell name (hn)"
              dbgBLabel flattening 1 "NAM :: QName = " nam
              dbgBLabel flattening 1 "HN :: String = " hn
              dbgBLabel flattening 1 "T :: DataScheme = " t
              dbgBLabel flattening 1 "NREF :: Reference = " nRef
              dbgBLabel flattening 1 "NDEFNS :: [Definition] = " nDefns
              dbgLn flattening 1 $ "MODE " ++ m
            error $ "Expected TypeRef for nRef but found " ++ bpp els
        let defn = AttributeDefn nam nam
                     (SingleAttributeDefn typeQName (stringToAttributeUsage m)
                                          hn)
                     l d
            ref = AttributeRef nam (stringToAttributeUsage m)
        dbgBLabel flattening 3 "DEFN :: Definition = " defn
        dbgBLabel flattening 3 "REF :: Reference = " ref
        dbgResult flattening 2
          (showQName nam ++ " [fSAR] flattened to") (nDefns ++ [defn], ref)
      flattenSingleAttributeRef (WithName nam) _hn Neither mode _l _d = do
        -- This can legitimately arise, for example, in an <extension>
        -- where the USE of an existing attribute is set, but the type
        -- is left alone.
        --
        -- Translation assumes the name is defined elsewhere, and we
        -- have only a reference here.
        return ([], AttributeRef nam (stringToAttributeUsage mode))
        {-
        -- Old translation idea:
        --
        -- The type can be known from this call to QDHXB, or from a
        -- hint.
        ifDefn <- getAttributeDefn nam
        case ifDefn of
          Just defn -> liftIO $ do
            putStrLn "+--------------------"
            putStrLn $ "| [fSAR] With name, type neither nested nor named"
            putStrLn $ outBlock $ labelBlock "| NAM " $ block nam
            putStrLn $ outBlock $ labelBlock "| IFHNAME " $ block hn
            putStrLn "| IFTYPE (neither nested nor named)"
            putStrLn $ "| MODE " ++ mode
            putStrLn $ "| LN " ++ show l
            putStrLn $ "| nam -> DEFN " ++ bpp defn
            putStrLn "+--------------------"
            error "TODO flattenSingleAttributeRef > unmatched case"
          Nothing -> do
            ifHint <- getAttributeTypeHint nam
            case ifHint of
              Just (AttributeTypeHint _ _ hType) ->
                return ([], AttributeRef nam (stringToAttributeUsage mode))
              Nothing -> liftIO $ do
                putStrLn "+--------------------"
                putStrLn $
                  "| [fSAR] Name has no type defn, type neither nested nor named"
                putStrLn $ outBlock $ labelBlock "| NAM " $ block nam
                putStrLn $ "|   URI " ++ show (qURI nam)
                putStrLn $ "|   core name " ++ qName nam
                putStrLn $ outBlock $ labelBlock "| IFHNAME " $ block hn
                putStrLn "| IFTYPE (neither nested nor named)"
                putStrLn $ "| MODE " ++ mode
                putStrLn $ "| LN " ++ show l
                putStrLn $ "| IFHINT " ++ show ifHint
                putStrLn "+--------------------"
                error "TODO flattenSingleAttributeRef > unmatched case"
        -}
      flattenSingleAttributeRef (WithName nam) Nothing (Nested t) m _ _ = do
        boxed $ do
          dbgLn flattening 1 $ "[fSAR] Nested with no hn"
          dbgBLabel flattening 1 "NAM " nam
          dbgBLabel flattening 1 "T " t
          dbgLn flattening 1 $ "MODE " ++ m
        error "TODO Nested with no hn"
      flattenSingleAttributeRef nameRef ifHName maybeType mode l _ = liftIO $ do
        putStrLn "+--------------------"
        putStrLn $ "| [fSAR] flattenSingleAttributeRef"
        putStrLn $ outBlock $ labelBlock "| NAMEREF " $ block nameRef
        putStrLn $ outBlock $ labelBlock "| IFHNAME " $ block ifHName
        putStrLn $ outBlock $ labelBlock "| IFTYPE " $ block maybeType
        putStrLn $ "| MODE " ++ mode
        putStrLn $ "| LN " ++ show l
        putStrLn "+--------------------"
        error "TODO flattenSingleAttributeRef > unmatched case"


      flattenElementSchemeRef ::
        Maybe DataScheme -> Maybe QName -> Maybe QName -> Maybe QName
        -> Maybe Int -> Maybe Int -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      -- flattenElementSchemeRef contents ifName ifType ifRef ifLower ifUpper =

      flattenElementSchemeRef Nothing Nothing Nothing (Just r) lower upper
                              ln _ifDoc = do
        dbgLn flattening 1 $ "[fESR.1] ref only, contents/name/type Nothing"
        let result = ElementRef r lower upper ln
        dbgLn flattening 1 $ "Flattening element schema with reference only"
        dbgBLabel flattening 1 "  to " result
        dbgResult flattening 1
          ("Ref to " ++ showQName r ++ " flattened [fESR.2] to")
          ([], result)

      flattenElementSchemeRef Nothing (Just n)
                              (Just t@(QName resolvedName _resolvedURI _))
                              Nothing lo up ln ifDoc = do
        dbgLn flattening 1 $ "[fESR.3] contents/ref Nothing, have name+type"
        dbgBLabel flattening 3 "- n " n
        dbgBLabel flattening 3 "- t " t
        isKnown <- isKnownType t
        dbgBLabel flattening 3
          ("- Checking whether " ++ resolvedName ++ " is known: ") isKnown
        indenting $ if isKnown
          then (do impl <- freshenStringForBinding Nothing (Just n) $ qName n
                   let defn = ElementDefn n t impl ln ifDoc
                       ref = ElementRef n lo up ln
                   fileNewDefinition defn
                   dbgLn flattening 1 $ "Flattening schema with type"
                   -- dbgBLabel flattening 1 "       " e
                   dbgBLabel flattening 1 "    to " defn
                   dbgBLabel flattening 1 "       " ref
                   dbgResult flattening 1
                     ("Name ref " ++ showQName n
                       ++ " flattened [fESR.4] to")
                     ([defn], ref))
          else (do impl <- freshenStringForBinding Nothing (Just n) $ qName n
                   let defn = ElementDefn n t impl ln ifDoc
                       ref = ElementRef n lo up ln
                   fileNewDefinition defn
                   dbgLn flattening 1
                     "> Flattening element schema with name and type"
                   dbgBLabel flattening 1 "     " defn
                   dbgBLabel flattening 1 "     " ref
                   dbgResult flattening 1
                     ("Ref to " ++ showQName n ++ " flattened [fESR.5] to")
                       ([defn], ref))

      flattenElementSchemeRef s@(Just (CTS _ _ Nothing _ _))
                              n@(Just nam) t@Nothing r@Nothing lower upper
                              ln ifDoc = do
        dbgLn flattening 1 $ "[fESR.6] t and r and Nothing"
        -- impl <- freshenStringForBinding Nothing n $ qName nam
        prev <- flattenElementSchemeItem s n t r n lower upper False ln ifDoc
        let ref = ElementRef nam lower upper ln
        dbgLn flattening 1
          "Flattening element schema with name and nested complex type"
        dbgBLabel flattening 1 "       " s
        dbgBLabel flattening 1 "    to " prev
        dbgBLabel flattening 1 "       " ref
        dbgResult flattening 1 "Flattened [fESR.7] to" (prev, ref)

      flattenElementSchemeRef s@(Just (CTS _ _ (Just schemeName) _ _))
                              n@(Just nam) t@Nothing r@Nothing
                              lower upper ln ifDoc = do
        dbgLn flattening 1 $ "[fESR.8] CTS name, scheme name, no t, no r"
        indenting $ do
          dbgBLabel flattening 1 "CONTENTS " s
          dbgLn flattening 1 $ "SCHEMANAME (inner name) " ++ show schemeName
          dbgLn flattening 1 $ "NAM " ++ show nam
          dbgLn flattening 1 $ "IFTYPE Nothing"
          dbgLn flattening 1 $ "IFREF  Nothing"
          dbgLn flattening 1 $ "LOWER " ++ show lower
          dbgLn flattening 1 $ "UPPER " ++ show upper
          dbgLn flattening 1 $ "LN " ++ show ln
        -- impl <- freshenStringForBinding Nothing n $ qName nam
        prev <- flattenElementSchemeItem s n t r n lower upper False ln ifDoc
        dbgBLabel flattening 1 "- prev " prev
        let ref = ElementRef nam lower upper ln
        dbgBLabel flattening 1 "- ref " ref
        dbgResult flattening 1 "Flattened [fESR.9] to" (prev, ref)

      flattenElementSchemeRef ctnts ifName ifType ifRef lower upper _ _ = do
        boxed $ do
          dbgLn flattening 1 $ "[fESR.10] flattenElementSchemeRef"
          dbgBLabel flattening 1 "CONTENTS " ctnts
          dbgLn flattening 1 $ "IFNAME " ++ show ifName
          dbgLn flattening 1 $ "IFTYPE " ++ show ifType
          dbgLn flattening 1 $ "IFREF " ++ show ifRef
          dbgLn flattening 1 $ "LOWER " ++ show lower
          dbgLn flattening 1 $ "UPPER " ++ show upper
        error "TODO flattenSchemaRef > unmatched ElementScheme"


      flattenAttributes :: [AttributeScheme] -> XSDQ [Definition]
      flattenAttributes = fmap concat . mapM flattenAttribute

      flattenAttribute :: AttributeScheme -> XSDQ [Definition]
      flattenAttribute (SingleAttribute (WithRef _) _ Neither _ _) = do
        dbgLn flattening 1 $ "[fA] single attribute by ref"
        dbgLn flattening 1 $ "- Defined elsewhere --- returning []"
        return []
      flattenAttribute sa@(SingleAttribute (WithName n) (Just hn) (NameRef typ)
                                           mode d) = do
        dbgBLabel flattening 1 "[fA] single attribute with type reference " sa
        indenting $ do
          let defn = AttributeDefn n n (SingleAttributeDefn typ
                                          (stringToAttributeUsage mode) hn)
                                   Nothing d
          fileNewDefinition defn
          dbgResult flattening 1 "Flattened [fA] to" $ [defn]
      flattenAttribute sa@(SingleAttribute (WithName n) (Just hn) (Nested ds)
                                           mode d) = do
        dbgBLabel flattening 1 "[fA] Single attribute with nested type " sa
        (defs, ref) <- indenting $ flattenSchemaRef ds
        dbgLn flattening 1 $ "(Back in flattenAttribute)"
        dbgBLabel flattening 1 "- defs " defs
        dbgBLabel flattening 1 "- ref " ref
        case ref of
          TypeRef qn (Just 1) (Just 1) _ _ -> do
            dbgLn flattening 1 $
              "Case for TypeRef " ++ showQName qn ++ ", min/max single"
            let defn = AttributeDefn n n
                         (SingleAttributeDefn qn (stringToAttributeUsage mode)
                                              hn)
                         Nothing d
            fileNewDefinition defn
            dbgResult flattening 1 "Flattened [fA] to" $ defs ++ [defn]
          TypeRef qn Nothing Nothing _ _ -> do
            dbgLn flattening 1 $
              "Case for TypeRef " ++ showQName qn ++ ", no bounds"
            let defn = AttributeDefn n n
                         (SingleAttributeDefn qn (stringToAttributeUsage mode)
                                              hn)
                         Nothing d
            fileNewDefinition defn
            dbgResult flattening 1 "Flattened [fA] to" $ defs ++ [defn]
          TypeRef qn mn (Just 1) _ _ -> do
            dbgLn flattening 1 $
              "Case for TypeRef " ++ showQName qn ++ ", min bound "
                ++ show mn ++ ", max bound 1"
            boxed $ do
              dbgLn flattening 1 $ "flattenAttribute nested single"
              dbgBLabel flattening 1 "N " n
              dbgBLabel flattening 1 "DS " ds
              dbgLn flattening 1 $ "MODE " ++ mode
              dbgLn flattening 1 $ "D " ++ show d
              dbgLn flattening 1 $ ""
              dbgLn flattening 1 $ "flattenAttribute nested single (II)"
              dbgBLabel flattening 1 "DEFS" defs
              dbgBLabel flattening 1 "REF" ref
            error $ "minOccurs " ++ show mn ++ ", max 1 for attribute type "
                    ++ showQName qn ++ " of " ++ showQName n ++ " not allowed"
          TypeRef qn mn Nothing _ _ ->
            error $ "minOccurs " ++ show mn ++ ", no max for attribute type "
                    ++ showQName qn ++ " of " ++ showQName n ++ " not allowed"
          TypeRef qn mn mx _ _ ->
            error $ "minOccurs " ++ show mn ++ ", maxOccurs " ++ show mx ++
              " for attribute type "  ++ showQName qn ++
              " of " ++ showQName n ++ " not allowed"
          _ -> error $ "Nested type " ++ bpp ref ++
                 " for attribute " ++ showQName n ++ " not allowed"
      flattenAttribute ag@(AttributeGroup (WithName n) schemes d) = do
        dbgBLabel flattening 1 "[fA] Attribute group with name reference " ag
        indenting $ do
          let schemesNames = map grabNameAndUsage schemes
              defn = AttributeDefn n n
                       (AttributeGroupDefn schemesNames (qName n))
                       Nothing d
          fileNewDefinition defn
          sub <- fmap concat $ mapM flattenAttribute schemes
          dbgResult flattening 1 "Flattened [fA] to" $ sub ++ [defn]
      flattenAttribute a = do
        boxed $ do
          dbgLn flattening 1 $ "flattenAttribute "
          dbgBLabel flattening 1 "ARG " a
        error "TODO flattenAttribute missing case"

  -- | Converting XML `Content` to `DataScheme`s.
  decodeXML = inputSchemaItems
    where

      -- | Rewrite otherwise-unstructured parsed XML content structures as a
      -- sequence of internal XSD representations.
      inputSchemaItems :: String -> [Content] -> XSDQ [DataScheme]
      inputSchemaItems outer items = do
        res <- inputSchemaItems' outer items
        -- dbgLn flattening 1 $ show res
        return res

      inputSchemaItems' :: String -> [Content] -> XSDQ [DataScheme]
      inputSchemaItems' outer items = do
        dbgPt input 1 $ "inputSchemaItems' with \"" ++ outer ++ "\""
        dbgBLabel input 4 "  items " items
        res <- mapM (\(s, i) ->
                       indenting $ inputSchemaItem (outer ++ show i) s) $
                    zip items disambigNums
        -- dbgLn inputs 3 $ show res
        return res

      inputSchemaItem :: String -> Content -> XSDQ DataScheme
      inputSchemaItem o e@(Elem (Element q a c l)) = do
        dbgPt input 3 $ "[iSI] Encoding element " ++ showContent e
        ifDoc <- getAnnotationDocFrom c
        res <- indenting $ inputElement q a c o l ifDoc
        dbgBLabel input 3 "  Encoding result " res
        return res
      inputSchemaItem _ (Text _) = do
        dbgPt input 3 "[iSI] Dropping Text entry "
        return Skip
      inputSchemaItem _ (CRef txt) = do
        dbgPt input 3 $ "[iSI] Dropping CRef entry " ++ txt
        return Skip


      inputElement ::
        QName -> [Attr] -> [Content] -> String -> Maybe Line -> Maybe String
        -> XSDQ DataScheme

      inputElement (QName "element" _ _) ats content outer ln _d = do
        dbgPt input 3 $ "inputElement for element tag"
        dbgLn input 3 $ "  outer tag " ++ outer
        included <- indenting $
          fmap (filter nonSkip) $ inputSchemaItems (outer ++ "Elem")  $
            filter isNonKeyNonNotationElem content
        typeQName <- pullAttrQName "type" ats
        nameQName <- pullAttrQName "name" ats
        refQName <- pullAttrQName "ref" ats
        let ifAbstr = pullAttr "abstract" ats
            isAbstract = case ifAbstr of
                           Just "true" -> True
                           _ -> False
        ifDoc <- getAnnotationDocFrom content
        let ifId = pullAttr "id" ats
        sub <- case included of
                 [] -> return Nothing
                 [x] -> return $ Just x
                 _ -> do
                   dbgLn input 3 $
                     "More than one subelement to <element>" ++ ifAtLine ln
                   dbgBLabel input 3 "ATS " ats
                   dbgBLabel input 3 "CONTENT " content
                   dbgBLabel input 3 "INCLUDED " included
                   error $
                     "More than one subelement to <element>" ++ ifAtLine ln
        tagName <- case nameQName of
                        Just _ -> return nameQName
                        Nothing -> case refQName of
                                    Just _ -> return refQName
                                    Nothing -> fmap Just $ inDefaultNamespace $
                                                 outer ++ "Element"
        dbgResult input 1 "Element inputElemen3 result" $
          ElementScheme sub nameQName typeQName refQName ifId tagName
                   (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
                   (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
                   isAbstract ln ifDoc

      inputElement q@(QName "attribute" _ _) a c o l _d = do
        dbgLn input 3 $ "inputElement for attribute tag"
        dbgPt input 3 $ "outer tag " ++ o
        ifDoc <- indenting $ getAnnotationDocFrom c
        scheme <- indenting $
          encodeAttribute (o ++ "Attr") q a (filter isFocusElem c) l ifDoc
        dbgBLabel input 3 "- scheme " scheme
        let implName = case attributeSchemeQname scheme of
                         Nothing -> o ++ "Attr"
                         Just s -> qName s
        return $ AttributeScheme scheme implName l ifDoc

      inputElement q@(QName "attributeGroup" _ _) a c o l _d = do
        dbgLn input 3 $ "inputElement for attribute group"
        dbgPt input 3 $ "outer tag " ++ o
        ifDoc <- getAnnotationDocFrom c
        scheme <- indenting $
          encodeAttribute (o ++ "AtrGrp") q a
                          (filter isNonKeyNonNotationElem c) l ifDoc
        dbgBLabel input 3 "- scheme " scheme
        let implName = case attributeSchemeQname scheme of
                         Nothing -> o ++ "Attr"
                         Just s -> qName s
        return $ AttributeScheme scheme implName l ifDoc


      inputElement (QName "complexType" _ _) ats ctnts outer l d = do
        (pr, atspecs', atgrspecs') <- separateComplexContents ctnts l
        name <- pullAttrQName "name" ats
        case pr of
          Nothing -> do
            atrSpecs <- mapM (encodeAttributeScheme $ outer ++ "Cplx") $
                          atspecs' ++ atgrspecs'
            return $ CTS (Composing [] atrSpecs) [] name l d
          Just (PB (_, tag, _uri, _pfx, qn, ats', subctnts, _)) ->
            case tag of

            "sequence" -> do
              ct <- encodeSequenceTypeScheme (outer ++ "Complex") subctnts
                                             (atspecs' ++ atgrspecs')
              return $ CTS ct [] name l d

            "choice" -> do
              dbgLn input 3 "inputElement > complexType > case \"choice\""
              choiceName <- pullAttrQName "name" ats'
              cts <- indenting $ encodeChoiceTypeScheme choiceName ats' subctnts
              let res = CTS cts [] name l d
              dbgResult input 3 "Result is " res

            "complexContent" -> do
              dbgLn input 3
                "      inputElement > complexType > case \"complexContent\""
              (pr', _, _) <- separateComplexContents subctnts l
              case pr' of
                Just (PB (_, _, _, _, sqn, sats', ssubcs, ssln)) ->
                  inputElement sqn (ats ++ sats')
                               (filter isNonKeyNonNotationElem ssubcs)
                               (outer ++ "Complex") ssln Nothing
                Nothing -> error
                  ("Complex content must have primary subcontents"
                   ++ ifAtLine l)

            "group" -> do
              groupName <- pullAttrQName "name" ats'
              groupRef <- pullAttrQName "ref" ats'
              groupNameOrRef <-
                nameOrRefOptDft groupName groupRef (outer ++ "Group1")
              givenMin <- pullAttrQName "minOccurs" ats'
              givenMax <- pullAttrQName "maxOccurs" ats'
              let useMin = case givenMin of
                    Nothing -> Just 1
                    Just n -> Just $ read $ qName n
              let useMax = case givenMax of
                    Nothing -> Just 1
                    Just n | qName n == "unbounded" -> Nothing
                    Just n -> Just $ read $ qName n
              attrSpecs <- mapM (\(spec, n) ->
                                   encodeAttributeScheme (outer ++ "Group2Attr"
                                                          ++ show n) spec)
                $ zip atspecs' disambigNums
              contained <- inputSchemaItems' (outer ++ "Group3") subctnts
              let content = case filter nonSkip contained of
                              [] -> Nothing
                              [x] -> Just x
                              _ -> error
                                     "Multiple contents in <group> not allowed"
              dbgResult input 3 "inputElement result" $
                CTS (finishGroup groupNameOrRef content
                                               useMin useMax)
                                  attrSpecs name l d
                where finishGroup r@(WithRef _) c@Nothing mn mx =
                        Group r c mn mx
                      finishGroup r@(WithRef _) (Just Skip) mn mx =
                        Group r Nothing mn mx
                      finishGroup (WithRef r) _ _ _ = error $
                        "Group complexType with ref " ++ show r
                        ++ " should have no contents"
                      finishGroup n c mn mx = Group n c mn mx



            "simpleContent" -> do
              dbgLn input 3
                "      inputElement > complexType > case \"simpleContent\""
              error "inputElement > complexType > case \"simpleContent\""

            _ -> boxed $ do
              dbgLn input 1 "inputElement > complexType > another tag case"
              dbgBLabel input 1 "ATS " ats
              dbgBLabel input 1 "CTNTS " $ filter isElem ctnts
              dbgLn input 1 $    "OUTER " ++ show outer
              dbgLn input 1 $    "L " ++ show l
              dbgLn input 1 $    "D " ++ show d
              dbgLn input 1      "-------"
              dbgBLabel input 1 "ATSPECS' " atspecs'
              dbgBLabel input 1 "ATGRSPECS' " atgrspecs'
              dbgBLabel input 1 "NAME " name
              dbgLn input 1      "-------"
              dbgLn input 1 $    "TAG " ++ show tag
              dbgBLabel input 1 "QN " qn
              dbgBLabel input 1 "ATS' " ats'
              dbgBLabel input 1 "SUBCTNTS " $ filter isElem subctnts
              error "inputElement > complexType > unmatched"


      inputElement (QName "simpleType" _ _) ats ctnts outer ifLn ifDoc = do
        let ctnts' = filter isElem ctnts
        dbgPt input 1 "Input element is simpletype"
        dbgLn input 3 $ "  Outer name " ++ outer
        indenting $ case separateSimpleTypeContents ats ctnts' of

          (nam, One restr, Zero, Zero) -> do
            dbgPt input 3 "Subcase restr"
            qnam <- mapM decodePrefixedName nam
            res <- indenting $ encodeSimpleTypeByRestriction qnam
                                 (maybe (outer ++ "Simple") (outer ++) nam)
                                 ats restr
            dbgResult input 3 "Subcase result" res

          (ifNam, Zero,
           One (Elem (Element (QName "union" _ _) ats' cs' _)), Zero) -> do
            let outerUnion = outer ++ "Union"
                nam = maybe outerUnion id ifNam
            dbgPt input 2 "Subcase union"
            dbgBLabel input 3 "- ifNam " $ show ifNam
            dbgBLabel input 3 "- cs' " cs'
            qnam <- decodePrefixedName nam
            dbgBLabel input 3 "- qnam " qnam
            dbgPt input 1 "Calling inputSchemaItem3' "
            nestedAlts <- indenting $
                      inputSchemaItems' outerUnion $ filter isElem cs'
            dbgBLabel input 3 "- nestedAlts " nestedAlts
            -- Extract from the memberTypes attribute here
            membersAttr <- pullAttrQNameList "memberTypes" ats'
            dbgBLabel input 3 "- membersAttr " membersAttr
            let members = maybe [] id membersAttr
            dbgResult input 3 "Subcase result" $
              STS (Just qnam) (Union nestedAlts members) ifLn ifDoc

          (ifNam, Zero, Zero,
           One (Elem (Element (QName "list" _ _) ats' ctnts'' _))) -> do
            dbgPt input 2 "Subcase list"
            itemTypeAttr <- pullAttrQName "itemType" ats'
            let simpleWithin = pullContent "simpleType" ctnts''
            indenting $ case (itemTypeAttr, simpleWithin) of

              (Nothing, Zero) -> error $
                "Simple type list without itemType attribute" ++ ifAtLine ifLn

              (Nothing, One node) -> do
                dbgPt input 2 "Subcase with included element type"
                tds <- indenting $ inputSchemaItem (outer ++ "List") node
                thisName <- case ifNam of
                  Just n -> inDefaultNamespace n
                  Nothing -> do
                    let tdsLabel = labelOf tds
                    case tdsLabel of
                      Just n -> return $ withSuffix "Element" n
                      Nothing -> do
                        fresh <- liftQtoXSDQ $ newName "List"
                        return $ QName (nameBase fresh) Nothing Nothing
                dbgResult input 3 "Subcase result" $
                  STS (Just thisName) (List Nothing (Just tds))
                                   ifLn ifDoc

              (Just itemType, Zero) -> do
                dbgPt input 2 "Subcase with element type reference"
                qnam <- case ifNam of
                  Just n -> decodePrefixedName n
                  Nothing -> return $ QName ("List_" ++ qName itemType)
                                            (qURI itemType) (qPrefix itemType)
                dbgResult input 2 "Subcase result" $
                  STS (Just qnam)
                                   (List (Just itemType) Nothing) ifLn ifDoc
              (x, y) -> do
                boxed $ do
                  dbgLn input 1 "Disallowed subsubcase within list subcase"
                  dbgBLabel input 1 "ATS " ats
                  dbgBLabel input 1 "CTNTS' " ctnts'
                  dbgLn input 1 $ "OUTER " ++ outer
                  dbgBLabel input 1 "X " x
                  dbgBLabel input 1 "Y " y
                error $
                  "Disallowed subcase within subcase list" ++ ifAtLine ifLn



          (ifName, zomRestr, zomUnion, zomList) -> do
            boxed $ do
              dbgLn input 1
                "TODO inputElement > simpleType > another separation case"
              dbgBLabel input 1 "ATS " ats
              dbgBLabel input 1 "CTNTS' " ctnts'
              dbgLn input 1 $ "OUTER " ++ outer
              dbgLn input 1 $ "IFNAME " ++ show ifName
              dbgBLabel input 1 "ZOMRESTR " zomRestr
              dbgBLabel input 1 "ZOMUNION " zomUnion
              dbgBLabel input 1 "ZOMLIST " zomList
            error $ "TODO inputElement > simpleType > another separation case"
              ++ ifAtLine ifLn

      inputElement (QName "annotation" _ _) _ _ _ _ _ = do
        -- We do nothing with documentation and other annotations; currently
        -- there is no way to pass Haddock docstrings via the TH API.
        dbgPt input 2 $ "Dropping <annotation> element"
        return Skip

      inputElement (QName tag _ _) _ _ _ l _d
                   | tag=="include" || tag=="import" = do
        -- Skipping these documents for now
        dbgLn input 2 $
          "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
        return Skip

      inputElement (QName tagname _ _) _ _ _ _ _
        | tagname == "key" || tagname == "keyref" = do
        dbgPt input 2 $ "Dropping <" ++ tagname ++ "> entry "
        return Skip

      inputElement (QName "sequence" _ _) ats ctnts outer ifLn ifDoc = do
        ifName <- pullAttrQName "name" ats
        dbgLn input 2 $
          maybe "- Sequence (unnamed)"
                (\n -> "- Sequence \"" ++ showQName n ++ "\"")
                ifName
        included <- indenting $ inputSchemaItems (outer ++ "Seq") ctnts
        name <- useNameOrWrap ifName outer "Seq"
        dbgResult input 2 "Sequence result" $
          CTS (Composing (filter nonSkip included) [])
                            [] (Just name) ifLn ifDoc

      inputElement (QName "restriction" _ _) ats ctnts outer ifLn ifDoc = do
        dbgPt input 2 $ "Restriction, outer name " ++ outer
        ifDoc' <- getAnnotationDocFrom ctnts
        ifName <- pullAttrQName "name" ats
        case pullAttr "base" ats of
          Just base -> do
            baseQName <- decodePrefixedName base
            let thisName = case ifName of
                  Just _ -> ifName
                  Nothing -> Just $
                    QName (outer ++ "Restricted" ++ qName baseQName)
                          (qURI baseQName) (qPrefix baseQName)
            dbgResult input 2 "Restriction result" $
              CTS (ComplexRestriction baseQName) []
                                thisName ifLn (pickOrCombine ifDoc ifDoc')
          Nothing -> error "restriction without base"


      inputElement (QName "extension" _ _) ats ctnts outer ifLn ifDoc = do
        dbgPt input 2 $ "Extension, outer name " ++ outer
        maybeBase <- pullAttrQName "base" ats
        let base = maybe (error $ "<extension> without base" ++ ifAtLine ifLn)
                      id maybeBase
        ifName <- pullAttrQName "name" ats
        name <- useNameOrWrap ifName outer "Ext"
        (ext, newAttrs, newAttrGroups) <- separateComplexContents ctnts ifLn
        dbgPt input 3 "Complex extension"
        dbgPt input 3 $ "outer name " ++ outer
        dbgBLabel input 3 "- base " base
        dbgBLabel input 3 "- ext " ext
        dbgBLabel input 3 "- newAttrs " newAttrs           -- :: [Content]
        dbgBLabel input 3 "- newAttrGroups " newAttrGroups -- :: [Content]
        newAttrSchemes <- mapM (encodeAttributeScheme $
                                  outer ++ "ExtAnn" ++ qName base) newAttrs
        dbgBLabel input 3 "- newAttrSchemes " newAttrSchemes
        newAttrGroupSchemes <- mapM (encodeAttributeScheme $
                                       outer ++ "ExtGrp" ++ qName base)
                                    newAttrGroups
        dbgBLabel input 3 "- newAttrGroupSchemes " newAttrGroupSchemes
        let attrSchemes = newAttrSchemes ++ newAttrGroupSchemes
        res <- case ext of
          Nothing -> do
            return $
              CTS (Extension base []) attrSchemes (Just name) ifLn ifDoc
          Just (PB (e,_,_,_,_,_,_,_)) -> do
            e' <- indenting $ inputSchemaItem (outer ++ "ExtB") e
            return $
              CTS (Extension base [e']) attrSchemes (Just name) ifLn ifDoc
        dbgBLabel input 2 "  Extension result " res
        return res


      inputElement (QName "group" _ _) ats ctnts outer ifLn ifDoc = do
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        let subOuter = outer ++ "Group"
        let filtered = filter isFocusElem ctnts
        dbgLn input 3 $ "inputElement case group" ++ ifAtLine ifLn
        dbgPt input 3 $ "For <group> schema"
          ++ maybe "(no line num)" (\x -> " at " ++ show x) ifLn
          ++ ":"
        dbgLn input 3 $ "  outer name " ++ outer
        dbgLn input 3 $ "  name from attributes " ++ maybe "(none)" qName name
        -- dbgLn input 3 $
        --   "  filtered content " ++ show (map showContent filtered)
        indenting $ case filtered of
          (Elem (Element (QName "choice" _ _) attrs' ctnts' ifLn')):[] -> do
            dbgPt input 3 "choice subcase"
            ts <- indenting $ encodeChoiceTypeScheme name attrs' ctnts'
            dbgResult input 3 "Subcase result" $
              finishGroupScheme (nameOrRefOpt name ref) (Just ts) ifLn' ifDoc
          (Elem (Element (QName "sequence" _ _) _ats ctnts' _)):[] -> do
            dbgPt input 3 "sequence subcase after (filter isElem ctnts)"
            indenting $ do
              dbgBLabel input 3 "- group attrs " ats
              dbgBLabel input 3 "- name " name
              dbgBLabel input 3 "- ctnts " $ filter isElem ctnts
              dbgBLabel input 3 "- ctnts' " $ filter isElem ctnts'
            seqn <- indenting $ encodeSequenceTypeScheme subOuter ctnts' []
            dbgResult input 3 "Subcase result" $
              finishGroupScheme (nameOrRefOpt name ref) (Just seqn) ifLn ifDoc
          (Elem (Element (QName "all" _ _) _ats _contents ifLn')):[] -> do
            dbgPt input 3 "all subcase"
            -- ifDoc' <- getAnnotationDocFrom contents
            boxed $ do
              dbgLn input 3 $
                "TODO inputElement > group with all" ++ ifAtLine ifLn'
              dbgLn input 3 $
                "ATS " ++ (intercalate "\n    " $ map showAttr ats)
              dbgLn input 3 $ "NAME " ++ show (fmap showQName name)
              dbgLn input 3 $ "CTNTS " ++
                (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
            error $ "TODO inputElement > group with all" ++ ifAtLine ifLn'
          _ -> do
            dbgPt input 3 "Default subcase is group of nothing"
            dbgResult input 3 "Subcase result" $
              finishGroupScheme (nameOrRefOpt name ref) Nothing ifLn ifDoc

        where finishGroupScheme r@(WithRef _) n@Nothing l d =
                GroupScheme r n l d
              finishGroupScheme (WithRef r) _ _ _ = error $
                "Group with ref " ++ show r ++ " should not contain subforms"
              finishGroupScheme n@(WithName _) c@(Just _) l d =
                GroupScheme n c l d
              finishGroupScheme (WithName n) Nothing _ _ = error $
                "Group with name " ++ show n ++ " should contain subforms"
              finishGroupScheme n@WithNeither c@(Just _) l d =
                GroupScheme n c l d
              finishGroupScheme WithNeither Nothing _ _ = error $
                "Unnamed/unreferenced group should contain subforms"


      inputElement (QName "choice" _ _) ats ctnts outer ifLn ifDoc = do
        dbgPt input 2 "For <choice> scheme:"
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        nameRef <- nameOrRefOptDft name ref $ outer ++ "Choice"
        let minOcc = decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats
            maxOcc = decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats
        dbgPt input 3 $ "inputElement > choice" ++ ifAtLine ifLn
        dbgLn input 3 $ "-- minOccurs " ++ show minOcc
        dbgLn input 3 $ "-- maxOccurs " ++ show maxOcc
        dbgBLabel input 3 "-- ats " ats
        dbgBLabel input 3 "-- ctnts " $ filter isElem ctnts
        ts <- indenting $ encodeChoiceTypeScheme name ats ctnts
        dbgResult input 3 "Choice encoding" $
          ChoiceScheme nameRef (Just ts) ifLn ifDoc

      inputElement (QName "any" _ _) ats _ outer ifLn ifDoc = do
        dbgPt input 2 "Fo3 <any> scheme:"
        ifName <- pullAttrQName "name" ats
        name <- useNameOrWrap ifName outer "Any"
        dbgLn input 3 $ "TODO <any>" ++ ifAtLine ifLn
        dbgLn input 3 $ "NAME " ++ showQName name
        dbgLn input 3 $ "OUTER " ++ outer
        dbgResult input 3 "Encoded as" $ UnprocessedXML (Just name) ifLn ifDoc

      inputElement (QName "notation" _ _) _ _ _ _ _ = do
        dbgPt input 2 "For <notation> scheme:"
        dbgResult input 3 "Encoded as" Skip

      inputElement (QName tag _ _) ats ctnts outer ifLn _ifDoc = do
        boxed $ do
          dbgLn input 2 $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn
          dbgLn input 3 $ "TAG " ++ show tag
          dbgBLabel input 3 "ATS " ats
          dbgBLabel input 3 "CTNTS " $ filter isElem ctnts
          dbgLn input 3 $ "OUTER " ++ outer
        error $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn


      encodeSequenceTypeScheme ::
        String -> [Content] -> [Content] -> XSDQ ComplexTypeScheme
      encodeSequenceTypeScheme outer subcontents attrSpecs = indenting $ do
        dbgLn input 2 $
          "encodeSequenceTypeScheme outer=\"" ++ outer ++ "\""
        included <- indenting $ inputSchemaItems' (outer ++ "Seq") subcontents
        atrSpecs <- indenting $
          mapM (\(e, n) -> encodeAttributeScheme (outer ++ "Seq" ++ show n) e) $
            zip attrSpecs disambigNums
        return $ Composing (filter nonSkip included) atrSpecs

      encodeChoiceTypeScheme ::
        Maybe QName -> [Attr] -> [Content] -> XSDQ ComplexTypeScheme
      encodeChoiceTypeScheme ifNam attrs allCtnts = indenting $ do
        dbgLn input 2 "encodeChoiceTypeScheme"
        let ctnts = filter isElem allCtnts
        dbgLn input 5 $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
        dbgLn input 5 $ "IFNAM " ++ show ifNam
        dbgLn input 5 $ "CTNTS " ++
            (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
        contentSchemes <- indenting $ mapM (inputSchemaItem "X") ctnts
        return $ Choice ifNam $ filter nonSkip contentSchemes


      encodeAttributeScheme :: String -> Content -> XSDQ AttributeScheme
      encodeAttributeScheme outer (Elem e@(Element q a allC l)) = indenting $ do
        dbgBLabel input 2 "- Encoding attribute scheme " e
        ifDoc <- getAnnotationDocFrom allC
        let c = filter isFocusElem allC
        res <- indenting $
          encodeAttribute (outer ++ "Elem") q a
                          (filter isNonKeyNonNotationElem c) l ifDoc
        dbgBLabel input 3 "  Encoding result " res
        return res
      encodeAttributeScheme _o c = do
        dbgBLabel input 2 "** Nonattribute" c
        error $ "Illegal use of encodeAttributeScheme on\n" ++ showContent c

      encodeAttribute ::
        String -> QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String
        -> XSDQ AttributeScheme
      encodeAttribute _ (QName "attribute" _ _) ats [] _ d = indenting $ do
        typeQName <- pullAttrQName "type" ats
        refQName <- pullAttrQName "ref" ats
        nameQname <- pullAttrQName "name" ats
        let nameOrRef = nameOrRefOpt nameQname refQName
        return $ SingleAttribute nameOrRef
                   (nameOrRefOptToMaybeString nameOrRef)
                   (maybe Neither NameRef typeQName)
                   (maybe "optional" id $ pullAttr "use" ats)
                   d
      encodeAttribute outer (QName "attribute" _ _) ats (st:sts) l d = do
        dbgLn input 2 $
          "encodeSequenceTypeScheme attribute outer=\"" ++ outer ++ "\""
        dbgBLabel input 3 "- ats " ats
        dbgBLabel input 3 "- st "  st
        dbgBLabel input 3 "- sts " sts
        typeQName <- pullAttrQName "type" ats
        case typeQName of
          Just n -> do
            error $ "Both named type " ++ showQName n
              ++ " and nested type spec given to attribute, " ++ showContent st
          Nothing -> do
            nameQName <- pullAttrQName "name" ats
            refQName <- pullAttrQName "ref" ats
            encodeAttributeWithNestedType (outer ++ "Attr")
                                          (nameOrRefOpt nameQName refQName)
                                          st sts
                                          (maybe "optional" id $
                                            pullAttr "use" ats)
                                          l d
      encodeAttribute o (QName "attributeGroup" _ _) ats ctnts _ d = do
        indenting $ do
          dbgLn input 2 $
            "encodeSequenceTypeScheme attributeGroup outer=\"" ++ o ++ "\""
          dbgBLabel input 3 "- ats " ats
          dbgBLabel input 3 "- ctnts " ctnts
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        let attrs = filterTagged "attribute" ctnts
            atGroups = filterTagged "attributeGroup" ctnts
        subcontents <- indenting $ mapM (encodeAttributeScheme $ o ++ "Group") $
                                     attrs ++ atGroups
        return $ AttributeGroup (nameOrRefOpt name ref) subcontents d
      encodeAttribute outer (QName n _ _) a c _ _ = do
        boxed $ do
          dbgLn input 2 n
          dbgLn input 3 $ "OUTER " ++ outer
          dbgBLabel input 3 "A " a
          dbgBLabel input 3 "C " $ filter isElem c
        error $ "Can't use encodeAttribute with <" ++ n ++ ">"

      encodeAttributeWithNestedType ::
        String -> NameOrRefOpt -> Content -> [Content] -> String
        -> Maybe Line -> Maybe String
        -> XSDQ AttributeScheme
      encodeAttributeWithNestedType outer nameOrRef tySpec [] use _ d = do
        dbgLn input 2 $
          "encodeAttributeWithNestedType outer=\"" ++ outer ++ "\""
        dbgBLabel input 3 "- nameOrRef " nameOrRef
        dbgBLabel input 3 "- tySpec " tySpec
        dbgBLabel input 3 "- use " use
        ds <- inputSchemaItem outer tySpec
        dbgBLabel input 3 "- ds " ds
        dbgResult input 3 "Result [encodeAttributeWithNestedType]:" $
          SingleAttribute nameOrRef (nameOrRefOptToMaybeString nameOrRef)
                          (Nested ds) use d
      encodeAttributeWithNestedType _ _ tySpec (s:ss) _ _ _ = do
        boxed $ do
          dbgLn input 2 "Too many nested types for attribute"
          dbgBLabel input 3 "1st one " tySpec
          dbgBLabel input 3 "2nd one " s
          dbgBLabel input 3 "others " ss
        error "TODO Too many nested types for attribute"


      -- | Separate the innards of a complexType element into:
      --
      --  1. The primary subcontents, if any
      --
      --  2. Attribute definitions
      --
      --  3. Attribute group definitions
      --
      separateComplexContents ::
        [Content] -> Maybe Line
        -> XSDQ (Maybe PrimaryBundle, [Content], [Content])
      separateComplexContents contents ifLn =
        separateComplexContents' Nothing [] [] contents

        where separateComplexContents' ::
                Maybe PrimaryBundle -> [Content] -> [Content] -> [Content]
                -> XSDQ (Maybe PrimaryBundle, [Content], [Content])
              separateComplexContents' primary annAcc annGroupAcc [] =
                return (primary, reverse annAcc, reverse annGroupAcc)
              separateComplexContents' pr as ags (e:xs) =
                case e of
                  Elem (Element q@(QName n u p) a c l) -> case n of
                    "attribute" -> separateComplexContents' pr (e:as) ags xs
                    "attributeGroup" ->
                      separateComplexContents' pr (e:as) ags xs
                    "annotation" -> separateComplexContents' pr as ags xs
                    "anyAttribute" -> separateComplexContents' pr as ags xs
                    -- TODO revisit anyAnnotation later --- key-value pairs?
                    "documentation" -> separateComplexContents' pr as ags xs
                    _ -> case pr of
                      Nothing -> separateComplexContents'
                                   (Just (PB (e, n, u, p, q, a, c, l)))
                                   as ags xs
                      Just (PB (_, n', _, _, _, _, _, _)) -> error $
                        "Multiple primary sub-elements " ++ n' ++ " and " ++ n
                        ++ " as complexType contents"
                        ++ maybe "" ((" at line " ++) . show) ifLn
                  _ -> separateComplexContents' pr as ags xs

      separateSimpleTypeContents ::
        [Attr] -> [Content] ->
          (Maybe String, ZeroOneMany Content, ZeroOneMany Content,
           ZeroOneMany Content)
      separateSimpleTypeContents attrs cts =
        (pullAttr "name" attrs,
         pullContent "restriction" cts,
         pullContent "union" cts,
         pullContent "list" cts)


      encodeSimpleTypeByRestriction ::
        Maybe QName -> String -> [Attr] -> Content -> XSDQ DataScheme
      encodeSimpleTypeByRestriction -- Note ignoring ats
          ifName outer _ (Elem (Element (QName "restriction" _ _) ats'
                                        cs ln)) = do
        dbgPt input 2 $ "encode simple by restr, outer name " ++ outer
        ifDoc <- getAnnotationDocFrom cs
        case pullAttr "base" ats' of
          Just base -> do
            dbgLn input 3 $ "- base " ++ base
            baseQName <- decodePrefixedName base
            dbgBLabel input 3 "- baseQName " baseQName
            let useName = maybe (Just $ withPrefix (outer ++ "Restr") baseQName)
                                (const ifName) ifName
                          -- TODO --- make sure this is in target namespace
            dbgBLabel input 3 "- useName " useName
            -- freshUseName <- freshenQNameForBinding (Just outer) useName
            -- freshBaseName <- freshenQNameForBinding (Just outer) baseQName
            dbgResult input 3 "Encoding result" $
              STS useName {- (Just freshUseName) -}
                               (SimpleRestriction baseQName {- freshBaseName -})
                                                  ln ifDoc
          Nothing -> error "restriction without base"
      encodeSimpleTypeByRestriction ifNam _ ats s = do
        boxed $ do
          dbgLn input 2 "TODO encodeSimpleTypeByRestriction > additional cases"
          dbgLn input 3 $ "IFNAM " ++ show ifNam
          dbgLn input 3 $ "ATS "   ++ (intercalate "\n    " $ map showAttr ats)
          case s of
            Elem (Element _ _ _ (Just l)) ->
              dbgLn input 3 $ "source line: " ++ show l
            _ -> return ()
          dbgBLabel input 3 "S " s
        error "TODO encodeSimpleTypeByRestriction > additional cases"

      -- | Decode the `String` representation of an XSD integer as a Haskell
      -- `Int`.  Might fail, so the result is `Maybe`-wrapped.
      decodeIntOrUnbound :: String -> Maybe Int
      decodeIntOrUnbound "unbounded" = Nothing
      decodeIntOrUnbound s = (readMaybe s) :: Maybe Int

      -- | Another decoder of the `String` representation of an XSD integer
      -- as a Haskell `Int`, where there may be no `String` in the first
      -- place.
      decodeMaybeIntOrUnbound1 :: Maybe String -> Maybe Int
      decodeMaybeIntOrUnbound1 = maybe (Just 1) decodeIntOrUnbound

      pullAttrQName :: String -> [Attr] -> XSDQ (Maybe QName)
      pullAttrQName str attrs = mapM decodePrefixedName (pullAttr str attrs)

      pullAttrQNameList :: String -> [Attr] -> XSDQ (Maybe [QName])
      pullAttrQNameList str attrs =
        mapM decodePrefixedNameList (pullAttr str attrs)

      -- | Assemble a `NameOrRefOpt` value from two @(`Maybe` `QName`)@
      -- values and a default `String` name.  The first argument corresponds
      -- to a possible @name@ attribute; the second argument, to a possible
      -- @ref@ attribute; the third, to a default name derived from the
      -- context of the defining element in the source XSD.
      nameOrRefOptDft ::
        Maybe QName -> Maybe QName -> String -> XSDQ NameOrRefOpt
      nameOrRefOptDft (Just _) (Just _) _ =
        error "Cannot give both name and ref attributes"
      nameOrRefOptDft (Just n) Nothing _ = return $ WithName n
      nameOrRefOptDft Nothing (Just r) _ = return $ WithRef r
      nameOrRefOptDft Nothing Nothing s = fmap WithName $ inDefaultNamespace s

      disambigNums :: [Int]
      disambigNums = [1..]

  debugSlug Skip = "Skip"
  debugSlug (ElementScheme _ ifName _ _ _ _ _ _ _ _ _) =
    maybe "element (unnamed)" (("element " ++) . qName) ifName
  debugSlug (AttributeScheme (SingleAttribute nameOrRef _ _ _ _) impl _ _) =
    "attribute " ++ show nameOrRef ++ " as " ++ impl
  debugSlug (AttributeScheme (AttributeGroup nameOrRef _ _) impl _ _) =
    "attribute group " ++ show nameOrRef ++ " as " ++ impl
  debugSlug (CTS _ _ ifName _ _) =
    maybe "complex type (unnamed)" (("complex type " ++) . qName) ifName
  debugSlug (STS ifName _ _ _) =
    maybe "simple type (unnamed)" (("simple type " ++) . qName) ifName
  debugSlug (GroupScheme nameOrRef _ _ _) = "group " ++ show nameOrRef
  debugSlug (ChoiceScheme nameOrRef _ _ _) = "choice " ++ show nameOrRef
  debugSlug (UnprocessedXML ifName _ _) =
    maybe "raw XML (unnamed)" (("raw XML " ++) . qName) ifName

-- TH calls must be after the big mutually-recursive blocks

verticalBlockablePair [t|QName|] [t|DataScheme|]
verticalBlockList [t|SimpleTypeScheme|]
verticalBlockList [t|(QName, DataScheme)|]
verticalBlockList [t|ComplexTypeScheme|]

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: QDHXBOption -> [String] -> Q [Dec]
-- | Load and translate the given XSD files with the default options.
qdhxb' :: [String] -> Q [Dec]
(qdhxb, qdhxb') = apiFunctions @DataScheme
