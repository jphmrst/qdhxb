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

-- | A sort of variation of `Maybe` with two `Just` forms, for schema
-- which allow either a @name@ or a @ref@ attribute, but not both, and
-- possibly neither.
data NameOrRefOpt =
  WithName QName  -- ^ Case for having a @name@ but no @ref@
  | WithRef QName -- ^ Case for having a @ref@  but no @name@
  | WithNeither   -- ^ Case for neither
  deriving Show

{-
nameOrRefOptToMaybeName :: NameOrRefOpt -> Maybe QName
nameOrRefOptToMaybeName (WithName qn) = Just qn
nameOrRefOptToMaybeName (WithRef qn)  = Just qn
nameOrRefOptToMaybeName (WithNeither) = Nothing
-}

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

-- | TODO Local version of `ensureUniqueInternalNames` for
-- `SimpleTypeScheme`s.
unique_internals_sts :: SimpleTypeScheme -> XSDQ (MaybeUpdated SimpleTypeScheme)
unique_internals_sts sts@(Synonym _) = return $ Same sts
unique_internals_sts sts@(SimpleRestriction _) = return $ Same sts
unique_internals_sts sts@(Union dss typeQNames) = do
  dss' <- indenting $ fmap hoistUpdate $ mapM ensureUniqueInternalNames dss
  return $ assembleIfUpdated [Upd dss'] sts $ Union (resultOnly dss') typeQNames
unique_internals_sts sts@(List _ifElemTypeQName _ifNestedTypeDS) = do
  return $ Same sts
  {- TODO Weird bug here
  ds' <- case ifNestedTypeDS of
    Nothing -> return $ Same ifNestedTypeDS
    Just ds -> ensureUniqueNames1 ds
  return $ assembleIfUpdated [Upd ds'] sts $
    List ifElemTypeQName (resultOnly ds')
-}

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

{-
Composing dss attrSchs
ComplexRestriction qn
Extension qn dss
Choice ifQn dss
Group nameOrRef ifDS ifMin ifMax
-}

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

-- | TODO Local version of `ensureUniqueInternalNames` for
-- `ComplexTypeScheme`.
unique_internals_cts ::
  ComplexTypeScheme -> XSDQ (MaybeUpdated ComplexTypeScheme)
unique_internals_cts cts@(Composing dss attrSchs) = do
  dss' <- indenting $ ensureUniqueNames' dss
  attrSchs' <- indenting $ fmap hoistUpdate $
    mapM unique_internals_attr_scheme attrSchs
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
                  QNameOr -- ^ ifType
                  String -- ^ use mode: prohibited, optional
                         -- (default), required
                  -- String -- ^ TODO Implementation type name
                  (Maybe String) -- ^ ifDoc
  | AttributeGroup NameOrRefOpt -- ^ Name or reference, or neither
                   [AttributeScheme]  -- ^ included attributes and
                                      -- attribute groups
                   (Maybe String) -- ^ ifDoc
  deriving Show

-- SingleAttribute nameOrRef ifTypeOr mode ifDoc
-- AttributeGroup nameOrRef attrSchs ifDoc

subst_attr_scheme ::
  Substitutions -> AttributeScheme -> MaybeUpdated AttributeScheme
subst_attr_scheme ss aspec@(SingleAttribute nameOrRef ifType mode ifDoc) =
  let nameOrRef' = subst_NameOrRefOpt ss nameOrRef
      ifType' = substQNameOr ss ifType
  in assembleIfUpdated [Upd nameOrRef', Upd ifType'] aspec $
       SingleAttribute (resultOnly nameOrRef') (resultOnly ifType') mode ifDoc
subst_attr_scheme ss aspec@(AttributeGroup nameOrRef schemes ifDoc) =
  let nameOrRef' = subst_NameOrRefOpt ss nameOrRef
      schemes' = hoistUpdate $ map (subst_attr_scheme ss) schemes
  in assembleIfUpdated [Upd nameOrRef', Upd schemes'] aspec $
       AttributeGroup (resultOnly nameOrRef') (resultOnly schemes') ifDoc

unique_internals_attr_scheme ::
  AttributeScheme -> XSDQ (MaybeUpdated AttributeScheme)
unique_internals_attr_scheme attrsc@(SingleAttribute nameOrRef ifType
                                                     mode ifDoc) = do
  whenDebugging $ dbgLn $ "unique_internals for attr " ++ show nameOrRef
  ifType' <- indenting $ unique_internals_QNameOr ifType
  return $ assembleIfUpdated [Upd ifType'] attrsc $
    SingleAttribute nameOrRef (resultOnly ifType') mode ifDoc
unique_internals_attr_scheme ag@(AttributeGroup nameOrRef attrDefs
                                                       ifDoc) = do
  whenDebugging $ dbgLn $ "unique_internals for attr group " ++ show nameOrRef
  attrDefs' <- indenting $ fmap hoistUpdate $
    mapM unique_internals_attr_scheme attrDefs
  return $ assembleIfUpdated [Upd attrDefs'] ag $
    AttributeGroup nameOrRef (resultOnly attrDefs') ifDoc

bound_names_attrsch :: AttributeScheme -> [String]
bound_names_attrsch (SingleAttribute nameOrRef _ _ _) =
  nameonly_to_stringlist nameOrRef
bound_names_attrsch (AttributeGroup nameOrRef attrSchs _) =
  nameonly_to_stringlist nameOrRef ++ concat (map bound_names_attrsch attrSchs)


-- | Main representation of possibly-nested XSD definitions.
data DataScheme =
  Skip
  | ElementScheme (Maybe DataScheme) -- ^ contents
                  (Maybe QName) -- ^ ifName
                  (Maybe QName) -- ^ ifType
                  (Maybe QName) -- ^ ifRef
                  (Maybe String) -- ^ ifId
                  String -- ^ `String` name of implementing class
                  (Maybe Int) -- ^ ifMin
                  (Maybe Int) -- ^ ifMax
                  Bool -- ^ isAbstract
                  (Maybe Line) -- ^ ifLine
                  (Maybe String) -- ^ ifDocumentation
  | AttributeScheme AttributeScheme -- ^ Single vs. group
                    -- String -- ^ `String` name of implementing class
                    (Maybe Line) -- ^ ifLine
                    (Maybe String) -- ^ ifDocumentation
  | CTS ComplexTypeScheme -- ^ typeDetail
                      [AttributeScheme] -- ^ addlAttrs
                      -- TODO --- Go back and populate this field
                      (Maybe QName) -- ^ ifName
                      -- String -- ^ `String` name of implementing class
                      (Maybe Line) -- ^ ifLine
                      (Maybe String) -- ^ ifDocumentation
  | STS (Maybe QName) -- ^ ifName
                     -- String -- ^ `String` name of implementing class
                     SimpleTypeScheme -- ^ Details
                     (Maybe Line) -- ^ ifLine
                     (Maybe String) -- ^ ifDocumentation
  | GroupScheme NameOrRefOpt -- ^ name or reference, or possibly neither
                -- String -- ^ `String` name of implementing class
                (Maybe ComplexTypeScheme) -- ^ contents
                -- TODO String -- ^ Implementation type name
                (Maybe Line) -- ^ ifLine
                (Maybe String) -- ^ ifDocumentation
  | ChoiceScheme NameOrRefOpt -- ^ name or reference, or possibly neither
                 -- String -- ^ `String` name of implementing class
                 (Maybe ComplexTypeScheme) -- ^ contents
                 -- TODO String -- ^ Implementation type name
                 (Maybe Line) -- ^ ifLine
                 (Maybe String) -- ^ ifDocumentation
  | UnprocessedXML (Maybe QName) -- ^ name
                   (Maybe Line) -- ^ ifLine
                   (Maybe String) -- ^ ifDocumentation
  deriving Show

{-
Skip ->
(ElementScheme ctnts ifName ifType ifRef ifId impl ifMin ifMax isAbst _ _) ->
(AttributeScheme (SingleAttribute nameOrRef ifType mode ifDoc) _ _) ->
(AttributeScheme (AttributeGroup nameOrRef attrDefs _) _ _) ->
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
labelOf (AttributeScheme (SingleAttribute (WithName n) _ _ _) _ _) = Just n
labelOf (AttributeScheme (SingleAttribute _ (NameRef qn) _ _) _ _) = Just qn
labelOf (AttributeScheme (SingleAttribute _ (Nested d) _ _) _ _) = labelOf d
labelOf (AttributeScheme (SingleAttribute (WithRef r) _ _ _) _ _) = Just r
labelOf (AttributeScheme (AttributeGroup (WithName n) _ _) _ _) = Just n
labelOf (AttributeScheme (AttributeGroup (WithRef r) _ _) _ _) = Just r
labelOf (AttributeScheme _ _ _) = Nothing
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
  block (ElementScheme ctnts ifName ifType ifRef ifId impl
                       ifMin ifMax isAbstract _ifLine ifDoc) =
    stackBlocks [
      stringToBlock ("ElementScheme name="
                       ++ maybe "undef" quoteShowQName ifName
                       ++ " as " ++ impl
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

  block (AttributeScheme s _ _) = labelBlock "AttributeScheme " $ block s

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

instance Blockable NameOrRefOpt where
  block (WithName n) = labelBlock "name=" $ block n
  block (WithRef r)  = labelBlock "ref="  $ block r
  block (WithNeither) = stringToBlock "(neither)"

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

instance Blockable QNameOr where
  block (NameRef qn) = block qn
  block (Nested ds) = block ds
  block (Neither) = stringToBlock "(neither)"

instance Blockable [DataScheme] where block = verticalBlockListFn
instance Blockable [AttributeScheme] where block = verticalBlockListFn

newtype PrimaryBundle = PB (Content, String, Maybe String, Maybe String, QName,
                            [Attr], [Content], Maybe Line)

nameonly_to_stringlist :: NameOrRefOpt -> [String]
nameonly_to_stringlist (WithName qn) = [qName qn]
nameonly_to_stringlist _ = []

maybeqname_to_stringlist :: Maybe QName -> [String]
maybeqname_to_stringlist (Just qn) = [qName qn]
maybeqname_to_stringlist _ = []

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
    (ElementScheme _ _ _ _ _ impl _ _ _ _ _) -> [impl]
    (AttributeScheme spec _ _) -> bound_names_attrsch spec
    (CTS _ _ ifName _ _) -> maybeqname_to_stringlist ifName
    (STS ifName _ _ _) -> maybeqname_to_stringlist ifName
    (GroupScheme nameOrRef _ _ _) -> nameonly_to_stringlist nameOrRef
    (ChoiceScheme nameOrRef _ _ _) -> nameonly_to_stringlist nameOrRef
    (UnprocessedXML ifName _ _) -> maybeqname_to_stringlist ifName

  -- | Rename any nonunique hidden names within the scope of the given
  -- @ast@, but not defined at top-level.  Used by
  -- `QDHXB.Internal.AST.ensureUniqueNames`.  This is a case over the
  -- structure of the @ast@ type, and applying `ensureUniqueNames` or
  -- `ensureUniqueNames1` to recursively-held lists of ASTs.
  ensureUniqueInternalNames dss@Skip = return $ Same dss
  ensureUniqueInternalNames dss@((ElementScheme ifDS ifName ifType ifRef ifId
                                                impl ifMin ifMax isAbstract
                                                ifLn ifDoc)) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug dss
    ifDS' <- maybe (return $ Same ifDS)
                   (fmap (fmap Just) . ensureUniqueNames1) ifDS
    return $ assembleIfUpdated [Upd ifDS'] dss $
               ElementScheme (resultOnly ifDS') ifName ifType ifRef ifId
                             impl ifMin ifMax isAbstract ifLn ifDoc
  ensureUniqueInternalNames ats@(AttributeScheme scheme ifLn ifDoc) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug ats
    scheme' <- indenting $ unique_internals_attr_scheme scheme
    return $ assembleIfUpdated [Upd scheme'] ats $
      AttributeScheme (resultOnly scheme') ifLn ifDoc
  ensureUniqueInternalNames dss@(CTS form attrs ifName
                                                   ifLn ifDoc) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug dss
    form' <- indenting $ unique_internals_cts form
    attrs' <- indenting $
      fmap hoistUpdate $ mapM unique_internals_attr_scheme attrs
    return $ assembleIfUpdated [Upd form', Upd attrs'] dss $
      CTS (resultOnly form') (resultOnly attrs') ifName ifLn ifDoc
  ensureUniqueInternalNames dss@(STS name sts ifLn ifDoc) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug dss
    sts' <- indenting $ unique_internals_sts sts
    return $ assembleIfUpdated [Upd sts'] dss $
      STS name (resultOnly sts') ifLn ifDoc
  ensureUniqueInternalNames dss@(GroupScheme nameOrRef ifCts ifLn ifDoc) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug dss
    cts' <- case ifCts of
              Just cts -> do
                cts'' <- indenting $ unique_internals_cts cts
                return $ fmap Just cts''
              Nothing -> return $ Same ifCts
    return $ assembleIfUpdated [Upd cts'] dss $
      GroupScheme nameOrRef (resultOnly cts') ifLn ifDoc
  ensureUniqueInternalNames dss@(ChoiceScheme nameOrRef ifCts ifLn ifDoc) = do
    whenDebugging $ dbgLn $ "ensureUniqueInternalNames for " ++ debugSlug dss
    cts' <- case ifCts of
              Just cts -> do
                cts'' <- indenting $ unique_internals_cts cts
                return $ fmap Just cts''
              Nothing -> return $ Same ifCts
    return $ assembleIfUpdated [Upd cts'] dss $
      ChoiceScheme nameOrRef (resultOnly cts') ifLn ifDoc
  ensureUniqueInternalNames dss@(UnprocessedXML _ _ _) = return $ Same dss

  -- | Apply the given substitutions to the given AST.
  applySubstitutionsTo substs ast = case ast of
    Skip -> Same ast
    (ElementScheme ifCtnt ifName ifType ifRef ifId
                   impl ifMin ifMax isAbstract ifLn ifDoc) ->
      let ifCtnt' = hoistUpdate $ fmap (applySubstitutionsTo substs) ifCtnt
          impl' = substString substs impl
          sq = substQName substs
          ifType' = hoistUpdate $ fmap sq ifType
          ifRef' = hoistUpdate $ fmap sq ifRef
      in assembleIfUpdated [Upd ifCtnt', Upd impl', Upd ifType', Upd ifRef']
                           ast $
           ElementScheme (resultOnly ifCtnt') ifName (resultOnly ifType')
                         (resultOnly ifRef') ifId (resultOnly impl')
                         ifMin ifMax isAbstract ifLn ifDoc
    (AttributeScheme aspec ifLn ifDoc) ->
      let aspec' = subst_attr_scheme substs aspec
      in assembleIfUpdated [Upd aspec'] ast $
           AttributeScheme (resultOnly aspec') ifLn ifDoc
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

  -- | Converting `DataScheme`s to `Definition`s.
  flatten = fmap concat . mapM flattenSchemaItem . filter nonSkip
    where

      flattenSchemaItem :: DataScheme -> XSDQ [Definition]
      {-# INLINE flattenSchemaItem #-}
      flattenSchemaItem s = do
        whenDebugging $ dbgBLabel "[fSI] from " s
        results <- indenting $ flattenSchemaItem' s
        dbgResult "Result [fSI]:" results

      flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
      flattenSchemaItem' Skip = return []

      flattenSchemaItem' (ElementScheme contents ifName ifType ifRef _ifId
                                        _impl ifMin ifMax abst l ifDoc) = do
        whenDebugging $ dbgLn "[fSI'] Relaying to flattenElementSchemeItem"
        flattenElementSchemeItem contents ifName ifType ifRef ifMin ifMax
                                 abst l ifDoc

      flattenSchemaItem' (AttributeScheme
                          (SingleAttribute (WithName nam) (NameRef typ) m d')
                          l d) = do
        whenDebugging $ dbgLn "[fSI'] Flattening single attribute"
        let attrDefn =
              AttributeDefn nam
                (SingleAttributeDefn typ $ stringToAttributeUsage m)
                l (pickOrCombine d d')
        fileNewDefinition attrDefn
        dbgResult "Flattened [fSI'] to" [attrDefn]

      flattenSchemaItem' (AttributeScheme (SingleAttribute (WithRef _) _ _ _)
                                          _l _d) = do
        return $ error "[fSI'] Reference in attribute"

      flattenSchemaItem' (AttributeScheme
                          (SingleAttribute (WithName nam) (Nested ds)
                                           use innerDoc)
                          l outerDoc) = do
        whenDebugging $ dbgLn "[fSI'] attribute with nested type"
        (defs, ref) <- flattenSchemaRef ds
        let qn = referenceQName ref
        let attrDefn =
              AttributeDefn nam
                (SingleAttributeDefn qn $ stringToAttributeUsage use)
                l (pickOrCombine innerDoc outerDoc)
        fileNewDefinition attrDefn
        dbgResult "Flattened [fSI'] to" $ defs ++ [attrDefn]

      flattenSchemaItem' (AttributeScheme (AttributeGroup nr cs _) l d) = do
        whenDebugging $
          dbgLn "[fSI'] Relaying to flattenAttributeGroupItem for "
        flattenAttributeGroupItem nr cs l d

      flattenSchemaItem' (CTS cts ats ifNam l d) = do
        whenDebugging $ dbgLn "[fSI'] Relaying to flattenComplexTypeScheme"
        flattenComplexTypeScheme cts ats ifNam l d

      flattenSchemaItem' s@(STS (Just n) (Synonym base) ln d) = do
        whenDebugging $ dbgBLabel "[fSI'] Flattening simple synonym " s
        indenting $ do
          let tyDefn = SimpleSynonymDefn n base ln d
          fileNewDefinition tyDefn
          dbgResult "Flattened [fSI'] to" [tyDefn]

      -- TODO Insert cases of SimpleRestriction that we /can/ handle in the
      -- types here

      flattenSchemaItem' sts@(STS (Just nam)
                                               (SimpleRestriction base)
                                               ln d) = do
        whenDebugging $ dbgBLabel "[fSI'] Flattening simple restriction " sts
        indenting $ do
          let tyDefn = SimpleSynonymDefn nam base ln d
          addTypeDefn nam tyDefn
          dbgResult "Flattened [fSI'] to" $ [ tyDefn ]

      flattenSchemaItem' sts@(STS (Just nam) (Union alts ns)
                                               ln d) = do
        whenDebugging $ dbgBLabel "[fSI'] Flattening simple union " sts
        let nameUnnamed :: QName -> DataScheme -> DataScheme
            nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifId
                                         impl ifMin ifMax isAbstract l ifDoc) =
              ElementScheme ctnts (Just q) ifType ifRef ifId
                            impl ifMin ifMax isAbstract l ifDoc
            nameUnnamed q (AttributeScheme
                           (SingleAttribute (WithRef _) ifType usage d')
                           ln' d'') =
              AttributeScheme (SingleAttribute (WithName q) ifType usage d') ln'
                (pickOrCombine d d'')
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
        -- whenDebugging $ dbgBLabel "- labelledAlts " labelledAlts
        let (names, defnss) = unzip labelledAlts
            defns = concat defnss
        whenDebugging $ do
          dbgBLabel "- names " names
          -- dbgBLabel "- defnss " defnss
        let fromMemberList = map pullRefLabel ns
        whenDebugging $ dbgBLabel "- fromMemberList " fromMemberList
        let uDef = UnionDefn nam (names ++ fromMemberList) ln d
        whenDebugging $ dbgBLabel "- uDef " uDef
        fileNewDefinition uDef
        dbgResult "Flattened [fSI'] to" $ defns ++ [uDef]


      flattenSchemaItem' s@(STS (Just nam)
                             (List (Just elemTyp) Nothing)
                             ln d) = do
        whenDebugging $
          dbgBLabel
            "[fSI'] Flattening simple list with referenced element type " s
        indenting $ do
          let lDef = ListDefn nam elemTyp ln d
          fileNewDefinition lDef
          dbgResult "Flattened [fSI'] to" [lDef]

      flattenSchemaItem' s@(STS (Just nam)
                                             (List Nothing (Just inlineTyp))
                                             ln d) = do
        whenDebugging $
          dbgBLabel "[fSI'] Flattening simple list with inline element " s
        indenting $ do
          (subdefs, subref) <- flattenSchemaRef inlineTyp
          let lDef = ListDefn nam (referenceQName subref) ln d
          fileNewDefinition lDef
          dbgResult "Flattened [fSI'] to" $ subdefs ++ [lDef]

      flattenSchemaItem' gs@(GroupScheme (WithName name) (Just cts) ln doc) = do
        whenDebugging $ dbgBLabel
          "[fSI'] Flattening group scheme with name and present content " gs
        -- let typeSchemeName = withSuffix "GroupContent" name
        defs <- flattenComplexTypeScheme cts []
                  (Just name {- typeSchemeName -} )
                  ln doc

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

        dbgResult "Flattened [fSI'] to" $ defs -- ++ [defn]

      flattenSchemaItem' gs@(GroupScheme (WithRef ref) Nothing ln _doc) = do
        whenDebugging $ dbgBLabel
          "[fSI'] Flattening group scheme with reference and no content " gs
        boxed $ do
          dbgLn "TODO flattenSchemaItem' group with ref, no contents"
          dbgLn $ "REF " ++ qName ref
          dbgLn $ "LN " ++ show ln
        error $ "TODO flatten group with ref "
          ++ qName ref
          ++ ", no contents, at "
          ++ maybe "(no XSD line num)" show ln

      flattenSchemaItem' s = do
        whenDebugging $ dbgLn "[fSI'] missed case"
        boxed $ do
          dbgLn "TODO flattenSchemaItem' missed case"
          dbgBLabel "ARG " s
        error $ show $ labelBlock "TODO another flatten case: " $ block s

      flattenAttributeGroupItem ::
        NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
        -> XSDQ [Definition]
      flattenAttributeGroupItem nro cs l d = case nro of
        WithName name -> flattenWithName name
        WithRef name -> flattenWithName name
        _ -> boxed $ do
          dbgLn "TODO [fAGI] missed case"
          dbgBLabel "NAMEREF " nro
          dbgBLabel "CONTENTS " cs
          dbgLn $ "LN " ++ show l
          error "TODO flattenAttributeGroupItem unmatched"
        where flattenWithName n = do
                whenDebugging $ dbgLn "[fAGI] Flattening attribute group item"
                let names = map grabNameAndUsage cs
                defs <- indenting $ flattenAttributes cs
                let attrDefn = AttributeDefn n (AttributeGroupDefn names) l d
                fileNewDefinition attrDefn
                let res = defs ++ [attrDefn]
                return res


      flattenComplexTypeScheme ::
        ComplexTypeScheme -> [AttributeScheme] -> Maybe QName
        -> Maybe Line -> Maybe String
        -> XSDQ [Definition]

      flattenComplexTypeScheme c@(Composing cts ats0) ats (Just nam) l d = do
        whenDebugging $ dbgBLabel ("[fCTS] Complex composition at " ++ show l) c
        (defs, refs) <- indenting $
          musterComplexSequenceComponents (filter nonSkip cts) (ats0 ++ ats) nam
            -- TODO DOC possible to add a docstring here?

        let tyDefn = SequenceDefn nam refs l d
        fileNewDefinition tyDefn
        -- whenDebugging $ do
        --   recheck <- isKnownType nam
        --   dbgLn $ "- Have set " ++ qName nam
        --            ++ " to be known; rechecked as " ++ show recheck
        dbgResult "Flattened [fCTS] to" $ defs ++ [ tyDefn ]

      flattenComplexTypeScheme c@(ComplexRestriction base) _ (Just nam) l d = do
        whenDebugging $ dbgBLabel "[fCTS] Complex restriction " c
        let defn = ComplexSynonymDefn nam base l d
        fileNewDefinition defn
        dbgResult "Flattened [fCTS] to" $ [defn]

      flattenComplexTypeScheme e@(Extension base ds) _ats (Just nam) l d = do
        whenDebugging $ dbgBLabel "[fCTS] Complex extension " e
        (defs, refs) <- indenting $ flattenSchemaRefs ds
        let defn =
              ExtensionDefn nam (TypeRef base (Just 1) (Just 1) l d) refs l d
        fileNewDefinition defn
        dbgResult "Flattened [fCTS] to" $ defs ++ [defn]

      flattenComplexTypeScheme c@(Choice ifName contents) _ ifOuter ln doc = do
        whenDebugging $ do
          dbgBLabel "[fCTS] Choice " c
          dbgLn $ "- Line " ++ show ln
          -- dbgBLabel "- contents " contents
        let name = maybe (maybe (QName "???" Nothing Nothing) id ifOuter)
                         id ifName
        (defs, refs) <- flattenSchemaRefs contents
        let labelledRefs = zipWith getLabelledDisjunct refs contents
            defn = ChoiceDefn name labelledRefs ln doc
        addElementType name name
        fileNewDefinition defn
        dbgResult "Flattened [fCTS] to" $
          defs ++ [defn]

      flattenComplexTypeScheme (Group (WithName n) (Just ctnt) ifMin ifMax)
                               ats ifName l d = do
        (flatCtnt, ctntRef) <- flattenSchemaRef ctnt
        whenDebugging $ boxed $ do
          dbgLn "TODO [fCTS] Group/WithName case"
          dbgLn "Group:"
          dbgBLabel ". WithName N " n
          dbgBLabel ". CTNT " $ ctnt
          dbgBLabel ". IFMIN " ifMin
          dbgBLabel ". IFMAX " ifMax
          dbgBLabel "ATS " ats
          dbgBLabel "IFNAME " ifName
          dbgLn $ "L " ++ show l
          dbgBLabel "FLATCTNT " $ flatCtnt
        let defn = GroupDefn n ctntRef l d
        fileNewDefinition defn
        dbgResult "Flattened [fCTS] to" $ flatCtnt ++ [defn]

      flattenComplexTypeScheme (Group (WithRef r) Nothing ifMin ifMax)
                               ats (Just name) l d = do
        whenDebugging $ dbgLn "[fCTS] Group/WithRef case"
        (defs, refs) <- indenting $ musterAttributesForComplexSequence [] [
          GroupRef r ifMin ifMax l d
          ] ats
        let tyDefn = SequenceDefn name refs l d
        fileNewDefinition tyDefn
        dbgResult "Flattened [fCTS] to" $ defs ++ [tyDefn]

      flattenComplexTypeScheme (Group WithNeither (Just ctnt) ifMin ifMax)
                               ats (Just name) l d = do
        whenDebugging $ dbgLn "[fCTS] Group/WithNeither case"
        (flatCtnt, ctntRef) <- flattenSchemaRef ctnt
        boxed $ do
          dbgLn "TODO flattenComplexTypeScheme Group case"
          dbgLn "Group:"
          dbgBLabel ". CTNT " $ ctnt
          dbgBLabel ". IFMIN " ifMin
          dbgBLabel ". IFMAX " ifMax
          dbgBLabel "ATS " ats
          dbgBLabel "NAME " name
          dbgLn $ "L " ++ show l
          dbgBLabel "FLATCTNT " $ flatCtnt
        let defn = GroupDefn name ctntRef l d
        fileNewDefinition defn
        dbgResult "Flattened [fCTS] to" $ flatCtnt ++ [defn]

      flattenComplexTypeScheme cts ats ifName ln _ = do
        boxed $ do
          dbgLn "TODO [fCTS] flattenComplexTypeScheme missed case"
          dbgBLabel "CTS " cts
          dbgBLabel "ATS " ats
          dbgBLabel "IFNAME " ifName
          dbgLn $ "LN " ++ show ln
        error "TODO flattenComplexTypeScheme missed case"

      getLabelledDisjunct :: Reference -> DataScheme -> (QName, Reference)
      getLabelledDisjunct ref ds = (maybe (referenceBase ref) id $ labelOf ds,
                                    ref)

      flattenElementSchemeItem ::
        Maybe DataScheme -> Maybe QName -> Maybe QName -> Maybe QName
        -> Maybe Int -> Maybe Int -> Bool -> Maybe Line -> Maybe String
        -> XSDQ [Definition]
      flattenElementSchemeItem Nothing (Just nam) (Just typ) Nothing _ _ _
                               ln ifDoc = do
        whenDebugging $ dbgLn "[fESI] With name/type"
        indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
      flattenElementSchemeItem (Just Skip) (Just nam) (Just typ) _ _ _ _
                               ln ifDoc = do
        whenDebugging $ dbgLn "[fESI] Enclosing skip"
        indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
      flattenElementSchemeItem (Just (STS _ ts ln d))
                               ifName@(Just nam) Nothing Nothing _ _ _ _
                               ifDoc = do
        whenDebugging $ dbgLn "[fESI] Enclosing simple type scheme"
        flatTS <- flattenSchemaItem' $ STS ifName ts ln d
        let elemDefn = ElementDefn nam nam ln ifDoc
        fileNewDefinition elemDefn
        dbgResult "Flattened [fESI] to " $ flatTS ++ [elemDefn]
      flattenElementSchemeItem (Just (CTS ts attrs ifCTSName l d))
                               ifElementName@(Just nam) Nothing Nothing _ _
                               _ _ ifDoc = do
        whenDebugging $ dbgLn "[fESI] Enclosing complex type scheme"
        let typeName = maybe nam id ifCTSName
        let ifTypeName = maybe ifElementName Just ifCTSName
        whenDebugging $
          dbgLn "Flattening element scheme enclosing complex type scheme"
        flatTS <- flattenSchemaItem' $ CTS ts attrs ifTypeName l d
        let elemDefn = ElementDefn nam typeName l ifDoc
        fileNewDefinition elemDefn
        dbgResult "Flattened [fESI] to " $ flatTS ++ [elemDefn]
      flattenElementSchemeItem Nothing ifName@(Just _)
                               Nothing Nothing ifMax ifMin
                               True ifLine ifDoc = do
        whenDebugging $ dbgLn
          "[fESI] Abstract with name but no contents/type --- relay with any"
        anyType <- anyTypeQName
        flattenElementSchemeItem Nothing ifName (Just anyType) Nothing
                                 ifMax ifMin True ifLine ifDoc

      flattenElementSchemeItem content ifName ifType ifRef ifMin ifMax
                               _ _ _ifDoc = do
        boxed $ do
          dbgLn "[fESI] flattenElementSchemeItem"
          dbgBLabel "CONTENT " content
          dbgBLabel "IFNAME " ifName
          dbgBLabel "IFTYPE " ifType
          dbgBLabel "IFREF " ifRef
          dbgLn $ "IFMIN " ++ show ifMin
          dbgLn $ "IFMAX " ++ show ifMax
        error "Unmatched case for flattenElementSchemeItem"

      flattenWithNameTypeOnly ::
        QName -> QName -> Maybe Line -> Maybe String -> XSDQ [Definition]
      flattenWithNameTypeOnly nam typ ln ifDoc = do
        whenDebugging $ dbgLn "flattenWithNameTypeOnly"
        let elemDefn = ElementDefn nam typ ln ifDoc
        fileNewDefinition elemDefn
        dbgResult "Flattened [fWNTO] to " [ elemDefn ]


      musterComplexSequenceComponents ::
        [DataScheme] ->  [AttributeScheme] -> QName
        -> XSDQ ([Definition], [Reference])
      musterComplexSequenceComponents steps ats _ = do
        whenDebugging $ do
          dbgLn "musterComplexSequenceComponents"
          dbgBLabel "- STEPS " steps
          dbgBLabel "- ATS " ats
        (otherDefs, refs) <- indenting $ flattenSchemaRefs steps
        whenDebugging $ do
          dbgBLabel "- OTHERDEFS " otherDefs
          dbgBLabel "- REFS " refs
          dbgLn "Relaying to musterAttributesForComplexSequence"
        musterAttributesForComplexSequence otherDefs refs ats

      musterAttributesForComplexSequence ::
        [Definition] -> [Reference] ->  [AttributeScheme]
        -> XSDQ ([Definition], [Reference])
      musterAttributesForComplexSequence defs refs ats = do
        whenDebugging $ do
          dbgLn "musterAttributesForComplexSequence"
          dbgBLabel "- DEFS " defs
          dbgBLabel "- REFS " refs
          dbgBLabel "- ATS " ats
        (atsDefs, atsRefs) <- indenting $ flattenSchemaAttributeRefs ats
        dbgResult "Result [mAFCS]:" $
          (defs ++ atsDefs, atsRefs ++ refs)

      grabNameAndUsage :: AttributeScheme -> (QName, AttributeUsage)
      grabNameAndUsage (SingleAttribute (WithName n) _ useStr _) =
        (n, stringToAttributeUsage useStr)
      grabNameAndUsage (SingleAttribute (WithRef n) _ useStr _) =
        (n, stringToAttributeUsage useStr)
      grabNameAndUsage (SingleAttribute WithNeither (NameRef t) useStr _) =
        (t, stringToAttributeUsage useStr)
      grabNameAndUsage (AttributeGroup (WithName n) _ _) = (n, Optional)
      grabNameAndUsage (AttributeGroup (WithRef n) _ _) = (n, Optional)
      grabNameAndUsage a = error $ "No useable name in " ++ show a

      flattenSchemaAttributeRefs ::
        [AttributeScheme] -> XSDQ ([Definition], [Reference])
      flattenSchemaAttributeRefs ass = do
        whenDebugging $
          dbgLn "[flattenSchemaAttributeRefs] processing each by [fSchAR]"
        defsRefs <- indenting $ mapM flattenSchemaAttributeRef ass
        whenDebugging $ dbgLn "returned from (mapM flattenSchemaAttributeRef)"
        dbgResult "Result [fSAR]:" $
          applyFst concat $ unzip defsRefs

      flattenSchemaAttributeRef ::
        AttributeScheme -> XSDQ ([Definition], Reference)
      flattenSchemaAttributeRef r@(SingleAttribute nr t m d) = do
        whenDebugging $ dbgBLabel "[fSchAR -> fSngAR] " r
        flattenSingleAttributeRef nr t m Nothing d
      flattenSchemaAttributeRef r@(AttributeGroup nameRef cs d) = do
        whenDebugging $ dbgBLabel "[fSchAR -> fAGR] " r
        flattenAttributeGroupRef nameRef cs Nothing d

      flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
      flattenSchemaRefs =
        fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

      flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
      flattenSchemaRef (ElementScheme c ifName ifType ifRef _ifId _impl
                                      ifLower ifUpper _isAbstract ln ifDoc) = do
        whenDebugging $ dbgLn "[fSR -> flattenElementSchemeRef]"
        flattenElementSchemeRef c ifName ifType ifRef ifLower ifUpper ln ifDoc
      flattenSchemaRef (AttributeScheme (SingleAttribute nr t m d') l d) = do
        whenDebugging $ dbgLn "[fSR -> flattenSingleAttributeRef]"
        flattenSingleAttributeRef nr t m l (pickOrCombine d d')
      flattenSchemaRef (AttributeScheme (AttributeGroup nameRef cs _) l d) = do
        whenDebugging $ dbgLn "[fSR -> flattenAttributeGroupRef]"
        flattenAttributeGroupRef nameRef cs l d
      flattenSchemaRef c@(CTS _ _ (Just n) ifLine ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] CTS " c
        defns <- indenting $ flattenSchemaItem c
        dbgResult "Flattened [fSR.CTS] to" $
          (defns, TypeRef n (Just 1) (Just 1) ifLine ifDoc)
      flattenSchemaRef s@(STS (Just n) _ ifLine ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] STS " s
        defns <- indenting $ flattenSchemaItem s
        dbgResult "Flattened [fSR.STS] to" $
          (defns, TypeRef n (Just 1) (Just 1) ifLine ifDoc)

      flattenSchemaRef gs@(GroupScheme (WithRef ref) _ifCtnts ifLn ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] GS-WR " gs
        dbgResult "Flattened [fSR.GS-WR] to" $
          ([], GroupRef ref (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef gs@(GroupScheme (WithName name) (Just sub)
                                       ifLn ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] GS-WN " gs
        defns <- indenting $
          flattenComplexTypeScheme sub [] (Just name) ifLn ifDoc
        dbgResult "Flattened [fSR.GS-WN] to" $
          (defns, GroupRef name (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef (GroupScheme WithNeither (Just cts) ifLn _ifDoc) = do
        boxed $ do
          dbgLn "[fSR] GroupScheme"
          dbgBLabel "CTS " cts
          dbgLn $ "IFLN " ++ maybe "(none)" show ifLn
        error $ "TODO flattenSchemaRef > GroupScheme with no name/reference"

      flattenSchemaRef gs@(ChoiceScheme (WithRef ref) _ifCtnts ifLn ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] CS-WR " gs
        dbgResult
          "Flattened [fSR.CS-WR, just converting to type reference] to" $
            ([], TypeRef ref (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef gs@(ChoiceScheme (WithName name) (Just sub)
                                        ifLn ifDoc) = do
        whenDebugging $ dbgBLabel "[fSR] CS-WN " gs
        defns <- indenting $
          flattenComplexTypeScheme sub [] (Just name) ifLn ifDoc
        dbgResult "Flattened [fSR.GS-WN] to" $
          (defns, TypeRef name (Just 1) (Just 1) ifLn ifDoc)
      flattenSchemaRef (ChoiceScheme WithNeither (Just cts) ifLn _ifDoc) = do
        boxed $ do
          dbgLn "[fSR] ChoiceScheme"
          dbgBLabel "CTS " cts
          dbgLn $ "IFLN " ++ maybe "(none)" show ifLn
        error $ "TODO flattenSchemaRef > ChoiceScheme with no name/reference"

      flattenSchemaRef (UnprocessedXML _ ifLn ifDoc) = do
        dbgResult "Flattened [fSR.UNPROC] to" ([], RawXML ifLn ifDoc)

      flattenSchemaRef s = do
        boxed $ do
          dbgLn "[fSR] flattenSchemaRef"
          dbgBLabel "arg " s
        error $ "TODO flattenSchemaRef > additional case:"

      flattenAttributeGroupRef ::
        NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      flattenAttributeGroupRef n@(WithName name) contents l d = do
        whenDebugging $ dbgLn "[fAGR] WithName "
        refs <- indenting $ flattenAttributeGroupItem n contents l d
        dbgResult (showQName name ++ " [fAGR] flattened to") $
          (refs, AttributeRef name Optional)
      flattenAttributeGroupRef (WithRef ref) [] _ln _d = do
        whenDebugging $ dbgLn "[fAGR] WithRef "
        dbgResult (showQName ref ++ " [fAGR] flattened to") $
          ([], AttributeRef ref Optional)
      flattenAttributeGroupRef nameRef contents _ln _d = do
        boxed $ do
          dbgLn "flattenAttributeGroupRef"
          dbgBLabel "NAMEREF " nameRef
          dbgBLabel "CONTENTS " contents
        error $ "TODO flattenAttributeGroupRef > unmatched"


      flattenSingleAttributeRef ::
        NameOrRefOpt -> QNameOr -> String -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      flattenSingleAttributeRef (WithRef ref) Neither useStr _ _ = do
        whenDebugging $ dbgLn "[fSAR] WithRef+Neither "
        let res = AttributeRef ref (stringToAttributeUsage useStr)
        dbgResult (showQName ref ++ " [fSAR] flattened to") ([], res)
      flattenSingleAttributeRef (WithName nam) (NameRef t) m l d = do
        whenDebugging $ dbgLn "[fSAR] WithRef+NameRef "
        let defn = AttributeDefn nam
                     (SingleAttributeDefn t $ stringToAttributeUsage m) l d
            ref = AttributeRef nam (stringToAttributeUsage m)
        fileNewDefinition defn
        dbgResult (showQName nam ++ " [fSAR] flattened to") ([defn], ref)
      flattenSingleAttributeRef (WithName nam) (Nested t) m _ _ = do
        boxed $ do
          dbgLn "[fSAR] new nested case"
          dbgBLabel "NAM " nam
          dbgBLabel "T " t
          dbgLn $ "MODE " ++ m
        error "TODO new Nested case"
      flattenSingleAttributeRef nameRef maybeType mode _ _ = do
        boxed $ do
          dbgLn "[fSAR] flattenSingleAttributeRef"
          dbgBLabel "NAMEREF " nameRef
          dbgBLabel "IFTYPE " maybeType
          dbgLn $ "MODE " ++ mode
        error "TODO flattenSingleAttributeRef > unmatched case"


      flattenElementSchemeRef ::
        Maybe DataScheme -> Maybe QName -> Maybe QName -> Maybe QName
        -> Maybe Int -> Maybe Int -> Maybe Line -> Maybe String
        -> XSDQ ([Definition], Reference)
      -- flattenElementSchemeRef contents ifName ifType ifRef ifLower ifUpper =
      flattenElementSchemeRef Nothing Nothing Nothing (Just r) lower upper
                              ln _ifDoc = do
        whenDebugging $ dbgLn "[fESR.1] all Nothing"
        let result = ElementRef r lower upper ln
        whenDebugging $ do
          dbgLn $ "Flattening element schema with reference only"
          dbgBLabel "  to " result
        dbgResult ("Ref to " ++ showQName r ++ " flattened [fESR.2] to")
          ([], result)
      flattenElementSchemeRef Nothing (Just n)
                              (Just t@(QName resolvedName _resolvedURI _))
                              Nothing lo up ln ifDoc = do
        whenDebugging $ dbgLn "[fESR.3] first Nothing"
        isKnown <- isKnownType t
        whenDebugging $ dbgLn $
          "- Checking whether " ++ resolvedName ++ " is known: " ++ show isKnown
        if isKnown then (do let defn = ElementDefn n t ln ifDoc
                                ref = ElementRef n lo up ln
                            fileNewDefinition defn
                            whenDebugging $ do
                              dbgLn "Flattening schema with type"
                              -- dbgBLabel "       " e
                              dbgBLabel "    to " defn
                              dbgBLabel "       " ref
                            dbgResult
                              ("Name ref " ++ showQName n
                                ++ " flattened [fESR.4] to")
                              ([defn], ref))
          else (do let defn1 = SimpleSynonymDefn n t ln ifDoc
                       defn2 = ElementDefn n n ln ifDoc
                       ref = ElementRef n lo up ln
                   addTypeDefn n defn1
                   fileNewDefinition defn2
                   whenDebugging $ do
                     dbgLn "  > Flattening element schema with name and type"
                     -- dbgBLabel "       " e
                     dbgBLabel "    to " defn1
                     dbgBLabel "       " defn2
                     dbgBLabel "       " ref
                   dbgResult
                     ("Ref to " ++ showQName n ++ " flattened [fESR.5] to")
                       ([defn1, defn2], ref))
      flattenElementSchemeRef s@(Just (CTS _ _ Nothing _ _))
                              n@(Just nam) t@Nothing r@Nothing lower upper
                              ln ifDoc = do
        whenDebugging $ dbgLn "[fESR.6] t and r and Nothing"
        prev <- flattenElementSchemeItem s n t r lower upper False ln ifDoc
        let ref = ElementRef nam lower upper ln
        whenDebugging $ do
          dbgLn "Flattening element schema with name and nested complex type"
          dbgBLabel "       " s
          dbgBLabel "    to " prev
          dbgBLabel "       " ref
        dbgResult "Flattened [fESR.7] to" (prev, ref)
      flattenElementSchemeRef s@(Just (CTS _ _ (Just schemeName)
                                                         _ _))
                              n@(Just nam) t@Nothing r@Nothing
                              lower upper ln ifDoc = do
        whenDebugging $ do
          dbgLn "[fESR.8] CTS name, scheme name, no t, no r"
          indenting $ do
            dbgBLabel "CONTENTS " s
            dbgLn $ "SCHEMANAME (inner name) " ++ show schemeName
            dbgLn $ "NAM " ++ show nam
            dbgLn $ "IFTYPE Nothing"
            dbgLn $ "IFREF  Nothing"
            dbgLn $ "LOWER " ++ show lower
            dbgLn $ "UPPER " ++ show upper
            dbgLn $ "LN " ++ show ln
        prev <- flattenElementSchemeItem s n t r lower upper False ln ifDoc
        whenDebugging $ dbgBLabel "- prev " prev
        let ref = ElementRef nam lower upper ln
        whenDebugging $ dbgBLabel "- ref " ref
        dbgResult "Flattened [fESR.9] to" (prev, ref)
      flattenElementSchemeRef ctnts maybeName maybeType maybeRef lower upper
                              _ _ = do
        boxed $ do
          whenDebugging $ dbgLn "[fESR.10] flattenElementSchemeRef"
          dbgBLabel "CONTENTS " ctnts
          dbgLn $ "IFNAME " ++ show maybeName
          dbgLn $ "IFTYPE " ++ show maybeType
          dbgLn $ "IFREF " ++ show maybeRef
          dbgLn $ "LOWER " ++ show lower
          dbgLn $ "UPPER " ++ show upper
        error "TODO flattenSchemaRef > unmatched ElementScheme"


      flattenAttributes :: [AttributeScheme] -> XSDQ [Definition]
      flattenAttributes = fmap concat . mapM flattenAttribute

      flattenAttribute :: AttributeScheme -> XSDQ [Definition]
      flattenAttribute (SingleAttribute (WithRef _) Neither _ _) = do
        whenDebugging $ do
          dbgLn "[fA] single attribute by ref"
          dbgLn "- Defined elsewhere --- returning []"
        return []
      flattenAttribute sa@(SingleAttribute (WithName n) (NameRef typ)
                                           mode d) = do
        whenDebugging $
          dbgBLabel "[fA] single attribute with type reference " sa
        indenting $ do
          let defn = AttributeDefn n (SingleAttributeDefn typ $
                                        stringToAttributeUsage mode)
                                   Nothing d
          fileNewDefinition defn
          dbgResult "Flattened [fA] to" $ [defn]
      flattenAttribute sa@(SingleAttribute (WithName n) (Nested ds) mode d) = do
        whenDebugging $
          dbgBLabel "[fA] Single attribute with nested type " sa
        (defs, ref) <- indenting $ flattenSchemaRef ds
        whenDebugging $ do
          dbgLn $ "(Back in flattenAttribute)"
          dbgBLabel "- defs " defs
          dbgBLabel "- ref " ref
        case ref of
          TypeRef qn (Just 1) (Just 1) _ _ -> do
            whenDebugging $
              dbgLn $ "Case for TypeRef " ++ showQName qn ++ ", min/max single"
            let defn = AttributeDefn n
                         (SingleAttributeDefn qn $ stringToAttributeUsage mode)
                         Nothing d
            fileNewDefinition defn
            dbgResult "Flattened [fA] to" $ defs ++ [defn]
          TypeRef qn Nothing Nothing _ _ -> do
            whenDebugging $
              dbgLn $ "Case for TypeRef " ++ showQName qn ++ ", no bounds"
            let defn = AttributeDefn n
                         (SingleAttributeDefn qn $ stringToAttributeUsage mode)
                         Nothing d
            fileNewDefinition defn
            dbgResult "Flattened [fA] to" $ defs ++ [defn]
          TypeRef qn mn (Just 1) _ _ -> do
            whenDebugging $ do
              dbgLn $
                "Case for TypeRef " ++ showQName qn ++ ", min bound "
                  ++ show mn ++ ", max bound 1"
              boxed $ do
                dbgLn "flattenAttribute nested single"
                dbgBLabel "N " n
                dbgBLabel "DS " ds
                dbgLn $ "MODE " ++ mode
                dbgLn $ "D " ++ show d
                dbgLn ""
                dbgLn "flattenAttribute nested single (II)"
                dbgBLabel "DEFS" defs
                dbgBLabel "REF" ref
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
        whenDebugging $
          dbgBLabel "[fA] Attribute group with name reference " ag
        indenting $ do
          let names = map grabNameAndUsage schemes
              defn = AttributeDefn n (AttributeGroupDefn names) Nothing d
          fileNewDefinition defn
          sub <- fmap concat $ mapM flattenAttribute schemes
          dbgResult "Flattened [fA] to" $ sub ++ [defn]
      flattenAttribute a = do
        boxed $ do
          dbgLn "flattenAttribute "
          dbgBLabel "ARG " a
        error "TODO flattenAttribute missing case"

  -- | Converting XML `Content` to `DataScheme`s.
  decodeXML = inputSchemaItems "Top"
    where

      -- | Rewrite otherwise-unstructured parsed XML content structures as a
      -- sequence of internal XSD representations.
      inputSchemaItems :: String -> [Content] -> XSDQ [DataScheme]
      inputSchemaItems outer items = do
        res <- inputSchemaItems' outer items
        -- dbgLn $ show res
        return res

      inputSchemaItems' :: String -> [Content] -> XSDQ [DataScheme]
      inputSchemaItems' outer items = do
        whenDebugging $ do
          dbgPt $ "inputSchemaItems' with \"" ++ outer ++ "\""
          dbgBLabel "  items " items
        res <- mapM (\(s, i) ->
                       indenting $ inputSchemaItem (outer ++ show i) s) $
                    zip items disambigNums
        -- dbgLn $ show res
        return res

      inputSchemaItem :: String -> Content -> XSDQ DataScheme
      inputSchemaItem o e@(Elem (Element q a c l)) = do
        whenDebugging $ dbgPt $ "[iSI] Encoding element " ++ showContent e
        ifDoc <- getAnnotationDocFrom c
        res <- indenting $ inputElement q a c o l ifDoc
        whenDebugging $ dbgBLabel "  Encoding result " res
        return res
      inputSchemaItem _ (Text _) = do
        whenDebugging $ dbgPt "[iSI] Dropping Text entry "
        return Skip
      inputSchemaItem _ (CRef txt) = do
        whenDebugging $ dbgPt $ "[iSI] Dropping CRef entry " ++ txt
        return Skip


      inputElement ::
        QName -> [Attr] -> [Content] -> String -> Maybe Line -> Maybe String
        -> XSDQ DataScheme

      inputElement (QName "element" _ _) ats content outer ln _d = do
        whenDebugging $ do
          dbgPt $ "inputElement for element tag"
          dbgLn $ "  outer tag " ++ outer
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
                   whenDebugging $ boxed $ do
                     dbgLn $
                       "More than one subelement to <element>" ++ ifAtLine ln
                     dbgBLabel "ATS " ats
                     dbgBLabel "CONTENT " content
                     dbgBLabel "INCLUDED " included
                   error $
                     "More than one subelement to <element>" ++ ifAtLine ln
        let implName = case nameQName of
                        Just nqn -> qName nqn
                        Nothing -> case refQName of
                                    Just rqn -> qName rqn
                                    Nothing -> outer ++ "Element"
        dbgResult "Element inputElement result" $
          ElementScheme sub nameQName typeQName refQName ifId implName
                   (decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats)
                   (decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats)
                   isAbstract ln ifDoc

      inputElement q@(QName "attribute" _ _) a c o l _d = do
        whenDebugging $ do
          dbgLn $ "inputElement for attribute tag"
          dbgPt $ "outer tag " ++ o
        ifDoc <- indenting $ getAnnotationDocFrom c
        scheme <- indenting $
          encodeAttribute (o ++ "Attr") q a (filter isFocusElem c) l ifDoc
        whenDebugging $ dbgBLabel "- scheme " scheme
        return $ AttributeScheme scheme l ifDoc

      inputElement q@(QName "attributeGroup" _ _) a c o l _d = do
        whenDebugging $ do
          dbgLn $ "inputElement for attribute group"
          dbgPt $ "outer tag " ++ o
        ifDoc <- getAnnotationDocFrom c
        scheme <- indenting $
          encodeAttribute (o ++ "AtrGrp") q a
                          (filter isNonKeyNonNotationElem c) l ifDoc
        whenDebugging $ dbgBLabel "- scheme " scheme
        return $ AttributeScheme scheme l ifDoc


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
              whenDebugging $
                dbgLn "inputElement > complexType > case \"choice\""
              choiceName <- pullAttrQName "name" ats'
              cts <- indenting $ encodeChoiceTypeScheme choiceName ats' subctnts
              let res = CTS cts [] name l d
              dbgResult "Result is " res

            "complexContent" -> do
              whenDebugging $ dbgLn
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
              dbgResult "inputElement result" $
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
              whenDebugging $ dbgLn
                "      inputElement > complexType > case \"simpleContent\""
              error "inputElement > complexType > case \"simpleContent\""

            _ -> boxed $ do
                    dbgLn      "inputElement > complexType > another tag case"
                    dbgBLabel "ATS " ats
                    dbgBLabel "CTNTS " $ filter isElem ctnts
                    dbgLn $    "OUTER " ++ show outer
                    dbgLn $    "L " ++ show l
                    dbgLn $    "D " ++ show d
                    dbgLn      "-------"
                    dbgBLabel "ATSPECS' " atspecs'
                    dbgBLabel "ATGRSPECS' " atgrspecs'
                    dbgBLabel "NAME " name
                    dbgLn      "-------"
                    dbgLn $    "TAG " ++ show tag
                    dbgBLabel "QN " qn
                    dbgBLabel "ATS' " ats'
                    dbgBLabel "SUBCTNTS " $ filter isElem subctnts
                    error "inputElement > complexType > unmatched"


      inputElement (QName "simpleType" _ _) ats ctnts outer ifLn ifDoc = do
        let ctnts' = filter isElem ctnts
        whenDebugging $ do
          dbgPt "Input element is simpletype"
          dbgLn $ "  Outer name " ++ outer
        indenting $ case separateSimpleTypeContents ats ctnts' of

          (nam, One restr, Zero, Zero) -> do
            whenDebugging $ do
              dbgPt "Subcase restr"
            qnam <- mapM decodePrefixedName nam
            res <- indenting $ encodeSimpleTypeByRestriction qnam
                                 (maybe (outer ++ "Simple") (outer ++) nam)
                                 ats restr
            dbgResult "Subcase result" res

          (ifNam, Zero,
           One (Elem (Element (QName "union" _ _) ats' cs' _)), Zero) -> do
            let outerUnion = outer ++ "Union"
                nam = maybe outerUnion id ifNam
            whenDebugging $ do
              dbgPt "Subcase union"
              dbgBLabel "- ifNam " $ show ifNam
              dbgBLabel "- cs' " cs'
            qnam <- decodePrefixedName nam
            whenDebugging $ do
              dbgBLabel "- qnam " qnam
              dbgPt "Calling inputSchemaItems' "
            nestedAlts <- indenting $
                      inputSchemaItems' outerUnion $ filter isElem cs'
            whenDebugging $ dbgBLabel "- nestedAlts " nestedAlts
            -- Extract from the memberTypes attribute here
            membersAttr <- pullAttrQNameList "memberTypes" ats'
            whenDebugging $ dbgBLabel "- membersAttr " membersAttr
            let members = maybe [] id membersAttr
            dbgResult "Subcase result" $
              STS (Just qnam) (Union nestedAlts members) ifLn ifDoc

          (ifNam, Zero, Zero,
           One (Elem (Element (QName "list" _ _) ats' ctnts'' _))) -> do
            whenDebugging $ dbgPt "Subcase list"
            itemTypeAttr <- pullAttrQName "itemType" ats'
            let simpleWithin = pullContent "simpleType" ctnts''
            indenting $ case (itemTypeAttr, simpleWithin) of

              (Nothing, Zero) -> error $
                "Simple type list without itemType attribute" ++ ifAtLine ifLn

              (Nothing, One node) -> do
                whenDebugging $ dbgPt "Subcase with included element type"
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
                dbgResult "Subcase result" $
                  STS (Just thisName) (List Nothing (Just tds))
                                   ifLn ifDoc

              (Just itemType, Zero) -> do
                whenDebugging $ dbgPt "Subcase with element type reference"
                qnam <- case ifNam of
                  Just n -> decodePrefixedName n
                  Nothing -> return $ QName ("List_" ++ qName itemType)
                                            (qURI itemType) (qPrefix itemType)
                dbgResult "Subcase result" $
                  STS (Just qnam)
                                   (List (Just itemType) Nothing) ifLn ifDoc
              (x, y) -> do
                boxed $ do
                  dbgLn "Disallowed subsubcase within list subcase"
                  dbgBLabel "ATS " ats
                  dbgBLabel "CTNTS' " ctnts'
                  dbgLn $ "OUTER " ++ outer
                  dbgBLabel "X " x
                  dbgBLabel "Y " y
                error $
                  "Disallowed subcase within subcase list" ++ ifAtLine ifLn



          (ifName, zomRestr, zomUnion, zomList) -> do
            -- whenDebugging
            boxed $ do
              dbgLn "TODO inputElement > simpleType > another separation case"
              dbgBLabel "ATS " ats
              dbgBLabel "CTNTS' " ctnts'
              dbgLn $ "OUTER " ++ outer
              dbgLn $ "IFNAME " ++ show ifName
              dbgBLabel "ZOMRESTR " zomRestr
              dbgBLabel "ZOMUNION " zomUnion
              dbgBLabel "ZOMLIST " zomList
            error $ "TODO inputElement > simpleType > another separation case"
              ++ ifAtLine ifLn

      inputElement (QName "annotation" _ _) _ _ _ _ _ = do
        -- We do nothing with documentation and other annotations; currently
        -- there is no way to pass Haddock docstrings via the TH API.
        whenDebugging $ dbgPt $ "Dropping <annotation> element"
        return Skip

      inputElement (QName tag _ _) _ _ _ l _d
                   | tag=="include" || tag=="import" = do
        -- Skipping these documents for now
        dbgLn $ "  - WARNING: skipped <" ++ tag ++ "> element" ++ ifAtLine l
        return Skip

      inputElement (QName tagname _ _) _ _ _ _ _
        | tagname == "key" || tagname == "keyref" = do
        whenDebugging $ dbgPt $ "Dropping <" ++ tagname ++ "> entry "
        return Skip

      inputElement (QName "sequence" _ _) ats ctnts outer ifLn ifDoc = do
        ifName <- pullAttrQName "name" ats
        whenDebugging $ dbgLn $
          maybe "- Sequence (unnamed)"
                (\n -> "- Sequence \"" ++ showQName n ++ "\"")
                ifName
        included <- indenting $ inputSchemaItems (outer ++ "Seq") ctnts
        name <- useNameOrWrap ifName outer "Seq"
        dbgResult "Sequence result" $
          CTS (Composing (filter nonSkip included) [])
                            [] (Just name) ifLn ifDoc

      inputElement (QName "restriction" _ _) ats ctnts outer ifLn ifDoc = do
        whenDebugging $ dbgPt $ "Restriction, outer name " ++ outer
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
            dbgResult "Restriction result" $
              CTS (ComplexRestriction baseQName) []
                                thisName ifLn (pickOrCombine ifDoc ifDoc')
          Nothing -> error "restriction without base"


      inputElement (QName "extension" _ _) ats ctnts outer ifLn ifDoc = do
        whenDebugging $ dbgPt $ "Extension, outer name " ++ outer
        maybeBase <- pullAttrQName "base" ats
        let base = maybe (error $ "<extension> without base" ++ ifAtLine ifLn)
                      id maybeBase
        ifName <- pullAttrQName "name" ats
        name <- useNameOrWrap ifName outer "Ext"
        (ext, newAttrs, newAttrGroups) <- separateComplexContents ctnts ifLn
        whenDebugging $ do
          dbgPt "Complex extension"
          dbgPt $ "outer name " ++ outer
          dbgBLabel "- base " base
          dbgBLabel "- ext " ext
          dbgBLabel "- newAttrs " newAttrs
          dbgBLabel "- newAttrGroups " newAttrGroups
        res <- case ext of
          Nothing -> do
            return $
              CTS (Extension base []) [] (Just name) ifLn ifDoc
          Just (PB (e,_,_,_,_,_,_,_)) -> do
            e' <- indenting $ inputSchemaItem (outer ++ "ExtB") e
            return $
              CTS (Extension base [e']) [] (Just name) ifLn ifDoc
        whenDebugging $ dbgBLabel "  Extension result " res
        return res


      inputElement (QName "group" _ _) ats ctnts outer ifLn ifDoc = do
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        let subOuter = outer ++ "Group"
        let filtered = filter isFocusElem ctnts
        whenDebugging $ do
          dbgLn $ "inputElement case group" ++ ifAtLine ifLn
          dbgPt $ "For <group> schema"
            ++ maybe "(no line num)" (\x -> " at " ++ show x) ifLn
            ++ ":"
          dbgLn $ "  outer name " ++ outer
          dbgLn $ "  name from attributes " ++ maybe "(none)" qName name
          -- dbgLn $ "  filtered content " ++ show (map showContent filtered)
        indenting $ case filtered of
          (Elem (Element (QName "choice" _ _) attrs' ctnts' ifLn')):[] -> do
            whenDebugging $ dbgPt "choice subcase"
            ts <- indenting $ encodeChoiceTypeScheme name attrs' ctnts'
            dbgResult "Subcase result" $
              finishGroupScheme (nameOrRefOpt name ref) (Just ts) ifLn' ifDoc
          (Elem (Element (QName "sequence" _ _) _ats ctnts' _)):[] -> do
            whenDebugging $ do
              dbgPt "sequence subcase after (filter isElem ctnts)"
              indenting $ do
                dbgBLabel "- group attrs " ats
                dbgBLabel "- name " name
                dbgBLabel "- ctnts " $ filter isElem ctnts
                dbgBLabel "- ctnts' " $ filter isElem ctnts'
            seqn <- indenting $ encodeSequenceTypeScheme subOuter ctnts' []
            dbgResult "Subcase result" $
              finishGroupScheme (nameOrRefOpt name ref) (Just seqn) ifLn ifDoc
          (Elem (Element (QName "all" _ _) _ats _contents ifLn')):[] -> do
            whenDebugging $ dbgPt "all subcase"
            -- ifDoc' <- getAnnotationDocFrom contents
            boxed $ do
              dbgLn $
                "TODO inputElement > group with all" ++ ifAtLine ifLn'
              dbgLn $ "ATS " ++ (intercalate "\n    " $ map showAttr ats)
              dbgLn $ "NAME " ++ show (fmap showQName name)
              dbgLn $ "CTNTS " ++
                (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
            error $ "TODO inputElement > group with all" ++ ifAtLine ifLn'
          _ -> do
            whenDebugging $ dbgPt "Default subcase is group of nothing"
            dbgResult "Subcase result" $
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
        whenDebugging $ dbgPt "For <choice> scheme:"
        -- whenDebugging $ do
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        nameRef <- nameOrRefOptDft name ref $ outer ++ "Choice"
        let minOcc = decodeMaybeIntOrUnbound1 $ pullAttr "minOccurs" ats
            maxOcc = decodeMaybeIntOrUnbound1 $ pullAttr "maxOccurs" ats
        whenDebugging $ do
          dbgPt $ "inputElement > choice" ++ ifAtLine ifLn
          dbgLn $ "-- minOccurs " ++ show minOcc
          dbgLn $ "-- maxOccurs " ++ show maxOcc
          dbgBLabel "-- ats " ats
          dbgBLabel "-- ctnts " $ filter isElem ctnts
        ts <- indenting $ encodeChoiceTypeScheme name ats ctnts
        dbgResult "Choice encoding" $ ChoiceScheme nameRef (Just ts) ifLn ifDoc

      inputElement (QName "any" _ _) ats _ outer ifLn ifDoc = do
        whenDebugging $ dbgPt "For <any> scheme:"
        ifName <- pullAttrQName "name" ats
        name <- useNameOrWrap ifName outer "Any"
        whenDebugging $ do
          dbgLn $ "TODO <any>" ++ ifAtLine ifLn
          dbgLn $ "NAME " ++ showQName name
          dbgLn $ "OUTER " ++ outer
        dbgResult "Encoded as" $ UnprocessedXML (Just name) ifLn ifDoc

      inputElement (QName "notation" _ _) _ _ _ _ _ = do
        whenDebugging $ dbgPt "For <notation> scheme:"
        -- whenDebugging $ do
        dbgResult "Encoded as" Skip

      inputElement (QName tag _ _) ats ctnts outer ifLn _ifDoc = do
        -- whenDebugging $ do
        boxed $ do
          dbgLn $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn
          dbgLn $ "TAG " ++ show tag
          dbgBLabel "ATS " ats
          dbgBLabel "CTNTS " $ filter isElem ctnts
          dbgLn $ "OUTER " ++ outer
        error $ "TODO inputElement > unmatched case" ++ ifAtLine ifLn


      encodeSequenceTypeScheme ::
        String -> [Content] -> [Content] -> XSDQ ComplexTypeScheme
      encodeSequenceTypeScheme outer subcontents attrSpecs = indenting $ do
        whenDebugging $ dbgLn $
          "encodeSequenceTypeScheme outer=\"" ++ outer ++ "\""
        included <- indenting $ inputSchemaItems' (outer ++ "Seq") subcontents
        atrSpecs <- indenting $
          mapM (\(e, n) -> encodeAttributeScheme (outer ++ "Seq" ++ show n) e) $
            zip attrSpecs disambigNums
        return $ Composing (filter nonSkip included) atrSpecs

      encodeChoiceTypeScheme ::
        Maybe QName -> [Attr] -> [Content] -> XSDQ ComplexTypeScheme
      encodeChoiceTypeScheme ifNam _attrs allCtnts = indenting $ do
        whenDebugging $ dbgLn "encodeChoiceTypeScheme"
        let ctnts = filter isElem allCtnts
        {-
        whenDebugging $ do
          dbgLn $ "ATS " ++ (intercalate "\n    " $ map showAttr attrs)
          dbgLn $ "IFNAM " ++ show ifNam
          dbgLn $ "CTNTS " ++
              (intercalate "\n    " $ map ppContent $ filter isElem ctnts)
        -}
        contentSchemes <- indenting $ mapM (inputSchemaItem "X") ctnts
        return $ Choice ifNam $ filter nonSkip contentSchemes


      encodeAttributeScheme :: String -> Content -> XSDQ AttributeScheme
      encodeAttributeScheme outer (Elem (Element q a c l)) = indenting $ do
        whenDebugging $ dbgBLabel "- Encoding attribute scheme " q
        ifDoc <- getAnnotationDocFrom c
        res <- indenting $
          encodeAttribute (outer ++ "Elem") q a
                          (filter isNonKeyNonNotationElem c) l ifDoc
        whenDebugging $ dbgBLabel "  Encoding result " res
        return res
      encodeAttributeScheme _o c = do
        dbgBLabel "** Nonattribute" c
        error $ "Illegal use of encodeAttributeScheme on\n" ++ showContent c

      encodeAttribute ::
        String -> QName -> [Attr] -> [Content] -> Maybe Line -> Maybe String
        -> XSDQ AttributeScheme
      encodeAttribute _ (QName "attribute" _ _) ats [] _ d = indenting $ do
        typeQName <- pullAttrQName "type" ats
        refQName <- pullAttrQName "ref" ats
        nameQname <- pullAttrQName "name" ats
        return $ SingleAttribute (nameOrRefOpt nameQname refQName)
                   (maybe Neither NameRef typeQName)
                   (maybe "optional" id $ pullAttr "use" ats)
                   d
      encodeAttribute outer (QName "attribute" _ _) ats (st:sts) l d = do
        whenDebugging $ do
          dbgLn $ "encodeSequenceTypeScheme attribute outer=\"" ++ outer ++ "\""
          dbgBLabel "- ats " ats
          dbgBLabel "- st "  st
          dbgBLabel "- sts " sts
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
                                          (maybe "optional" id $ pullAttr "use" ats)
                                          l d
      encodeAttribute o (QName "attributeGroup" _ _) ats ctnts _ d = do
        indenting $ do
          whenDebugging $ do
            dbgLn $
              "encodeSequenceTypeScheme attributeGroup outer=\"" ++ o ++ "\""
            dbgBLabel "- ats " ats
            dbgBLabel "- ctnts " ctnts
        name <- pullAttrQName "name" ats
        ref <- pullAttrQName "ref" ats
        let attrs = filterTagged "attribute" ctnts
            atGroups = filterTagged "attributeGroup" ctnts
        subcontents <- indenting $ mapM (encodeAttributeScheme $ o ++ "Group") $
                                     attrs ++ atGroups
        return $ AttributeGroup (nameOrRefOpt name ref) subcontents d
      encodeAttribute outer (QName n _ _) a c _ _ = do
        boxed $ do
          dbgLn n
          dbgLn $ "OUTER " ++ outer
          dbgBLabel "A " a
          dbgBLabel "C " $ filter isElem c
        error $ "Can't use encodeAttribute with <" ++ n ++ ">"

      encodeAttributeWithNestedType ::
        String -> NameOrRefOpt -> Content -> [Content] -> String
        -> Maybe Line -> Maybe String
        -> XSDQ AttributeScheme
      encodeAttributeWithNestedType outer nameOrRef tySpec [] use _ d = do
        whenDebugging $ do
          dbgLn $ "encodeAttributeWithNestedType outer=\"" ++ outer ++ "\""
          dbgBLabel "- nameOrRef " nameOrRef
          dbgBLabel "- tySpec " tySpec
          dbgBLabel "- use " use
        ds <- inputSchemaItem outer tySpec
        whenDebugging $ dbgBLabel "- ds " ds
        dbgResult "Result [encodeAttributeWithNestedType]:" $
          SingleAttribute nameOrRef (Nested ds) use d
      encodeAttributeWithNestedType _ _ tySpec (s:ss) _ _ _ = do
        boxed $ do
          dbgLn "Too many nested types for attribute"
          dbgBLabel "1st one " tySpec
          dbgBLabel "2nd one " s
          dbgBLabel "others " ss
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
        whenDebugging $ dbgPt $ "encode simple by restr, outer name " ++ outer
        ifDoc <- getAnnotationDocFrom cs
        case pullAttr "base" ats' of
          Just base -> do
            whenDebugging $ dbgLn $ "- base " ++ base
            baseQName <- decodePrefixedName base
            whenDebugging $ dbgBLabel "- baseQName " baseQName
            let useName = maybe (Just $ withPrefix (outer ++ "Restr") baseQName)
                                (const ifName) ifName
                          -- TODO --- make sure this is in target namespace
            whenDebugging $ dbgBLabel "- useName " useName
            -- freshUseName <- freshenTypeName outer useName
            -- freshBaseName <- freshenTypeName outer $ Just baseQName
            dbgResult "Encoding result" $
              STS useName {- (Just freshUseName) -}
                               (SimpleRestriction baseQName {- freshBaseName -})
                                                  ln ifDoc
          Nothing -> error "restriction without base"
      encodeSimpleTypeByRestriction ifNam _ ats s = do
        boxed $ do
          dbgLn "TODO encodeSimpleTypeByRestriction > additional cases"
          dbgLn $ "IFNAM " ++ show ifNam
          dbgLn $ "ATS "   ++ (intercalate "\n    " $ map showAttr ats)
          case s of
            Elem (Element _ _ _ (Just l)) -> dbgLn $ "source line: " ++ show l
            _ -> return ()
          dbgBLabel "S " s
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
  debugSlug (AttributeScheme (SingleAttribute nameOrRef _ _ _) _ _) =
    "attribute " ++ show nameOrRef
  debugSlug (AttributeScheme (AttributeGroup nameOrRef _ _) _ _) =
    "attribute group " ++ show nameOrRef
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
