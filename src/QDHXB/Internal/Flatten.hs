-- | Internal representation directly reflecting XSD code, in
-- particular allowing nested definitions.
module QDHXB.Internal.Flatten (flattenSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import Text.XML.Light.Types
import Text.XML.Light.Output
import QDHXB.Internal.Generate
import QDHXB.UtilMisc
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.Misc (pickOrCombine)
import QDHXB.Internal.Utils.TH (firstToUpper)
import QDHXB.Internal.Utils.XMLLight (withSuffix)
import QDHXB.Internal.NestedTypes
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

-- |Rewrite internally-represented XSD definitions, flattening any
-- nested definitions.
flattenSchemaItems :: [DataScheme] -> XSDQ [Definition]
flattenSchemaItems = fmap concat . mapM flattenSchemaItem

flattenSchemaItem :: DataScheme -> XSDQ [Definition]
{-# INLINE flattenSchemaItem #-}
flattenSchemaItem s = do
  whenDebugging $ dbgBLabel "> Flattening " s
  results <- indenting $ flattenSchemaItem' s
  whenDebugging $ dbgBLabel "  Result: " results
  return results

flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
flattenSchemaItem' Skip = return []

flattenSchemaItem' (ElementScheme contents ifName ifType ifRef _ifId
                                  ifMin ifMax l ifDoc) = do
  whenDebugging $ dbgLn "Relaying to flattenElementSchemeItem"
  indenting $
    flattenElementSchemeItem contents ifName ifType ifRef ifMin ifMax l ifDoc

flattenSchemaItem' (AttributeScheme
                    (SingleAttribute (WithName nam) (NameRef typ) m d')
                    l d) = do
  whenDebugging $ dbgLn "Flattening single attribute"
  let attrDefn =
        AttributeDefn nam (SingleAttributeDefn typ $ stringToAttributeUsage m)
                      l (pickOrCombine d d')
  fileNewDefinition attrDefn
  dbgResult "Flattened to" [attrDefn]

flattenSchemaItem' (AttributeScheme (SingleAttribute (WithRef _) _ _ _)
                                    _l _d) = do
  return $ error "Reference in attribute"

flattenSchemaItem' (AttributeScheme (AttributeGroup nr cs _) l d) = do
  whenDebugging $ dbgLn "Relaying to flattenAttributeGroupItem"
  indenting $
    flattenAttributeGroupItem nr cs l d

flattenSchemaItem' (ComplexTypeScheme cts ats ifNam l d) =
  flattenComplexTypeScheme cts ats ifNam l d

flattenSchemaItem' (SimpleTypeScheme (Just nam) (Synonym base) ln d) = do
  whenDebugging $ dbgLn "Flattening simple synonym"
  let tyDefn = SimpleSynonymDefn nam base ln d
  addTypeDefn nam tyDefn
  dbgResult "Flattened to" [tyDefn]
-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here

flattenSchemaItem' (SimpleTypeScheme (Just nam) (SimpleRestriction base)
                                     ln d) = do
  whenDebugging $ dbgLn "Flattening simple restriction"
  let tyDefn = SimpleSynonymDefn nam base ln d
  addTypeDefn nam tyDefn
  dbgResult "Flattened to" $ [ tyDefn ]

flattenSchemaItem' (SimpleTypeScheme (Just nam) (Union alts) ln d) = do
  whenDebugging $ dbgLn "Flattening simple union"
  let nameUnnamed :: QName -> DataScheme -> DataScheme
      nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifId
                                   ifMin ifMax l ifDoc) =
        ElementScheme ctnts (Just q) ifType ifRef ifId ifMin ifMax l ifDoc
      nameUnnamed q (AttributeScheme
                     (SingleAttribute (WithRef _) ifType usage d') ln' d'') =
        AttributeScheme (SingleAttribute (WithName q) ifType usage d') ln'
          (pickOrCombine d d'')
      nameUnnamed q (ComplexTypeScheme form attrs Nothing l d') =
        ComplexTypeScheme form attrs (Just q) l d'
      nameUnnamed q (SimpleTypeScheme Nothing detail ln' d') =
        SimpleTypeScheme (Just q) detail ln' d'
      nameUnnamed _ b = b

      pullLabel :: DataScheme -> XSDQ ((QName, QName), [Definition])
      pullLabel ds = do
        nameSuffix <- case labelOf ds of
                        Just q -> return q
                        Nothing -> do
                          freshName <- getNextCapName
                          freshQName <- decodePrefixedName freshName
                          return freshQName
        let name = withSuffix (firstToUpper $ qName nameSuffix) nam
        let d' = nameUnnamed name ds
        let typeName = case labelOf d' of
                         Just n -> n
                         Nothing -> error "Should not find anonymous decl"
        defns <- flattenSchemaItem d'
        return ((name, typeName), defns)
  labelledAlts <- mapM pullLabel alts
  let (names, defnss) = unzip labelledAlts
      defns = concat defnss
  let uDef = UnionDefn nam names ln d
  addTypeDefn nam uDef
  dbgResult "Flattened to" $ defns ++ [uDef]


flattenSchemaItem' (SimpleTypeScheme (Just nam)
                                     (List (Just elemTyp) Nothing) ln d) = do
  whenDebugging $ dbgLn "Flattening simple list with referenced element type"
  let lDef = ListDefn nam elemTyp ln d
  addTypeDefn nam lDef
  dbgResult "Flattened to" [lDef]

flattenSchemaItem' (SimpleTypeScheme (Just nam)
                                     (List Nothing (Just inlineTyp)) ln d) = do
  whenDebugging $ dbgLn "Flattening simple list with inline element"
  (subdefs, subref) <- flattenSchemaRef inlineTyp
  let lDef = ListDefn nam (referenceQName subref) ln d
  addTypeDefn nam lDef
  dbgResult "Flattened to" $ subdefs ++ [lDef]

flattenSchemaItem' (GroupScheme (WithName name) (Just cts) ln doc) = do
  defs <- flattenComplexTypeScheme cts [] (Just name) ln doc
  {-
  boxed $ do
    dbgLn "TODO flattenSchemaItem' group case"
    dbgLn $ "NAME " ++ qName name
    dbgBLabel "DEFS " defs
    -- dbgBLabel "REF " ref
    dbgBLabel "CTS " cts
    dbgLn $ "LN " ++ show ln
  error $ "TODO flatten group " ++ maybe "(unnamed)" qName ifName ++ " case: "
  -}
  return $ defs ++ [
    GroupDefn name (Just (TypeRef name Nothing Nothing Nothing Nothing)) ln doc
    ]

flattenSchemaItem' (GroupScheme (WithRef ref) Nothing ln _doc) = do
  boxed $ do
    dbgLn "TODO flattenSchemaItem' group with ref, no contents"
    dbgLn $ "REF " ++ qName ref
    dbgLn $ "LN " ++ show ln
  error $ "TODO flatten group with ref "
    ++ qName ref
    ++ ", no contents, at "
    ++ maybe "(no XSD line num)" show ln

flattenSchemaItem' s = do
  boxed $ do
    dbgLn "TODO flattenSchemaItem' missed case"
    dbgBLabel "ARG " s
  error $ show $ labelBlock "TODO another flatten case: " $ block s

flattenAttributeGroupItem ::
  NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
  -> XSDQ [Definition]
flattenAttributeGroupItem (WithName n) cs l d = do
  whenDebugging $ dbgLn "Flattening attribute group"
  let names = map grabName cs
  defs <- indenting $ flattenAttributes cs
  let res = defs ++ [AttributeDefn n (AttributeGroupDefn names) l d]
  return res
flattenAttributeGroupItem nameRef contents ln _d = do
  boxed $ do
    dbgLn "TODO flattenAttributeGroupItem missed case"
    dbgBLabel "NAMEREF " nameRef
    dbgBLabel "CONTENTS " contents
    dbgLn $ "LN " ++ show ln
  error "TODO flattenAttributeGroupItem unmatched"


flattenComplexTypeScheme ::
  ComplexTypeScheme -> [DataScheme] -> Maybe QName
  -> Maybe Line -> Maybe String
  -> XSDQ [Definition]
flattenComplexTypeScheme (Composing cts ats0) ats (Just nam) l d = do
  whenDebugging $ dbgLn "Flattening complex composition"
  (defs, refs) <-
    musterComplexSequenceComponents (filter nonSkip cts)
      -- TODO DOC possible to add a docstring here?
      (map (\x -> AttributeScheme x Nothing Nothing) ats0 ++ ats) nam
  let tyDefn = SequenceDefn (qName nam) refs l d
  addTypeDefn nam tyDefn
  -- whenDebugging $ do
  --   recheck <- isKnownType nam
  --   dbgLn $ "- Have set " ++ qName nam
  --            ++ " to be known; rechecked as " ++ show recheck
  dbgResult "Flattened to" $ defs ++ [ tyDefn ]

flattenComplexTypeScheme (ComplexRestriction base) ats (Just nam) l d = do
  whenDebugging $ dbgLn "Flattening complex restriction"
  dbgResult "Flattened to" $ [ComplexSynonymDefn nam base l d]

flattenComplexTypeScheme (Extension base ds) ats (Just nam) l d = do
  whenDebugging $ dbgLn "Flattening complex extension"
  (defs, refs) <- indenting $ flattenSchemaRefs ds
  dbgResult "Flattened to" $ defs ++ [
    ExtensionDefn nam (TypeRef base Nothing Nothing l d) refs l d
    ]

flattenComplexTypeScheme (Choice ifName contents) ats ifOuterName ln doc = do
  let name = maybe (maybe (QName "???" Nothing Nothing) id ifOuterName)
                   id ifName
  (defs, refs) <- flattenSchemaRefs contents
  let labelledRefs = zipWith getLabelledDisjunct refs contents
  {-
  boxed $ do
    dbgLn "TODO flattenComplexTypeScheme Choice case"
    dbgLn $ "NAME " ++ name
    dbgBLabel "DEFS " defs
    dbgBLabel "REFS " refs
    dbgBLabel "CONTENTS " contents
    dbgBLabel "ATS " ats
    dbgBLabel "inner IFNAME " ifName
    dbgBLabel "outer IFNAME " ifOuterName
    dbgLn $ "LN " ++ show ln
  error "TODO flattenComplexTypeScheme Choice case"
  -}
  return $ defs ++ [ChoiceDefn name labelledRefs ln doc]

flattenComplexTypeScheme cts ats ifName ln _d = do
  boxed $ do
    dbgLn "TODO flattenComplexTypeScheme missed case"
    dbgBLabel "CTS " cts
    dbgBLabel "ATS " ats
    dbgBLabel "IFNAME " ifName
    dbgLn $ "LN " ++ show ln
  error "TODO flattenComplexTypeScheme missed case"

getLabelledDisjunct :: Reference -> DataScheme -> (QName, Reference)
getLabelledDisjunct ref ds = (case labelOf ds of
                                Nothing -> referenceBase ref
                                Just n -> n,
                              ref)

flattenElementSchemeItem ::
  Maybe DataScheme -> Maybe QName -> Maybe QName -> (Maybe QName)
  -> (Maybe Int) -> (Maybe Int) -> (Maybe Line) -> Maybe String
  -> XSDQ [Definition]
flattenElementSchemeItem Nothing (Just nam) (Just typ) Nothing _ _ ln ifDoc = do
  whenDebugging $ dbgLn "Flattening element scheme with name/type"
  indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
flattenElementSchemeItem (Just Skip) (Just nam) (Just typ) _ _ _ ln ifDoc = do
  whenDebugging $ dbgLn "Flattening element scheme enclosing skip"
  indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
flattenElementSchemeItem (Just (SimpleTypeScheme _ ts ln d))
                          ifName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  whenDebugging $ dbgLn "Flattening element scheme enclosing simple type scheme"
  flatTS <- flattenSchemaItem' $ SimpleTypeScheme ifName ts ln d
  let elemDefn = ElementDefn nam nam ln ifDoc
  fileNewDefinition elemDefn
  dbgResult "Flattened to " $ flatTS ++ [elemDefn]
flattenElementSchemeItem (Just (ComplexTypeScheme ts attrs ifCTSName l d))
                          ifElementName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  let typeName = maybe nam id ifCTSName
  let ifTypeName = maybe ifElementName Just ifCTSName
  whenDebugging $
    dbgLn "Flattening element scheme enclosing complex type scheme"
  flatTS <- flattenSchemaItem' $ ComplexTypeScheme ts attrs ifTypeName l d
  let elemDefn = ElementDefn nam typeName l ifDoc
  fileNewDefinition elemDefn
  dbgResult "Flattened to " $ flatTS ++ [elemDefn]
  {-
flattenElementSchemeItem (Just (ComplexTypeScheme ts attrs (Just n) _l _d))
                          ifName ifType ifRef ifMin ifMax _ifLn _ifDoc = do
  boxed $ do
    dbgLn $ "flattenElementSchemeItem"
    dbgBLabel "TS " ts
    dbgBLabel "ATTRS " attrs
    dbgBLabel "N " n
    dbgBLabel "IFNAME " ifName
    dbgBLabel "IFTYPE " ifType
    dbgBLabel "IFREF " ifRef
    dbgLn $ "IFMIN " ++ show ifMin
    dbgLn $ "IFMAX " ++ show ifMax
  error "Unmatched ComplexTypeScheme case for flattenElementSchemeItem"
-}
flattenElementSchemeItem content ifName ifType ifRef ifMin ifMax _ _ifDoc = do
  boxed $ do
    dbgLn "flattenElementSchemeItem"
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
  whenDebugging $ dbgLn "Flattening element scheme with name/type"
  let elemDefn = ElementDefn nam typ ln ifDoc
  fileNewDefinition elemDefn
  dbgResult "Flattened to " [ elemDefn ]
  {-
  isKnown <- isKnownType typ
  isSimple <- isSimpleType typ
  whenDebugging $ dbgLn $
    "- Checking whether " ++ showQName typ ++ " is simple: " ++ show isSimple
  if isSimple || not isKnown
    then (do whenDebugging $ dbgLn "Subcase for simple-or-unknown"
             let tyDefn = ElementTypeDecl nam typ ln ifDoc
             addTypeDefn nam tyDefn
             let elemDefn = ElementDefn nam nam ln ifDoc
             fileNewDefinition elemDefn
             dbgResult "Flattened to " [ tyDefn, elemDefn ])
    else do whenDebugging $ dbgLn "Subcase for known complex"
            let elemDefn = ElementDefn nam typ ln ifDoc
            fileNewDefinition elemDefn
            dbgResult "Flattened to " [ elemDefn ]
-}


musterComplexSequenceComponents ::
  [DataScheme] ->  [DataScheme] -> QName -> XSDQ ([Definition], [Reference])
musterComplexSequenceComponents steps ats _ = do
  whenDebugging $ dbgLn "musterComplexSequenceComponents"
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return (otherDefs ++ atsDefs, atsRefs ++ refs)

grabName :: AttributeScheme -> QName
grabName (SingleAttribute (WithName n) _ _ _) = n
grabName (SingleAttribute (WithRef n) _ _ _) = n
grabName (SingleAttribute WithNeither (NameRef t) _ _) = t
grabName (AttributeGroup (WithName n) _ _) = n
grabName (AttributeGroup (WithRef n) _ _) = n
grabName a = error $ "No useable name in " ++ show a

flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef (ElementScheme c ifName ifType ifRef _ifId
                                ifLower ifUpper ln ifDoc) =
  flattenElementSchemeRef c ifName ifType ifRef ifLower ifUpper ln ifDoc
flattenSchemaRef (AttributeScheme (SingleAttribute nr t m d') l d) =
  flattenSingleAttributeRef nr t m l (pickOrCombine d d')
flattenSchemaRef (AttributeScheme (AttributeGroup nameRef cs _) l d) =
  flattenAttributeGroupRef nameRef cs l d
flattenSchemaRef c@(ComplexTypeScheme _ _ (Just n) ifLine ifDoc) = do
  defns <- flattenSchemaItem c
  return (defns, TypeRef n Nothing Nothing ifLine ifDoc)
flattenSchemaRef s@(SimpleTypeScheme (Just n) _details ifLine ifDoc) = do
  defns <- flattenSchemaItem s
  return (defns, TypeRef n Nothing Nothing ifLine ifDoc)
flattenSchemaRef (GroupScheme (WithRef ref) _ifCtnts ifLn ifDoc) = do
  {-
  boxed $ do
    dbgLn "flattenSchemaRef of GroupScheme"
    dbgBLabel "REF " ref
    dbgBLabel "IFCTNTS " ifCtnts
    dbgLn $ "IFLN " ++ maybe "(none)" show ifLn
  -}
  return ([], TypeRef ref Nothing Nothing ifLn ifDoc)
flattenSchemaRef s = do
  boxed $ do
    dbgLn "flattenSchemaRef"
    dbgBLabel "arg " s
  error $ "TODO flattenSchemaRef > additional case:"

flattenAttributeGroupRef ::
  NameOrRefOpt -> [AttributeScheme] -> Maybe Line -> Maybe String
  -> XSDQ ([Definition], Reference)
flattenAttributeGroupRef n@(WithName name) contents l d = do
  refs <- flattenAttributeGroupItem n contents l d
  return (refs, AttributeRef name Required)
flattenAttributeGroupRef (WithRef ref) [] _ln _d = do
  return ([], AttributeRef ref Required)
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
  let res = AttributeRef ref (stringToAttributeUsage useStr)
  return ([], res)
flattenSingleAttributeRef (WithName nam) (NameRef t) m l d = do
  let defn = AttributeDefn nam
               (SingleAttributeDefn t $ stringToAttributeUsage m) l d
      ref = AttributeRef nam (stringToAttributeUsage m)
  return ([defn], ref)
flattenSingleAttributeRef (WithName nam) (Nested t) m l d = do
  boxed $ do
    dbgLn "new Nested case"
    dbgBLabel "NAM " nam
    dbgBLabel "T " t
    dbgLn $ "MODE " ++ m
  error "TODO new Nested case"
flattenSingleAttributeRef nameRef maybeType mode _ _ = do
  boxed $ do
    dbgLn "flattenSingleAttributeRef"
    dbgBLabel "NAMEREF " nameRef
    dbgBLabel "IFTYPE " maybeType
    dbgLn $ "MODE " ++ mode
  error "TODO flattenSingleAttributeRef > unmatched case"


flattenElementSchemeRef ::
  Maybe DataScheme -> Maybe QName -> Maybe QName -> Maybe QName
  -> Maybe Int -> Maybe Int -> Maybe Line -> Maybe String
  -> XSDQ ([Definition], Reference)
-- flattenElementSchemeRef contents ifName ifType ifRef ifLower ifUpper =
flattenElementSchemeRef Nothing Nothing Nothing (Just r) lower upper ln _ifDoc = do
  let result = ElementRef r lower upper ln
  whenDebugging $ do
    dbgLn $ "> Flattening element schema with reference only"
    dbgBLabel "  to " result
  return ([], result)
flattenElementSchemeRef Nothing (Just n)
                        (Just t@(QName resolvedName _resolvedURI _))
                        Nothing lo up ln ifDoc = do
  isKnown <- isKnownType t
  whenDebugging $ dbgLn $
    "- Checking whether " ++ resolvedName ++ " is known: " ++ show isKnown
  if isKnown then (do let defn = ElementDefn n t ln ifDoc
                          ref = ElementRef n lo up ln
                      fileNewDefinition defn
                      whenDebugging $ do
                        dbgLn "> Flattening schema with type"
                        -- dbgBLabel "       " e
                        dbgBLabel "    to " defn
                        dbgBLabel "       " ref
                      return ([defn], ref))
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
             return ([defn1, defn2], ref))
flattenElementSchemeRef s@(Just (ComplexTypeScheme _ _ Nothing _ _))
                        n@(Just nam) t@Nothing r@Nothing lower upper ln ifDoc = do
  prev <- flattenElementSchemeItem s n t r lower upper ln ifDoc
  let ref = ElementRef nam lower upper ln
  whenDebugging $ do
    dbgLn "  > Flattening element schema with name and nested complex type"
    dbgBLabel "       " s
    dbgBLabel "    to " prev
    dbgBLabel "       " ref
  return (prev, ref)
flattenElementSchemeRef ctnts maybeName maybeType maybeRef lower upper _ _ = do
  boxed $ do
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
flattenAttribute (SingleAttribute (WithName n) (NameRef typ) mode d) = do
  whenDebugging $ dbgPt "Flattening single attribute with type reference"
  dbgResult "Flattened to" $ [
    AttributeDefn n
      (SingleAttributeDefn typ $ stringToAttributeUsage mode)
      Nothing d
    ]
flattenAttribute (SingleAttribute (WithName n) (Nested ds) mode d) = do
  whenDebugging $ dbgPt "Flattening single attribute with nested type"
  (defs, ref) <- flattenSchemaRef ds
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
  case ref of
    TypeRef qn (Just 1) (Just 1) ln doc -> do
      dbgResult "Flattened to" $ defs ++ [
        AttributeDefn n
          (SingleAttributeDefn qn $ stringToAttributeUsage mode)
          Nothing d
        ]
    TypeRef qn Nothing Nothing ln doc -> do
      dbgResult "Flattened to" $ defs ++ [
        AttributeDefn n
          (SingleAttributeDefn qn $ stringToAttributeUsage mode)
          Nothing d
        ]
    TypeRef qn mn (Just 1) ln _ ->
      error $ "minOccurs " ++ show mn ++ " for attribute type "
              ++ showQName qn ++ " of " ++ showQName n ++ " not allowed"
    TypeRef qn mn Nothing ln _ ->
      error $ "minOccurs " ++ show mn ++ " for attribute type "
              ++ showQName qn ++ " of " ++ showQName n ++ " not allowed"
    TypeRef qn _ mx ln _ ->
      error $ "maxOccurs " ++ show mx ++ " for attribute type "
              ++ showQName qn ++ " of " ++ showQName n ++ " not allowed"
    ref ->
      error $ "Nested type " ++ show ref
              ++ " for attribute " ++ showQName n ++ " not allowed"
flattenAttribute (AttributeGroup (WithName n) schemes d) = do
  let names = map grabName schemes
  sub <- fmap concat $ mapM flattenAttribute schemes
  return $ sub ++ [AttributeDefn n (AttributeGroupDefn names) Nothing d]
flattenAttribute a = do
  boxed $ do
    dbgLn "flattenAttribute "
    dbgBLabel "ARG " a
  error "TODO flattenAttribute missing case"

