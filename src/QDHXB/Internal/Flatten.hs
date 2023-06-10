-- | Internal representation directly reflecting XSD code, in
-- particular allowing nested definitions.
module QDHXB.Internal.Flatten (flattenSchemaItems) where

-- import System.Directory
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
flattenSchemaItems = fmap concat . mapM flattenSchemaItem . filter nonSkip

flattenSchemaItem :: DataScheme -> XSDQ [Definition]
{-# INLINE flattenSchemaItem #-}
flattenSchemaItem s = do
  whenDebugging $ dbgBLabel "[fSI] from " s
  results <- indenting $ flattenSchemaItem' s
  dbgResult "Result [fSI]:" results

flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
flattenSchemaItem' Skip = return []

flattenSchemaItem' (ElementScheme contents ifName ifType ifRef _ifId
                                  ifMin ifMax l ifDoc) = do
  whenDebugging $ dbgLn "[fSI'] Relaying to flattenElementSchemeItem"
  flattenElementSchemeItem contents ifName ifType ifRef ifMin ifMax l ifDoc

flattenSchemaItem' (AttributeScheme
                    (SingleAttribute (WithName nam) (NameRef typ) m d')
                    l d) = do
  whenDebugging $ dbgLn "[fSI'] Flattening single attribute"
  let attrDefn =
        AttributeDefn nam (SingleAttributeDefn typ $ stringToAttributeUsage m)
                      l (pickOrCombine d d')
  fileNewDefinition attrDefn
  dbgResult "Flattened [fSI'] to" [attrDefn]

flattenSchemaItem' (AttributeScheme (SingleAttribute (WithRef _) _ _ _)
                                    _l _d) = do
  return $ error "[fSI'] Reference in attribute"

flattenSchemaItem' (AttributeScheme (AttributeGroup nr cs _) l d) = do
  whenDebugging $ dbgLn "[fSI'] Relaying to flattenAttributeGroupItem for "
  flattenAttributeGroupItem nr cs l d

flattenSchemaItem' (ComplexTypeScheme cts ats ifNam l d) = do
  whenDebugging $ dbgLn "[fSI'] Relaying to flattenComplexTypeScheme"
  flattenComplexTypeScheme cts ats ifNam l d

flattenSchemaItem' sts@(SimpleTypeScheme (Just nam) (Synonym base) ln d) = do
  whenDebugging $ dbgBLabel "[fSI'] Flattening simple synonym " sts
  indenting $ do
    let tyDefn = SimpleSynonymDefn nam base ln d
    fileNewDefinition tyDefn
    dbgResult "Flattened [fSI'] to" [tyDefn]

-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here

flattenSchemaItem' sts@(SimpleTypeScheme (Just nam) (SimpleRestriction base)
                                     ln d) = do
  whenDebugging $ dbgBLabel "[fSI'] Flattening simple restriction " sts
  indenting $ do
    let tyDefn = SimpleSynonymDefn nam base ln d
    addTypeDefn nam tyDefn
    dbgResult "Flattened [fSI'] to" $ [ tyDefn ]

flattenSchemaItem' sts@(SimpleTypeScheme (Just nam) (Union alts) ln d) = do
  whenDebugging $ dbgBLabel "[fSI'] Flattening simple union " sts
  indenting $ do
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
          let typeName = maybe (error "Should not find anonymous decl") id $
                           labelOf d'
          defns <- flattenSchemaItem d'
          return ((name, typeName), defns)
    labelledAlts <- mapM pullLabel alts
    let (names, defnss) = unzip labelledAlts
        defns = concat defnss
    let uDef = UnionDefn nam names ln d
    fileNewDefinition uDef
    dbgResult "Flattened [fSI'] to" $ defns ++ [uDef]


flattenSchemaItem' s@(SimpleTypeScheme (Just nam)
                                       (List (Just elemTyp) Nothing) ln d) = do
  whenDebugging $
    dbgBLabel "[fSI'] Flattening simple list with referenced element type " s
  indenting $ do
    let lDef = ListDefn nam elemTyp ln d
    fileNewDefinition lDef
    dbgResult "Flattened [fSI'] to" [lDef]

flattenSchemaItem' s@(SimpleTypeScheme (Just nam)
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
  whenDebugging $
    dbgBLabel "[fSI'] Flattening group scheme with name and present content " gs
  let typeSchemeName = withSuffix "Group" name
  defs <- flattenComplexTypeScheme cts [] (Just typeSchemeName) ln doc
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
  let defn = GroupDefn name
               (TypeRef typeSchemeName Nothing Nothing Nothing Nothing) ln doc
  fileNewDefinition defn
  dbgResult "Flattened [fSI'] to" $ defs ++ [defn]

flattenSchemaItem' gs@(GroupScheme (WithRef ref) Nothing ln _doc) = do
  whenDebugging $
    dbgBLabel "[fSI'] Flattening group scheme with reference and no content " gs
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
flattenAttributeGroupItem (WithName n) cs l d = do
  whenDebugging $ dbgLn "[fAGI] Flattening attribute group item"
  let names = map grabName cs
  defs <- indenting $ flattenAttributes cs
  let attrDefn = AttributeDefn n (AttributeGroupDefn names) l d
  fileNewDefinition attrDefn
  let res = defs ++ [attrDefn]
  return res
flattenAttributeGroupItem nameRef contents ln _d = do
  boxed $ do
    dbgLn "TODO [fAGI] missed case"
    dbgBLabel "NAMEREF " nameRef
    dbgBLabel "CONTENTS " contents
    dbgLn $ "LN " ++ show ln
  error "TODO flattenAttributeGroupItem unmatched"


flattenComplexTypeScheme ::
  ComplexTypeScheme -> [AttributeScheme] -> Maybe QName
  -> Maybe Line -> Maybe String
  -> XSDQ [Definition]

flattenComplexTypeScheme c@(Composing cts ats0) ats (Just nam) l d = do
  whenDebugging $ dbgBLabel "[fCTS] Complex composition " c
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

flattenComplexTypeScheme c@(ComplexRestriction base) _ats (Just nam) l d = do
  whenDebugging $ dbgBLabel "[fCTS] Complex restriction " c
  let defn = ComplexSynonymDefn nam base l d
  fileNewDefinition defn
  dbgResult "Flattened [fCTS] to" $ [defn]

flattenComplexTypeScheme e@(Extension base ds) _ats (Just nam) l d = do
  whenDebugging $ dbgBLabel "[fCTS] Complex extension " e
  (defs, refs) <- indenting $ flattenSchemaRefs ds
  let defn = ExtensionDefn nam (TypeRef base Nothing Nothing l d) refs l d
  fileNewDefinition defn
  dbgResult "Flattened [fCTS] to" $ defs ++ [defn]

flattenComplexTypeScheme c@(Choice ifName contents) _ats ifOuterName ln doc = do
  whenDebugging $ dbgBLabel "[fCTS] Choice " c
  let name = maybe (maybe (QName "???" Nothing Nothing) id ifOuterName)
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
    TypeRef r ifMin ifMax l d
    ] ats
  let tyDefn = SequenceDefn name refs l d
  fileNewDefinition tyDefn
  dbgResult "Flattened [fCTS] to" $ defs ++ [tyDefn]

  {-
  -- First version --- but (1) this is the defn of a complex type
  -- which includes a group defn, not the defn of a group, and (2)
  -- ignores attributes.
  whenDebugging $ dbgLn "[fCTS] Group by reference"
  dbgResult "Flattened [fCTS] to" [GroupDefn name (TypeRef r ifMin ifMax l d) l d]
  -}

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
  Maybe DataScheme -> Maybe QName -> Maybe QName -> (Maybe QName)
  -> (Maybe Int) -> (Maybe Int) -> (Maybe Line) -> Maybe String
  -> XSDQ [Definition]
flattenElementSchemeItem Nothing (Just nam) (Just typ) Nothing _ _ ln ifDoc = do
  whenDebugging $ dbgLn "[fESI] With name/type"
  indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
flattenElementSchemeItem (Just Skip) (Just nam) (Just typ) _ _ _ ln ifDoc = do
  whenDebugging $ dbgLn "[fESI] Enclosing skip"
  indenting $ flattenWithNameTypeOnly nam typ ln ifDoc
flattenElementSchemeItem (Just (SimpleTypeScheme _ ts ln d))
                          ifName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  whenDebugging $ dbgLn "[fESI] Enclosing simple type scheme"
  flatTS <- flattenSchemaItem' $ SimpleTypeScheme ifName ts ln d
  let elemDefn = ElementDefn nam nam ln ifDoc
  fileNewDefinition elemDefn
  dbgResult "Flattened [fESI] to " $ flatTS ++ [elemDefn]
flattenElementSchemeItem (Just (ComplexTypeScheme ts attrs ifCTSName l d))
                          ifElementName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  whenDebugging $ dbgLn "[fESI] Enclosing complex type scheme"
  let typeName = maybe nam id ifCTSName
  let ifTypeName = maybe ifElementName Just ifCTSName
  whenDebugging $
    dbgLn "Flattening element scheme enclosing complex type scheme"
  flatTS <- flattenSchemaItem' $ ComplexTypeScheme ts attrs ifTypeName l d
  let elemDefn = ElementDefn nam typeName l ifDoc
  fileNewDefinition elemDefn
  dbgResult "Flattened [fESI] to " $ flatTS ++ [elemDefn]
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

grabName :: AttributeScheme -> QName
grabName (SingleAttribute (WithName n) _ _ _) = n
grabName (SingleAttribute (WithRef n) _ _ _) = n
grabName (SingleAttribute WithNeither (NameRef t) _ _) = t
grabName (AttributeGroup (WithName n) _ _) = n
grabName (AttributeGroup (WithRef n) _ _) = n
grabName a = error $ "No useable name in " ++ show a

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
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef (ElementScheme c ifName ifType ifRef _ifId
                                ifLower ifUpper ln ifDoc) = do
  whenDebugging $ dbgLn "[fSR -> flattenElementSchemeRef]"
  flattenElementSchemeRef c ifName ifType ifRef ifLower ifUpper ln ifDoc
flattenSchemaRef (AttributeScheme (SingleAttribute nr t m d') l d) = do
  whenDebugging $ dbgLn "[fSR -> flattenSingleAttributeRef]"
  flattenSingleAttributeRef nr t m l (pickOrCombine d d')
flattenSchemaRef (AttributeScheme (AttributeGroup nameRef cs _) l d) = do
  whenDebugging $ dbgLn "[fSR -> flattenAttributeGroupRef]"
  flattenAttributeGroupRef nameRef cs l d
flattenSchemaRef c@(ComplexTypeScheme _ _ (Just n) ifLine _) = do
  whenDebugging $ dbgBLabel "[fSR] CTS " c
  defns <- indenting $ flattenSchemaItem c
  dbgResult "Flattened [fSR.CTS] to" $
    (defns, ElementRef n (Just 1) (Just 1) ifLine)
flattenSchemaRef s@(SimpleTypeScheme (Just n) _details ifLine _) = do
  whenDebugging $ dbgBLabel "[fSR] STS " s
  defns <- indenting $ flattenSchemaItem s
  dbgResult "Flattened [fSR.STS] to" $
    (defns, ElementRef n (Just 1) (Just 1) ifLine)
flattenSchemaRef gs@(GroupScheme (WithRef ref) _ifCtnts ifLn _) = do
  whenDebugging $ dbgBLabel "[fSR] GS-WR " gs
  dbgResult "Flattened [fSR.GS-WR] to" $
    ([], ElementRef ref (Just 1) (Just 1) ifLn)
flattenSchemaRef gs@(GroupScheme (WithName name) (Just sub) ifLn ifDoc) = do
  whenDebugging $ dbgBLabel "[fSR] GS-WN " gs
  defns <- indenting $ flattenComplexTypeScheme sub [] (Just name) ifLn ifDoc
  dbgResult "Flattened [fSR.GS-WN] to" $
    (defns, ElementRef name (Just 1) (Just 1) ifLn)
flattenSchemaRef (GroupScheme WithNeither (Just cts) ifLn _ifDoc) = do
  boxed $ do
    dbgLn "[fSR] GroupScheme"
    dbgBLabel "CTS " cts
    dbgLn $ "IFLN " ++ maybe "(none)" show ifLn
  error $ "TODO flattenSchemaRef > GroupScheme with neither name nor reference"
  -- return ([], TypeRef ref Nothing Nothing ifLn ifDoc)
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
flattenElementSchemeRef Nothing Nothing Nothing (Just r) lower upper ln _ifDoc = do
  whenDebugging $ dbgLn "[fESR] all Nothing"
  let result = ElementRef r lower upper ln
  whenDebugging $ do
    dbgLn $ "Flattening element schema with reference only"
    dbgBLabel "  to " result
  dbgResult ("Ref to " ++ showQName r ++ " flattened [fESR] to") ([], result)
flattenElementSchemeRef Nothing (Just n)
                        (Just t@(QName resolvedName _resolvedURI _))
                        Nothing lo up ln ifDoc = do
  whenDebugging $ dbgLn "[fESR] first Nothing"
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
                        ("Name ref " ++ showQName n ++ " flattened [fESR] to")
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
             dbgResult ("Ref to " ++ showQName n ++ " flattened [fESR] to")
               ([defn1, defn2], ref))
flattenElementSchemeRef s@(Just (ComplexTypeScheme _ _ Nothing _ _))
                        n@(Just nam) t@Nothing r@Nothing lower upper ln ifDoc = do
  whenDebugging $ dbgLn "[fESR] t and r and Nothing"
  prev <- flattenElementSchemeItem s n t r lower upper ln ifDoc
  let ref = ElementRef nam lower upper ln
  whenDebugging $ do
    dbgLn "Flattening element schema with name and nested complex type"
    dbgBLabel "       " s
    dbgBLabel "    to " prev
    dbgBLabel "       " ref
  dbgResult "Flattened [fESR] to" (prev, ref)
flattenElementSchemeRef ctnts maybeName maybeType maybeRef lower upper _ _ = do
  boxed $ do
    whenDebugging $ dbgLn "[fESR] flattenElementSchemeRef"
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
flattenAttribute sa@(SingleAttribute (WithName n) (NameRef typ) mode d) = do
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

  -- MAYBE THIS SHOULDN'T BE CALLING [fSR] SINCE IT RETURNS AN ELEMENT-REF

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
    let names = map grabName schemes
        defn = AttributeDefn n (AttributeGroupDefn names) Nothing d
    fileNewDefinition defn
    sub <- fmap concat $ mapM flattenAttribute schemes
    dbgResult "Flattened [fA] to" $ sub ++ [defn]
flattenAttribute a = do
  boxed $ do
    dbgLn "flattenAttribute "
    dbgBLabel "ARG " a
  error "TODO flattenAttribute missing case"

