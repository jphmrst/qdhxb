
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
  results <- flattenSchemaItem' s
  whenDebugging $ do
    liftIO $ bLabelPrintln "> Flattening " s
    liftIO $ bLabelPrintln "   `--> " results
  return results

flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
flattenSchemaItem' Skip = return []
flattenSchemaItem' (ElementScheme contents ifName ifType ifRef _ifId
                                  ifMin ifMax l ifDoc) =
  flattenElementSchemeItem contents ifName ifType ifRef ifMin ifMax l ifDoc
flattenSchemaItem' (AttributeScheme
                    (SingleAttribute (Just nam) Nothing (Just typ) m d') l d) =
  let attrDefn =
        AttributeDefn nam (SingleAttributeDefn typ $ stringToAttributeUsage m)
                      l (pickOrCombine d d')
  in do fileNewDefinition attrDefn
        return [attrDefn]
flattenSchemaItem' (AttributeScheme (SingleAttribute _ (Just _) _ _ _)
                                    _l _d) = do
  return $ error "Reference in attribute"
flattenSchemaItem' (AttributeScheme (AttributeGroup n r cs _gd) l d) =
  flattenAttributeGroupItem n r cs l d
flattenSchemaItem' (ComplexTypeScheme (Composing cts ats0) ats (Just nam) l _d) = do
  (defs, refs) <-
    musterComplexSequenceComponents cts
      -- TODO DOC possible to add a docstring here?
      (map (\x -> AttributeScheme x Nothing Nothing) ats0 ++ ats) nam
  let tyDefn = SequenceDefn (qName nam) refs l
  addTypeDefn nam tyDefn
  whenDebugging $ do
    recheck <- isKnownType nam
    liftIO $ putStrLn $
      "  - Have set " ++ qName nam
      ++ " to be known; rechecked as " ++ show recheck
  return $ defs ++ [ tyDefn ]
flattenSchemaItem' (SimpleTypeScheme (Just nam) (Synonym base) ln d) = do
  let tyDefn = SimpleSynonymDefn nam base ln d
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here
flattenSchemaItem' (SimpleTypeScheme (Just nam) (SimpleRestriction base) ln d) = do
  let tyDefn = SimpleSynonymDefn nam base ln d
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
flattenSchemaItem' (SimpleTypeScheme (Just nam) (Union alts) ln _d) = do
  let nameUnnamed :: QName -> DataScheme -> DataScheme
      nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifId
                                   ifMin ifMax l ifDoc) =
        ElementScheme ctnts (Just q) ifType ifRef ifId ifMin ifMax l ifDoc
      nameUnnamed q (AttributeScheme
                     (SingleAttribute Nothing ifRef ifType usage d') ln' d) =
        AttributeScheme (SingleAttribute (Just q) ifRef ifType usage d') ln' d
      nameUnnamed q (ComplexTypeScheme form attrs Nothing l d) =
        ComplexTypeScheme form attrs (Just q) l d
      nameUnnamed q (SimpleTypeScheme Nothing detail ln' d) =
        SimpleTypeScheme (Just q) detail ln' d
      nameUnnamed _ b = b

      pullLabel :: DataScheme -> XSDQ ((QName, QName), [Definition])
      pullLabel d = do
        nameSuffix <- case labelOf d of
                        Just q -> return q
                        Nothing -> do
                          freshName <- getNextCapName
                          freshQName <- decodePrefixedName freshName
                          return freshQName
        let name = withSuffix (firstToUpper $ qName nameSuffix) nam
        let d' = nameUnnamed name d
        let typeName = case labelOf d' of
                         Just n -> n
                         Nothing -> error "Should not find anonymous decl"
        defns <- flattenSchemaItem d'
        return ((name, typeName), defns)
  labelledAlts <- mapM pullLabel alts
  let (names, defnss) = unzip labelledAlts
      defns = concat defnss
  let uDef = UnionDefn nam names ln
  addTypeDefn nam uDef
  return $ defns ++ [uDef]
flattenSchemaItem' (SimpleTypeScheme (Just nam) (List (Just elemType)) ln _d) = do
  let lDef = ListDefn nam elemType ln
  addTypeDefn nam lDef
  return [lDef]
flattenSchemaItem' s = do
  liftIO $ putStrLn      "+------"
  liftIO $ putStrLn      "| TODO flattenSchemaItem' missed case"
  liftIO $ bLabelPrintln "| " s
  error $ show $ labelBlock "TODO another flatten case: " $ block s

flattenAttributeGroupItem ::
  Maybe QName -> Maybe QName -> [AttributeScheme] -> Maybe Line -> Maybe String
  -> XSDQ [Definition]
flattenAttributeGroupItem (Just n) Nothing cs l d = do
  let names = map grabName cs
  defs <- flattenAttributes cs
  let res = defs ++ [AttributeDefn n (AttributeGroupDefn names) l d]
  return res
flattenAttributeGroupItem name ref contents ln _d = do
  liftIO $ do
    putStrLn      "+------"
    putStrLn      "| TODO flattenAttributeGroupItem missed case"
    bLabelPrintln "| NAME " name
    bLabelPrintln "| REF " ref
    bLabelPrintln "| CONTENTS " contents
    putStrLn $ "| LN " ++ show ln
  error "TODO flattenAttributeGroupItem unmatched"

flattenElementSchemeItem ::
  [DataScheme] -> Maybe QName -> Maybe QName -> Maybe QName
  -> Maybe Int -> Maybe Int -> Maybe Line -> Maybe String
  -> XSDQ [Definition]
flattenElementSchemeItem contents ifName ifType ifRef ifMin ifMax ifLine ifDoc =
  flattenElementSchemeItem' (filter nonSkip contents)
                            ifName ifType ifRef ifMin ifMax ifLine ifDoc

flattenElementSchemeItem' ::
  [DataScheme] -> Maybe QName -> Maybe QName -> (Maybe QName)
  -> (Maybe Int) -> (Maybe Int) -> (Maybe Line) -> Maybe String
  -> XSDQ [Definition]
flattenElementSchemeItem' [] (Just nam) (Just typ) Nothing _ _ ln ifDoc = do
  isKnown <- isKnownType typ
  isSimple <- isSimpleType typ
  whenDebugging $ do
    liftIO $ putStrLn $ "  - Checking whether "
      ++ showQName typ ++ " is simple: " ++ show isSimple
  if isSimple || not isKnown
    then (do let tyDefn = SimpleSynonymDefn nam typ ln ifDoc
             addTypeDefn nam tyDefn
             let elemDefn = ElementDefn nam nam ln ifDoc
             fileNewDefinition elemDefn
             return [ tyDefn, elemDefn ])
    else do let elemDefn = ElementDefn nam typ ln ifDoc
            fileNewDefinition elemDefn
            return [ elemDefn ]
flattenElementSchemeItem' [SimpleTypeScheme Nothing ts ln d]
                         ifName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  flatTS <- flattenSchemaItem' $ SimpleTypeScheme ifName ts ln d
  let elemDefn = ElementDefn nam nam ln ifDoc
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenElementSchemeItem' [ComplexTypeScheme ts attrs Nothing l d]
                                  ifName@(Just nam) Nothing Nothing _ _ _ ifDoc = do
  flatTS <- flattenSchemaItem' $ ComplexTypeScheme ts attrs ifName l d
  let elemDefn = ElementDefn nam nam l ifDoc
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenElementSchemeItem' contents ifName ifType ifRef ifMin ifMax _ _ifDoc = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenElementSchemeItem'"
    bLabelPrintln "| IFNAME " ifName
    bLabelPrintln "| IFTYPE " ifType
    bLabelPrintln "| IFREF " ifRef
    putStrLn $ "| IFMIN " ++ show ifMin
    putStrLn $ "| IFMAX " ++ show ifMax
    bLabelPrintln "| CONTENTS " contents
  error "Unmatched case for flattenElementSchemeItem'"

musterComplexSequenceComponents ::
  [DataScheme] ->  [DataScheme] -> QName -> XSDQ ([Definition], [Reference])
musterComplexSequenceComponents steps ats _ = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return (otherDefs ++ atsDefs, atsRefs ++ refs)

grabName :: AttributeScheme -> QName
grabName (SingleAttribute (Just n) _ _ _ _) = n
grabName (SingleAttribute Nothing (Just n) _ _ _) = n
grabName (SingleAttribute Nothing Nothing (Just t) _ _) = t
grabName (AttributeGroup (Just n) _ _ _) = n
grabName (AttributeGroup Nothing (Just n) _ _) = n
grabName a = error $ "No useable name in " ++ show a

flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef (ElementScheme c ifName ifType ifRef _ifId
                                ifLower ifUpper ln ifDoc) =
  flattenElementSchemeRef c ifName ifType ifRef ifLower ifUpper ln ifDoc
flattenSchemaRef (AttributeScheme (SingleAttribute n r t m d') l d) =
  flattenSingleAttributeRef n r t m l (pickOrCombine d d')
flattenSchemaRef (AttributeScheme (AttributeGroup ifName ifRef contents _gd) l d) =
  flattenAttributeGroupRef ifName ifRef contents l d
flattenSchemaRef (ComplexTypeScheme _ _ _ _ _) = -- typeDetail _ maybeName
  error "TODO flattenSchemaRef > ComplexTypeScheme"
flattenSchemaRef s = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenSchemaRef"
    bLabelPrintln "| arg " s
  error $ "TODO flattenSchemaRef > additional case:"

flattenAttributeGroupRef ::
  Maybe QName -> Maybe QName -> [AttributeScheme] -> Maybe Line -> Maybe String
  -> XSDQ ([Definition], Reference)
flattenAttributeGroupRef n@(Just name) Nothing contents l d = do
  refs <- flattenAttributeGroupItem n Nothing contents l d
  return (refs, AttributeRef name Required)
flattenAttributeGroupRef Nothing (Just ref) [] _ln _d = do
  return ([], AttributeRef ref Required)
flattenAttributeGroupRef ifName ifRef contents _ln _d = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenAttributeGroupRef"
    bLabelPrintln "| IFNAME " ifName
    bLabelPrintln "| IFREF " ifRef
    bLabelPrintln "| CONTENTS " contents
  error $ "TODO flattenAttributeGroupRef > unmatched"

flattenSingleAttributeRef ::
  Maybe QName -> Maybe QName -> Maybe QName -> String
  -> Maybe Line -> Maybe String
  -> XSDQ ([Definition], Reference)
flattenSingleAttributeRef Nothing (Just ref) Nothing useStr _ _ = do
  let res = AttributeRef ref (stringToAttributeUsage useStr)
  return ([], res)
flattenSingleAttributeRef (Just nam) Nothing (Just t) m l d = do
  let defn = AttributeDefn nam
               (SingleAttributeDefn t $ stringToAttributeUsage m) l d
      ref = AttributeRef nam (stringToAttributeUsage m)
  return ([defn], ref)
flattenSingleAttributeRef maybeName maybeRef maybeType mode _ _ = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenSingleAttributeRef"
    bLabelPrintln "| IFNAME " maybeName
    bLabelPrintln "| IFREF " maybeRef
    bLabelPrintln "| IFTYPE " maybeType
    putStrLn $ "| MODE " ++ mode
  error "TODO flattenSingleAttributeRef > unmatched case"

flattenElementSchemeRef ::
  [DataScheme] -> Maybe QName -> Maybe QName -> Maybe QName
  -> Maybe Int -> Maybe Int -> Maybe Line -> Maybe String
  -> XSDQ ([Definition], Reference)
-- flattenElementSchemeRef contents ifName ifType ifRef ifLower ifUpper =
flattenElementSchemeRef [] Nothing Nothing (Just r) lower upper ln _ifDoc = do
  let result = ElementRef r lower upper ln
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening element schema with reference only"
    liftIO $ bLabelPrintln "    to " result
  return ([], result)
flattenElementSchemeRef [] (Just n)
                        (Just t@(QName resolvedName _resolvedURI _))
                        Nothing lo up ln ifDoc = do
  isKnown <- isKnownType t
  whenDebugging $ do
    liftIO $ putStrLn $
      "  - Checking whether " ++ resolvedName ++ " is known: " ++ show isKnown
  if isKnown then (do let defn = ElementDefn n t ln ifDoc
                          ref = ElementRef n lo up ln
                      fileNewDefinition defn
                      whenDebugging $ do
                        liftIO $ putStrLn $ "  > Flattening schema with type"
                        -- liftIO $ bLabelPrintln "       " e
                        liftIO $ bLabelPrintln "    to " defn
                        liftIO $ bLabelPrintln "       " ref
                      return ([defn], ref))
    else (do let defn1 = SimpleSynonymDefn n t ln ifDoc
                 defn2 = ElementDefn n n ln ifDoc
                 ref = ElementRef n lo up ln
             addTypeDefn n defn1
             fileNewDefinition defn2
             whenDebugging $ do
               liftIO $ putStrLn $
                 "  > Flattening element schema with name and type"
               -- liftIO $ bLabelPrintln "       " e
               liftIO $ bLabelPrintln "    to " defn1
               liftIO $ bLabelPrintln "       " defn2
               liftIO $ bLabelPrintln "       " ref
             return ([defn1, defn2], ref))
flattenElementSchemeRef s@[ComplexTypeScheme _ _ Nothing _ _]
                        n@(Just nam) t@Nothing r@Nothing lower upper ln ifDoc = do
  prev <- flattenElementSchemeItem s n t r lower upper ln ifDoc
  let ref = ElementRef nam lower upper ln
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening element schema with name and nested complex type"
    liftIO $ bLabelPrintln "       " s
    liftIO $ bLabelPrintln "    to " prev
    liftIO $ bLabelPrintln "       " ref
  return (prev, ref)
flattenElementSchemeRef ctnts maybeName maybeType maybeRef lower upper _ _ = do
  liftIO $ do
    bLabelPrintln "CONTENTS " ctnts
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
    putStrLn $ "LOWER " ++ show lower
    putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"

flattenAttributes :: [AttributeScheme] -> XSDQ [Definition]
flattenAttributes = fmap concat . mapM flattenAttribute

flattenAttribute :: AttributeScheme -> XSDQ [Definition]
flattenAttribute (SingleAttribute (Just n) Nothing (Just typ) mode d) =
  return [AttributeDefn n
            (SingleAttributeDefn typ $ stringToAttributeUsage mode)
            Nothing d]
flattenAttribute (AttributeGroup (Just n) Nothing schemes d) = do
  let names = map grabName schemes
  sub <- fmap concat $ mapM flattenAttribute schemes
  return $ sub ++ [AttributeDefn n (AttributeGroupDefn names) Nothing d]
flattenAttribute a = do
  liftIO $ putStrLn $ "+--------"
  liftIO $ bLabelPrintln "| flattenAttribute " a
  error "TODO flattenAttribute missing case"

