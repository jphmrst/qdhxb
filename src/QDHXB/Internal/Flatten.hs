
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
  whenDebugging $ liftIO $ bLabelPrintln "> Flattening " s
  results <- flattenSchemaItem' s
  whenDebugging $ liftIO $ bLabelPrintln "   `--> " results
  return results

flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
flattenSchemaItem' Skip = return []
flattenSchemaItem' (ElementScheme contents ifName ifType ifRef ifMin ifMax) =
  flattenElementScheme contents ifName ifType ifRef ifMin ifMax
flattenSchemaItem' (AttributeScheme as) = flattenAttributeScheme as
flattenSchemaItem' (ComplexTypeScheme typeDetail addlAttrs ifName) =
  flattenComplexTypeScheme typeDetail addlAttrs ifName
flattenSchemaItem' (SimpleTypeScheme ifName scheme) =
  flattenSimpleTypeScheme ifName scheme
flattenSchemaItem' s = do
  liftIO $ do
    putStrLn      "+------"
    putStrLn      "| TODO flattenSchemaItem' missed case"
    bLabelPrintln "| " s
  error $ show $ labelBlock "TODO another flatten case: " $ block s

musterComplexSequenceComponents ::
  [DataScheme] ->  [DataScheme] -> QName -> XSDQ ([Definition], [Reference])
musterComplexSequenceComponents steps ats _ = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return (otherDefs ++ atsDefs, atsRefs ++ refs)

grabName :: AttributeScheme -> QName
grabName (SingleAttribute (Just n) _ _ _) = n
grabName (SingleAttribute Nothing (Just n) _ _) = n
grabName (SingleAttribute Nothing Nothing (Just t) _) = t
grabName (AttributeGroup (Just n) _ _) = n
grabName (AttributeGroup Nothing (Just n) _) = n
grabName a = error $ "No useable name in " ++ show a

flattenElementScheme ::
  [DataScheme] -> (Maybe QName) -> (Maybe QName) -> (Maybe QName) -> (Maybe Int) -> (Maybe Int)
    -> XSDQ [Definition]
flattenElementScheme [] (Just nam) (Just typ) Nothing _ _ = do
  isKnown <- isKnownType typ
  isSimple <- isSimpleType typ
  whenDebugging $ liftIO $ do
    putStrLn $ "- Is " ++ showQName typ ++ " known: " ++ show isKnown
    putStrLn $ "- Is " ++ showQName typ ++ " simple: " ++ show isSimple
  if isSimple || not isKnown
    then (do let tyDefn = SimpleSynonymDefn nam typ
             fileNewDefinition tyDefn
             let elemDefn = ElementDefn nam nam
             fileNewDefinition elemDefn
             return [ tyDefn, elemDefn ])
    else do let elemDefn = ElementDefn nam typ
            fileNewDefinition elemDefn
            return [ elemDefn ]
flattenElementScheme [SimpleTypeScheme Nothing ts]
                                  ifName@(Just nam) Nothing Nothing _ _ = do
  flatTS <- flattenSchemaItem' $ SimpleTypeScheme ifName ts
  let elemDefn = ElementDefn nam nam
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenElementScheme [ComplexTypeScheme ts attrs Nothing]
                                  ifName@(Just nam) Nothing Nothing _ _ = do
  flatTS <- flattenSchemaItem' $ ComplexTypeScheme ts attrs ifName
  let elemDefn = ElementDefn nam nam
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenElementScheme contents ifName ifType ifRef ifMin ifMax = liftIO $ do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenElementScheme unmatched case"
    bLabelPrintln "| CONTENTS " contents
    bLabelPrintln "| IFNAME " ifName
    bLabelPrintln "| IFTYPE " ifType
    bLabelPrintln "| IFREF " ifRef
    putStrLn $ "| IFMIN " ++ show ifMin
    putStrLn $ "| IFMAX " ++ show ifMax
  error "*** UNMATCHED CASE --- flattenElementScheme ***"


flattenAttributeScheme :: AttributeScheme -> XSDQ [Definition]
flattenAttributeScheme (SingleAttribute (Just nam) Nothing (Just typ) m) = do
  let attrDefn = AttributeDefn nam $ SingleAttributeDefn typ m
  fileNewDefinition attrDefn
  return [attrDefn]
flattenAttributeScheme (SingleAttribute _ (Just _) _ _) = do
  return $ error "Reference in attribute"
flattenAttributeScheme g@(AttributeGroup (Just n) Nothing cs) = do
  let names = map grabName cs
  defs <- flattenAttributes cs
  let attrDefn = AttributeDefn n $ AttributeGroupDefn names
  fileNewDefinition attrDefn
  let res = defs ++ [attrDefn]
  whenDebugging $ do
    liftIO $ bLabelPrintln "> flattenAttributeScheme " g
    liftIO $ bLabelPrintln "    '--> " res
  return res
  -- error $ show $ labelBlock "TODO flatten AttributeScheme: " $ block g

flattenAttributes :: [AttributeScheme] -> XSDQ [Definition]
flattenAttributes = fmap concat . mapM flattenAttribute

flattenAttribute :: AttributeScheme -> XSDQ [Definition]
flattenAttribute (SingleAttribute (Just n) Nothing (Just typ) mode) =
  return [AttributeDefn n $ SingleAttributeDefn typ mode]
flattenAttribute (AttributeGroup (Just n) Nothing schemes) = do
  let names = map grabName schemes
  sub <- fmap concat $ mapM flattenAttribute schemes
  return $ sub ++ [AttributeDefn n $ AttributeGroupDefn names]
flattenAttribute a = do
  liftIO $ putStrLn $ "+--------"
  liftIO $ bLabelPrintln "| flattenAttribute " a
  error "TODO flattenAttribute missing case"

flattenComplexTypeScheme ::
  ComplexTypeScheme -> [DataScheme] -> (Maybe QName) -> XSDQ [Definition]
flattenComplexTypeScheme (Sequence cts) ats (Just nam) = do
  (defs, refs) <- musterComplexSequenceComponents cts ats nam
  let tyDefn = SequenceDefn nam refs []
  fileNewDefinition tyDefn
  whenDebugging $ do
    recheck <- isKnownType nam
    liftIO $ putStrLn $
      "  - Have set " ++ qName nam
      ++ " to be known; rechecked as " ++ show recheck
  return $ defs ++ [ tyDefn ]
flattenComplexTypeScheme (ComplexSynonym s) _ (Just nam) = do
  liftIO $ do
    putStrLn      "********"
    putStrLn      "* TODO flattenComplexTypeScheme ComplexSynonym"
    bLabelPrintln "* " s
    bLabelPrintln "* " nam
  error $ show $ labelBlock "TODO flatten ComplexSynonym: " $ block s

flattenSimpleTypeScheme ::
  (Maybe QName) -> SimpleTypeScheme -> XSDQ [Definition]
flattenSimpleTypeScheme (Just nam) (SimpleSynonym base) = do
  let tyDefn = SimpleSynonymDefn nam base
  fileNewDefinition tyDefn
  return $ [ tyDefn ]
-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here
flattenSimpleTypeScheme (Just nam) (SimpleRestriction base) = do
  let tyDefn = SimpleSynonymDefn nam base
  fileNewDefinition tyDefn
  return $ [ tyDefn ]
flattenSimpleTypeScheme (Just nam) (Union alts) = do
  let nameUnnamed :: QName -> DataScheme -> DataScheme
      nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifMin ifMax) =
        ElementScheme ctnts (Just q) ifType ifRef ifMin ifMax
      nameUnnamed q (AttributeScheme
                     (SingleAttribute Nothing ifRef ifType usage)) =
        AttributeScheme (SingleAttribute (Just q) ifRef ifType usage)
      nameUnnamed q (ComplexTypeScheme form attrs Nothing) =
        ComplexTypeScheme form attrs (Just q)
      nameUnnamed q (SimpleTypeScheme Nothing detail) =
        SimpleTypeScheme (Just q) detail
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
  let uDef = UnionDefn nam names
  fileNewDefinition uDef
  return $ defns ++ [uDef]
flattenSimpleTypeScheme (Just nam) (List (Just elem)) = do
  let lDef = ListDefn nam elem
  fileNewDefinition lDef
  return [lDef]

-- =================================================================

flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef (ElementScheme contents ifName ifType ifRef ifMin ifMax) =
  flattenElementSchemeRef contents ifName ifType ifRef ifMin ifMax
flattenSchemaRef (AttributeScheme (SingleAttribute ifName ifRef ifType use)) =
  flattenSingleAttributeRef ifName ifRef ifType use
flattenSchemaRef (AttributeScheme (AttributeGroup ifName ifRef attrs)) =
  flattenAttributeGroupRef ifName ifRef attrs
flattenSchemaRef (ComplexTypeScheme _ _ _) = -- typeDetail _ maybeName
  error "TODO flattenSchemaRef > ComplexTypeScheme"
flattenSchemaRef s = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenSchemaRef missed case"
    bLabelPrintln "| " s
  error $ "TODO flattenSchemaRef > additional case:\n" ++ bpp s

flattenElementSchemeRef ::
  [DataScheme] -> (Maybe QName) -> (Maybe QName) ->
    (Maybe QName) -> (Maybe Int) -> (Maybe Int) ->
      XSDQ ([Definition], Reference)
flattenElementSchemeRef [] Nothing Nothing (Just r) lower upper = do
  let result = ElementRef r lower upper
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening element schema with reference only"
    liftIO $ bLabelPrintln "    to " result
  return ([], result)
flattenElementSchemeRef [] (Just n) (Just t@(QName resolvedName _resolvedURI _)) Nothing lo up = do
  isKnown <- isKnownType t
  whenDebugging $ do
    liftIO $ putStrLn $
      "  - Checking whether " ++ resolvedName ++ " is known: " ++ show isKnown
  if isKnown then (do let defn = ElementDefn n t
                          ref = ElementRef n lo up
                      fileNewDefinition defn
                      whenDebugging $ do
                        liftIO $ putStrLn $ "  > Flattening schema with type"
                        liftIO $ bLabelPrintln "    to " defn
                        liftIO $ bLabelPrintln "       " ref
                      return ([defn], ref))
    else (do let defn1 = SimpleSynonymDefn n t
                 defn2 = ElementDefn n n
                 ref = ElementRef n lo up
             fileNewDefinition defn1
             fileNewDefinition defn2
             whenDebugging $ do
               liftIO $ putStrLn $
                 "  > Flattening element schema with name and type"
               liftIO $ bLabelPrintln "    to " defn1
               liftIO $ bLabelPrintln "       " defn2
               liftIO $ bLabelPrintln "       " ref
             return ([defn1, defn2], ref))
flattenElementSchemeRef ds@[ComplexTypeScheme _ _ Nothing]
                        ifn@(Just nam) Nothing Nothing lower upper = do
  prev <- flattenElementScheme ds ifn Nothing Nothing lower upper
  let ref = ElementRef nam lower upper
  whenDebugging $ liftIO $ do
    putStrLn $ "  > Flattening element schema with name and nested complex type"
    bLabelPrintln "       " ds
    bLabelPrintln "       " ifn
    putStrLn $ "       " ++ show lower
    putStrLn $ "       " ++ show upper
    bLabelPrintln "    to " prev
    bLabelPrintln "       " ref
  return (prev, ref)
flattenElementSchemeRef ctnts maybeName maybeType maybeRef lower upper = do
  liftIO $ do
    bLabelPrintln "CONTENTS " ctnts
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
    putStrLn $ "LOWER " ++ show lower
    putStrLn $ "UPPER " ++ show upper
  error "TODO flattenElementSchemeRef > unmatched ElementScheme"

flattenSingleAttributeRef ::
  (Maybe QName) -> (Maybe QName) -> (Maybe QName) -> String ->
    XSDQ ([Definition], Reference)
flattenSingleAttributeRef Nothing (Just ref) Nothing useStr = do
  let res = AttributeRef ref (stringToAttributeUsage useStr)
  whenDebugging $ liftIO $ do
    putStrLn $ "  > Flattening attribute schema with reference only"
    bLabelPrintln "         ref " ref
    putStrLn $ "      useStr " ++ useStr
    bLabelPrintln "   --> " res
  return ([], res)
flattenSingleAttributeRef (Just nam) Nothing (Just t) m = do
  let defn = AttributeDefn nam $ SingleAttributeDefn t m
      ref = AttributeRef nam (stringToAttributeUsage m)
  whenDebugging $ liftIO $ do
    putStrLn $ "  > Flattening attribute schema with name and type"
    bLabelPrintln "       " nam
    bLabelPrintln "       " t
    putStrLn $ "       " ++ m
    bLabelPrintln "    to " defn
    bLabelPrintln "       " ref
  return ([defn], ref)
flattenSingleAttributeRef maybeName maybeRef maybeType _ = do
  liftIO $ do
    bLabelPrintln "IFNAME " maybeName
    bLabelPrintln "IFREF " maybeRef
    bLabelPrintln "IFTYPE " maybeType
  error "TODO flattenSchemaRef > unmatched AttributeScheme"

flattenAttributeGroupRef ::
  (Maybe QName) -> (Maybe QName) -> [AttributeScheme] ->
    XSDQ ([Definition], Reference)
flattenAttributeGroupRef ifName ifRef groups = do
  liftIO $ do
    putStrLn "+--------"
    putStrLn "| flattenAttributeGroupRef missed case"
    bLabelPrintln "| NAME? " ifName
    bLabelPrintln "| REF? " ifRef
    bLabelPrintln "| GROUPS " groups
  error "TODO flattenAttributeGroupRef > additional case:\n"
