
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
import QDHXB.Internal.NestedTypes
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Utils.Misc

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
flattenSchemaItem' (ElementScheme [] (Just nam) (Just typ) Nothing _ _) = do
  isKnown <- isKnownType typ
  isSimple <- isSimpleType typ
  whenDebugging $ do
    liftIO $ putStrLn $
      "  - Checking whether " ++ showQName typ ++ " is simple: " ++ show isSimple
  if isSimple || not isKnown
    then (do let tyDefn = SimpleSynonymDefn nam typ
             addTypeDefn nam tyDefn
             let elemDefn = ElementDefn nam nam
             fileNewDefinition elemDefn
             return [ tyDefn, elemDefn ])
    else return [ ElementDefn nam typ ]
flattenSchemaItem' (ElementScheme [SimpleTypeScheme Nothing ts]
                                  ifName@(Just nam) Nothing Nothing _ _) = do
  flatTS <- flattenSchemaItem' $ SimpleTypeScheme ifName ts
  let elemDefn = ElementDefn nam nam
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenSchemaItem' (ElementScheme [ComplexTypeScheme ts attrs Nothing]
                                  ifName@(Just nam) Nothing Nothing _ _) = do
  flatTS <- flattenSchemaItem' $ ComplexTypeScheme ts attrs ifName
  let elemDefn = ElementDefn nam nam
  fileNewDefinition elemDefn
  return $ flatTS ++ [elemDefn]
flattenSchemaItem' (AttributeScheme (Just nam) (Just typ) Nothing _) =
  let attrDefn = AttributeDefn nam typ
  in do fileNewDefinition attrDefn
        return [attrDefn]
flattenSchemaItem' (AttributeScheme _ _ (Just _) _) = do
  return $ error "Reference in attribute"
flattenSchemaItem' (ComplexTypeScheme (Sequence cts) ats (Just nam)) = do
  (defs, refs) <- musterComplexSequenceComponents cts ats nam
  let tyDefn = SequenceDefn (qName nam) $ refs
  addTypeDefn nam tyDefn
  whenDebugging $ do
    recheck <- isKnownType nam
    liftIO $ putStrLn $
      "  - Have set " ++ qName nam
      ++ " to be known; rechecked as " ++ show recheck
  return $ defs ++ [ tyDefn ]
flattenSchemaItem' (SimpleTypeScheme (Just nam) (Synonym base)) = do
  let tyDefn = SimpleSynonymDefn nam base
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here
flattenSchemaItem' (SimpleTypeScheme (Just nam) (SimpleRestriction base)) = do
  let tyDefn = SimpleSynonymDefn nam base
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
flattenSchemaItem' (SimpleTypeScheme (Just nam) (Union alts)) = do
  let makeUnionMinorLabel :: DataScheme -> Maybe QName
      makeUnionMinorLabel Skip =
        error "Should not see Skip in makeUnionMinorLabel"
      makeUnionMinorLabel (ElementScheme _ (Just name) _ _ _ _) = Just name
      makeUnionMinorLabel (ElementScheme _ (Just typ) _ _ _ _) = Just typ
      makeUnionMinorLabel (ElementScheme cs _ _ _ _ _) = makeFirst cs
        where makeFirst [] = Nothing
              makeFirst (d:ds) = case makeUnionMinorLabel d of
                                   Nothing -> makeFirst ds
                                   Just r -> Just r
      makeUnionMinorLabel (AttributeScheme j@(Just _) _ _ _) = j
      makeUnionMinorLabel (AttributeScheme _ j@(Just _) _ _) = j
      makeUnionMinorLabel (AttributeScheme _ _ j@(Just _) _) = j
      makeUnionMinorLabel (AttributeScheme _ _ _ _) = Nothing
      makeUnionMinorLabel (ComplexTypeScheme _ _ j@(Just _)) = j
      makeUnionMinorLabel (ComplexTypeScheme form _attrs _) = case form of
        Sequence ds -> Nothing
        ComplexRestriction r -> Just r
        Extension base ds -> Just base
        Choice base ds -> base
      makeUnionMinorLabel (SimpleTypeScheme name form) = case form of
        Synonym t -> Just t
        SimpleRestriction r -> Just r
        Union ds -> Nothing
      makeUnionMinorLabel (Group base n) = base

      nameUnnamed :: QName -> DataScheme -> DataScheme
      nameUnnamed q (ElementScheme ctnts Nothing ifType ifRef ifMin ifMax) =
        ElementScheme ctnts (Just q) ifType ifRef ifMin ifMax
      nameUnnamed q (AttributeScheme Nothing ifType ifRef usage) =
        AttributeScheme (Just q) ifType ifRef usage
      nameUnnamed q (ComplexTypeScheme form attrs Nothing) =
        ComplexTypeScheme form attrs (Just q)
      nameUnnamed q (SimpleTypeScheme Nothing detail) =
        SimpleTypeScheme (Just q) detail
      nameUnnamed _ b = b

      pullLabel :: DataScheme -> XSDQ (QName, [Definition])
      pullLabel d = do
        name <- case makeUnionMinorLabel d of
                  Just q -> return q
                  Nothing -> do
                    freshName <- getNextCapName
                    freshQName <- decodePrefixedName freshName
                    return freshQName
        defns <- flattenSchemaItem $ nameUnnamed name d
        return (name, defns)
  labelledAlts <- mapM pullLabel alts
  let (names, defnss) = unzip labelledAlts
      defns = concat defnss
  let uDef = UnionDefn nam names
  addTypeDefn nam uDef
  return $ defns ++ [uDef]
flattenSchemaItem' s = do
  liftIO $ putStrLn      "+------"
  liftIO $ putStrLn      "| TODO flattenSchemaItem' missed case"
  liftIO $ bLabelPrintln "| " s
  error $ show $ labelBlock "TODO another flatten case: " $ block s

musterComplexSequenceComponents ::
  [DataScheme] ->  [DataScheme] -> QName -> XSDQ ([Definition], [Reference])
musterComplexSequenceComponents steps ats _ = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return (otherDefs ++ atsDefs, atsRefs ++ refs)

flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef e@(ElementScheme [] Nothing Nothing (Just r) lower upper) = do
  let result = ElementRef r lower upper
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening element schema with reference only"
    liftIO $ bLabelPrintln "       " e
    liftIO $ bLabelPrintln "    to " result
  return ([], result)
flattenSchemaRef e@(ElementScheme [] (Just n) (Just t@(QName resolvedName _resolvedURI _)) Nothing lo up) = do
  isKnown <- isKnownType t
  whenDebugging $ do
    liftIO $ putStrLn $
      "  - Checking whether " ++ resolvedName ++ " is known: " ++ show isKnown
  if isKnown then (do let defn = ElementDefn n t
                          ref = ElementRef n lo up
                      fileNewDefinition defn
                      whenDebugging $ do
                        liftIO $ putStrLn $ "  > Flattening schema with type"
                        liftIO $ bLabelPrintln "       " e
                        liftIO $ bLabelPrintln "    to " defn
                        liftIO $ bLabelPrintln "       " ref
                      return ([defn], ref))
    else (do let defn1 = SimpleSynonymDefn n t
                 defn2 = ElementDefn n n
                 ref = ElementRef n lo up
             addTypeDefn n defn1
             fileNewDefinition defn2
             whenDebugging $ do
               liftIO $ putStrLn $
                 "  > Flattening element schema with name and type"
               liftIO $ bLabelPrintln "       " e
               liftIO $ bLabelPrintln "    to " defn1
               liftIO $ bLabelPrintln "       " defn2
               liftIO $ bLabelPrintln "       " ref
             return ([defn1, defn2], ref))
flattenSchemaRef s@(ElementScheme [ComplexTypeScheme _ _ Nothing]
                                  (Just nam) Nothing Nothing lower upper) = do
  prev <- flattenSchemaItem s
  let ref = ElementRef nam lower upper
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening element schema with name and nested complex type"
    liftIO $ bLabelPrintln "       " s
    liftIO $ bLabelPrintln "    to " prev
    liftIO $ bLabelPrintln "       " ref
  return (prev, ref)
flattenSchemaRef (ElementScheme ctnts maybeName maybeType maybeRef
                                lower upper) = do
  liftIO $ do
    bLabelPrintln "CONTENTS " ctnts
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
    putStrLn $ "LOWER " ++ show lower
    putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"
flattenSchemaRef sr@(AttributeScheme Nothing Nothing (Just ref) useStr) = do
  let res = AttributeRef ref (stringToAttributeUsage useStr)
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening attribute schema with reference only"
    liftIO $ bLabelPrintln "       " sr
    liftIO $ bLabelPrintln "    to " res
  return ([], res)
flattenSchemaRef s@(AttributeScheme (Just nam) (Just typ) Nothing useStr) = do
  let defn = AttributeDefn nam typ
      ref = AttributeRef nam (stringToAttributeUsage useStr)
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening attribute schema with name and type"
    liftIO $ bLabelPrintln "       " s
    liftIO $ bLabelPrintln "    to " defn
    liftIO $ bLabelPrintln "       " ref
  return ([defn], ref)
flattenSchemaRef (AttributeScheme maybeName maybeType maybeRef _) = do
  liftIO $ do
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
  error "TODO flattenSchemaRef > unmatched AttributeScheme"
flattenSchemaRef (ComplexTypeScheme _ _ _) = -- typeDetail _ maybeName
  error "TODO flattenSchemaRef > ComplexTypeScheme"
flattenSchemaRef s = do
  liftIO $ bLabelPrintln "S " s
  error $ "TODO flattenSchemaRef > additional case: " ++ show s
