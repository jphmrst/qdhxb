
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
    liftIO $ putStrLn $ "   `--> " ++ pprintDefns' "        " results
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
flattenSchemaItem' (ElementScheme [ComplexTypeScheme (Sequence steps)
                                                    ats Nothing]
                                 (Just nam) Nothing Nothing _ _) = do
  (defs, refs) <- musterComplexSequenceComponents steps ats nam
  let typName = qName nam
      tyDefn = SequenceDefn typName $ refs
  uri <- getDefaultNamespace
  pfx <- mapM getURIprefix uri
  let typQName = QName typName uri $ compressMaybe pfx
  addTypeDefn typQName tyDefn
  whenDebugging $ do
    recheck <- isKnownType typQName
    liftIO $ putStrLn $
      "  - Have set " ++ typName ++ " to be known; rechecked as "
      ++ show recheck
  let elemDefn = ElementDefn nam typQName
  fileNewDefinition elemDefn
  return $ defs ++ [tyDefn, elemDefn]
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
flattenSchemaItem' (SimpleTypeScheme nam (Synonym base)) = do
  let tyDefn = SimpleSynonymDefn nam base
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
-- TODO Insert cases of SimpleRestriction that we /can/ handle in the
-- types here
flattenSchemaItem' (SimpleTypeScheme nam (SimpleRestriction base)) = do
  let tyDefn = SimpleSynonymDefn nam base
  addTypeDefn nam tyDefn
  return $ [ tyDefn ]
flattenSchemaItem' s = do
  liftIO $ bLabelPrintln ">>> " s
  error $ "TODO another flatten case:\n" ++ bpp s

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
