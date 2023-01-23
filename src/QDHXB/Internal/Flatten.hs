
-- | Internal representation directly reflecting XSD code, in
-- particular allowing nested definitions.
module QDHXB.Internal.Flatten (flattenSchemaItems) where

-- import System.Directory
import Control.Monad.IO.Class
import QDHXB.Internal.Generate
import QDHXB.UtilMisc
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
  whenDebugging $ do
    liftIO $ putStrLn $ "> Flattening " ++ formatDataScheme' "             " s
  results <- flattenSchemaItem' s
  whenDebugging $ do
    liftIO $ putStrLn $ "   `--> " ++ pprintDefns' "        " results
  return results

flattenSchemaItem' :: DataScheme -> XSDQ [Definition]
flattenSchemaItem' (ElementScheme [] (Just nam) (Just typ) Nothing _ _) = do
  let typName = (case nam of
                   'x':'s':':':nam' -> nam'
                   _ -> nam) ++ "Type_"
  return [
    SimpleTypeDefn typName typ,
    ElementDefn nam typName
    ]
flattenSchemaItem' (ElementScheme [ComplexTypeScheme (Sequence steps)
                                                    ats Nothing]
                                 (Just nam) Nothing Nothing _ _) = do
  assembleComplexSequence steps ats nam
flattenSchemaItem' (AttributeScheme (Just nam) (Just typ) Nothing _) = do
  return [ AttributeDefn nam typ ]
flattenSchemaItem' (AttributeScheme _ _ (Just _) _) = do
  return $ error "Reference in attribute"
flattenSchemaItem' (ComplexTypeScheme (Sequence cts) ats (Just nam)) = do
  assembleComplexSequence cts ats nam
flattenSchemaItem' (SimpleTypeScheme base nam) = do
  return $ [ SimpleTypeDefn nam base ]
flattenSchemaItem' s = do
  liftIO $ putStrLn $ ">>> " ++ show s
  error "TODO another flatten case"

assembleComplexSequence ::
  [DataScheme] ->  [DataScheme] -> String -> XSDQ [Definition]
assembleComplexSequence steps ats nam = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  let typName = nam ++ "Type_"
  return $ otherDefs ++ atsDefs ++ [
    SequenceDefn typName $ atsRefs ++ refs,
    ElementDefn nam typName
    ]

flattenSchemaRefs :: [DataScheme] -> XSDQ ([Definition], [Reference])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: DataScheme -> XSDQ ([Definition], Reference)
flattenSchemaRef e@(ElementScheme [] Nothing Nothing (Just r) lower upper) = do
  let result = ElementRef r lower upper
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening schema ref"
    liftIO $ putStrLn $ "       " ++ show e
    liftIO $ putStrLn $ "    to " ++ show result
  return ([], result)
flattenSchemaRef e@(ElementScheme [] (Just n) (Just t) Nothing lo up) = do
  let intermed = n ++ "Type_"
      defn1 = SimpleTypeDefn intermed t
      defn2 = ElementDefn n intermed
      ref = ElementRef n lo up
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening schema ref"
    liftIO $ putStrLn $ "       " ++ show e
    liftIO $ putStrLn $ "    to " ++ show defn1
    liftIO $ putStrLn $ "       " ++ show defn2
    liftIO $ putStrLn $ "       " ++ show ref
  return ([defn1, defn2], ref)
flattenSchemaRef s@(ElementScheme [ComplexTypeScheme _ _ Nothing]
                                  (Just nam) Nothing Nothing lower upper) = do
  prev <- flattenSchemaItem s
  let ref = ElementRef nam lower upper
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening schema ref"
    liftIO $ putStrLn $ "       " ++ show s
    liftIO $ putStrLn $ "    to " ++ show prev
    liftIO $ putStrLn $ "       " ++ show ref
  return (prev, ref)
flattenSchemaRef (ElementScheme ctnts maybeName maybeType maybeRef
                                lower upper) = do
  liftIO $ do
    putStrLn $ "CONTENTS " ++ show ctnts
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
    putStrLn $ "LOWER " ++ show lower
    putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"
flattenSchemaRef sr@(AttributeScheme Nothing Nothing (Just ref) useStr) = do
  let res = AttributeRef ref (stringToAttributeUsage useStr)
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening schema ref"
    liftIO $ putStrLn $ "       " ++ show sr
    liftIO $ putStrLn $ "    to " ++ show res
  return ([], res)
flattenSchemaRef s@(AttributeScheme (Just nam) (Just typ) Nothing useStr) = do
  let defn = AttributeDefn nam typ
      ref = AttributeRef nam (stringToAttributeUsage useStr)
  whenDebugging $ do
    liftIO $ putStrLn $ "  > Flattening schema ref"
    liftIO $ putStrLn $ "       " ++ show s
    liftIO $ putStrLn $ "    to " ++ show defn
    liftIO $ putStrLn $ "       " ++ show ref
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
  liftIO $ putStrLn $ "S " ++ show s
  error "TODO flattenSchemaRef > additional case"
