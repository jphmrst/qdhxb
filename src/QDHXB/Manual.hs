{-# LANGUAGE TemplateHaskell #-}

-- | Manual translation of an XSD file into the internal @ItemDefn@
-- representation.
module QDHXB.Manual (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
import Text.XML.Light.Types
import QDHXB.Internal.Generate
import QDHXB.UtilMisc
import QDHXB.Internal.NestedTypes
import QDHXB.Internal.Input
import QDHXB.Internal.FlatTypes
import QDHXB.Internal.XSDQ

-- | Convert XML `Content` into a quotation monad returning top-level
-- Haskell declarations.
xmlToDecs :: [Content] -> XSDQ [Dec]
xmlToDecs ((Elem (Element (QName "?xml" _ _) _ _ _)) : ds) = case ds of
  (Elem (Element (QName "schema" _ _) _ forms _) : []) -> do
    schemaReps <- encodeSchemaItems forms
    -- liftIO $ putStrLn $ "====== REPS\n" ++ show schemaReps ++ "\n======\n"
    ir <- flattenSchemaItems schemaReps
    -- liftIO $ putStrLn $ "====== IR\n" ++ show ir ++ "\n======\n"
    decls <- xsdDeclsToHaskell ir
    return decls
  _ -> error "Expected top-level <schema> element"
xmlToDecs _ = error "Missing <?xml> element"

-- -----------------------------------------------------------------

flattenSchemaItems :: [SchemeRep] -> XSDQ [ItemDefn]
flattenSchemaItems = fmap concat . mapM flattenSchemaItem

flattenSchemaItem :: SchemeRep -> XSDQ [ItemDefn]
flattenSchemaItem (ElementScheme [] (Just nam) (Just typ) Nothing _ _) =
  return [ SimpleRep nam typ ]
flattenSchemaItem (ElementScheme [ComplexTypeScheme (Sequence steps)
                                                    ats Nothing]
                                 (Just nam) Nothing Nothing _ _) =
  assembleComplexSequence steps ats nam
flattenSchemaItem (AttributeScheme (Just nam) (Just typ) Nothing useStr) =
  return [ AttributeRep nam typ (stringToAttributeUsage useStr) ]
flattenSchemaItem (AttributeScheme _ _ (Just _) _) =
  return $ error "Reference in attribute"
flattenSchemaItem (ComplexTypeScheme (Sequence cts) ats (Just nam)) =
  assembleComplexSequence cts ats nam
flattenSchemaItem (SimpleTypeScheme base nam) =
  return $ [ SimpleRep nam base ]
flattenSchemaItem s = do
  liftIO $ putStrLn $ ">>> " ++ show s
  error "TODO another flatten case"

assembleComplexSequence ::
  [SchemeRep] ->  [SchemeRep] -> String -> XSDQ [ItemDefn]
assembleComplexSequence steps ats nam = do
  (otherDefs, refs) <- flattenSchemaRefs steps
  (atsDefs, atsRefs) <- flattenSchemaRefs ats
  return $ otherDefs ++ atsDefs ++ [ SequenceRep nam $ atsRefs ++ refs ]

flattenSchemaRefs :: [SchemeRep] -> XSDQ ([ItemDefn], [ItemRef])
flattenSchemaRefs = fmap (applyFst concat) . fmap unzip . mapM flattenSchemaRef

flattenSchemaRef :: SchemeRep -> XSDQ ([ItemDefn], ItemRef)
flattenSchemaRef (ElementScheme [] Nothing Nothing (Just ref) lower upper) =
  return ([], ElementItem ref lower upper)
flattenSchemaRef (ElementScheme [] (Just nam) (Just typ) Nothing lower upper) =
  return ([SimpleRep nam typ], ElementItem nam lower upper)
flattenSchemaRef s@(ElementScheme [ComplexTypeScheme _ _ Nothing]
                                  (Just nam) Nothing Nothing lower upper) = do
  prev <- flattenSchemaItem s
  return (prev, ElementItem nam lower upper)
flattenSchemaRef (ElementScheme ctnts maybeName maybeType maybeRef
                                lower upper) = do
  liftIO $ putStrLn $ "CONTENTS " ++ show ctnts
  liftIO $ putStrLn $ "IFNAME " ++ show maybeName
  liftIO $ putStrLn $ "IFTYPE " ++ show maybeType
  liftIO $ putStrLn $ "IFREF " ++ show maybeRef
  liftIO $ putStrLn $ "LOWER " ++ show lower
  liftIO $ putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"
flattenSchemaRef (AttributeScheme Nothing Nothing (Just ref) _) =
  return ([], AttributeItem ref)
flattenSchemaRef (AttributeScheme (Just nam) (Just typ) Nothing useStr) =
  return ([AttributeRep nam typ (stringToAttributeUsage useStr)],
          AttributeItem nam)
flattenSchemaRef (AttributeScheme maybeName maybeType maybeRef _) = do
  liftIO $ putStrLn $ "IFNAME " ++ show maybeName
  liftIO $ putStrLn $ "IFTYPE " ++ show maybeType
  liftIO $ putStrLn $ "IFREF " ++ show maybeRef
  error "TODO flattenSchemaRef > unmatched AttributeScheme"
flattenSchemaRef (ComplexTypeScheme _ _ _) = -- typeDetail _ maybeName
  error "TODO flattenSchemaRef > ComplexTypeScheme"
flattenSchemaRef s = do
  liftIO $ putStrLn $ "S " ++ show s
  error "TODO flattenSchemaRef > additional case"
