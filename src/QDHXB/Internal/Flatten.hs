
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
  liftIO $ do
    putStrLn $ "CONTENTS " ++ show ctnts
    putStrLn $ "IFNAME " ++ show maybeName
    putStrLn $ "IFTYPE " ++ show maybeType
    putStrLn $ "IFREF " ++ show maybeRef
    putStrLn $ "LOWER " ++ show lower
    putStrLn $ "UPPER " ++ show upper
  error "TODO flattenSchemaRef > unmatched ElementScheme"
flattenSchemaRef (AttributeScheme Nothing Nothing (Just ref) _) =
  return ([], AttributeItem ref)
flattenSchemaRef (AttributeScheme (Just nam) (Just typ) Nothing useStr) =
  return ([AttributeRep nam typ (stringToAttributeUsage useStr)],
          AttributeItem nam)
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
