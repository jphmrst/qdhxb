{-# LANGUAGE TemplateHaskell #-}

-- | Manual translation of an XSD file into the internal @ItemDefn@
-- representation.
module QDHXB.Manual (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Text.XML.Light.Types
import QDHXB.Internal.Generate
import QDHXB.Internal.Input
import QDHXB.Internal.Flatten
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
