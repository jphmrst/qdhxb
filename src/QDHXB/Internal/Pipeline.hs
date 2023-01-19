
-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.Pipeline (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
-- import Control.Monad.IO.Class
-- import Data.Char
-- import Text.XML.Light.Types
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate
import QDHXB.Internal.Input
import QDHXB.Internal.Flatten

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
