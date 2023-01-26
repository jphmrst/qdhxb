
-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.Pipeline (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
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
  (e@(Elem (Element (QName "schema" _ _) _ _ _)) : []) -> elementToDecs e
  _ -> error "Expected top-level <schema> element"
xmlToDecs (e@(Elem (Element (QName "schema" _ _) _ _ _)) : []) =
  elementToDecs e
xmlToDecs _ = error "Missing <?xml> element"

elementToDecs :: Content -> XSDQ [Dec]
elementToDecs (Elem (Element (QName "schema" _ _) attrs forms _)) = do
  pushNamespaces attrs
  whenDebugging $ do
    liftIO $ putStrLn "======================================== INPUT"
  schemaReps <- encodeSchemaItems forms
  whenDebugging $ do
    liftIO $ putStrLn "======================================== FLATTEN"
  ir <- flattenSchemaItems schemaReps
  whenDebugging $ do
    liftIO $ putStrLn "======================================== GENERATE"
  decls <- xsdDeclsToHaskell ir
  whenDebugging $ do
    liftIO $ putStrLn "======================================== end"
  return decls
elementToDecs _ = error "Unexpected form in elementToDecs"
