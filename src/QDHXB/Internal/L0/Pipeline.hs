
-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.L0.Pipeline (xmlToDecs) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
-- import Data.Char
-- import Text.XML.Light.Types
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import QDHXB.Utils.BPP
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate
import QDHXB.Internal.L0.Input
import QDHXB.Internal.L0.Flatten

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
elementToDecs e@(Elem (Element (QName "schema" _ _) attrs forms _)) = do
  pushNamespaces attrs
  whenDebugging $ liftIO $ do
    putStrLn "======================================== INPUT"
    bLabelPrintln "Source: " e
    putStrLn "----------------------------------------"
  putLog $ "------------------------------ SOURCE\n" ++ bpp e
    ++ "\n------------------------------ "

  schemaReps <- inputSchemaItems forms
  whenDebugging $ liftIO $ do
    putStrLn "----------------------------------------"
    bLabelPrintln "Final: " schemaReps
    putStrLn "======================================== FLATTEN"
  putLog $ " NESTED INPUT\n" ++ bpp schemaReps
    ++ "\n------------------------------ "

  ir <- flattenSchemaItems schemaReps
  whenDebugging $ do
    liftIO $ do
      putStrLn "----------------------------------------"
      bLabelPrintln "Final: " ir
      putStrLn "======================================== GENERATE"
    debugXSDQ
    liftIO $ do
      putStrLn "----------------------------------------"
  putLog $ " FLATTENED INPUT\n" ++ bpp ir
    ++ "\n------------------------------ "

  decls <- xsdDeclsToHaskell ir
  whenDebugging $ liftIO $ do
    putStrLn "----------------------------------------"
    bLabelPrintln "Final: " decls
    putStrLn "======================================== end "
  putLog $ " OUTPUT\n" ++ bpp decls ++ "\n==============================\n"

  return decls

elementToDecs _ = error "Unexpected form in elementToDecs"
