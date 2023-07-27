
-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.L0.Pipeline (translateParsedXSD) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
-- import Data.Char
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import QDHXB.Utils.BPP
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate
import QDHXB.Internal.L0.Input
import QDHXB.Internal.L0.Flatten

-- | Convert several parsed XSD files to a list of Haskell definitions
translateParsedXSD :: [[Content]] -> XSDQ [Dec]
translateParsedXSD xsds = do
  let cores = map getCoreContent xsds
  flatteneds <- mapM (
    \core ->
      case core of
        Elem (Element (QName "schema" _ _) attrs forms _) -> do
          pushNamespaces attrs
          whenDebugging $ liftIO $ do
            putStrLn "======================================== INPUT"
            bLabelPrintln "Source: " core
            putStrLn "----------------------------------------"
          putLog $ "------------------------------ SOURCE\n" ++ bpp core
            ++ "\n------------------------------ "

          schemaReps <- inputSchemaItems forms
          putLog $ " NESTED INPUT\n" ++ bpp schemaReps
            ++ "\n------------------------------ "
          whenDebugging $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " schemaReps
            putStrLn "======================================== FLATTEN"

          ir <- flattenSchemaItems schemaReps
          putLog $ " FLATTENED INPUT\n" ++ bpp ir
            ++ "\n------------------------------ "
          whenDebugging $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " ir

          return ir
        _ -> error "Expected top-level <schema> element") cores

  -- Now concatenate the flattened definition lists together, and
  -- convert them all to Haskell declarations.
  let flattened = concat flatteneds
  whenDebugging $ do
    debugXSDQ
    liftIO $ do
      putStrLn "======================================== GENERATE"
  decls <- xsdDeclsToHaskell flattened
  putLog $ " OUTPUT\n" ++ bpp decls ++ "\n==============================\n"
  whenDebugging $ liftIO $ do
    bLabelPrintln "Final: " decls
    putStrLn "======================================== end "

  return decls

-- | Find the core `Content` corresponding to an XML scheme in the
-- list of `Content` returned from parsing an XSD file.
getCoreContent :: [Content] -> Content
getCoreContent (e@(Elem (Element (QName "?xml" _ _) _ _ _)) : ds) = case ds of
  (e@(Elem (Element (QName "schema" _ _) _ _ _)) : []) -> e
  _ -> error "Expected top-level <schema> element"
getCoreContent (e@(Elem (Element (QName "schema" _ _) _ _ _)) : []) = e
getCoreContent _ = error "Missing <?xml> element"
