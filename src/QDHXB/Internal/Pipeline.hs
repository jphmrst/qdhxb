{-# LANGUAGE ExplicitForAll #-}

-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.Pipeline (translateParsedXSD) where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
-- import Data.Char
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import QDHXB.Utils.BPP
import QDHXB.Utils.XMLLight (getCoreContent)
import QDHXB.Internal.XSDQ
import QDHXB.Internal.AST
import QDHXB.Internal.Generate

-- | Convert several parsed XSD files to a list of Haskell definitions
translateParsedXSD :: forall ast . AST ast =>
  (String -> [Content] -> XSDQ [ast]) -> [[Content]] -> XSDQ [Dec]
translateParsedXSD inputSchemaItems xsds = do
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

          schemaReps <- inputSchemaItems "Top" forms
          putLog $ " NESTED INPUT\n" ++ bpp schemaReps
            ++ "\n------------------------------ "
          whenDebugging $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " schemaReps

          whenDebugging $ liftIO $ putStrLn
              "======================================== RENAMED NESTED INPUT"
          renamedSchemaReps <- ensureUniqueNames schemaReps
          putLog $ " RENAMED NESTED INPUT\n" ++ bpp renamedSchemaReps
            ++ "\n------------------------------ "
          whenDebugging $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " renamedSchemaReps

          whenDebugging $ liftIO $ putStrLn
            "======================================== FLATTEN"
          ir <- flatten renamedSchemaReps
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
  putLog $ " FULL FLATTENED\n" ++ bpp flattened
    ++ "\n==============================\n"
  whenDebugging $ do
    debugXSDQ
    liftIO $ putStrLn "======================================== FULL FLATTENED"
    liftIO $ putStrLn $ bpp flattened
    liftIO $ putStrLn "----------------------------------------"
    debugXSDQ
    liftIO $ putStrLn "======================================== GENERATE"

  decls <- xsdDeclsToHaskell flattened
  putLog $ " OUTPUT\n" ++ bpp decls ++ "\n==============================\n"
  whenDebugging $ liftIO $ do
    bLabelPrintln "Final: " decls
    putStrLn "======================================== end "

  return decls
