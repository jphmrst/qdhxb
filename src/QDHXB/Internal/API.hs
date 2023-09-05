{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

-- | Top-level XSD-to-Haskell rewriting pipeline and API.
module QDHXB.Internal.API (apiFunctions, API) where

import Language.Haskell.TH (Q, Dec)
import System.IO
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import Text.XML.Light.Input (parseXML)
import QDHXB.Options
import QDHXB.Utils.BPP
import QDHXB.Utils.XMLLight (getCoreContent, isElem)
import QDHXB.Internal.AST
import QDHXB.Internal.Generate
import QDHXB.Internal.XSDQ

import QDHXB.Internal.Debugln

-- | Shorthand for the two top-level functions based on a given XSD
-- `AST` implementation.
type API = (QDHXBOption -> [String] -> Q [Dec], [String] -> Q [Dec])

-- | Define the two API functions (with and without options) for a
-- given XSD `AST` implementation.
apiFunctions :: forall ast . AST ast => API
apiFunctions = (qdhxbFn, qdhxbFn')
  where -- | Load the given XSD files, translating each into Haskell
        -- declarations.
        qdhxbFn :: QDHXBOption -> [String] -> Q [Dec]
        qdhxbFn opts xsds = do
          -- liftIO (getCurrentDirectory >>= putStrLn . show)
          runXSDQ opts $ do
            whenLogging $ \file -> do
              whenResetLog $ resetLog file
              liftIO $ appendFile file $
                "Files: " ++ intercalate ", " xsds ++ "\n"
            xsdContents <- mapM load_content xsds
            translate_parsed_xsd @ast xsdContents

        -- | Load and translate the given XSD files with the default
        -- options.
        qdhxbFn' :: [String] -> Q [Dec]
        qdhxbFn' = qdhxbFn id

load_content :: String -> XSDQ [Content]
load_content xsdFile = do
  localLoggingStart
  putLog $ "============================== " ++ xsdFile ++ "\n"
  xsd <- liftIO $ readFile' xsdFile
  let xml :: [Content]
      xml = parseXML xsd
  let res = filter isElem xml
  localLoggingEnd
  return res

-- | Convert several parsed XSD files to a list of Haskell definitions
translate_parsed_xsd :: forall ast . AST ast => [[Content]] -> XSDQ [Dec]
translate_parsed_xsd xsds = do
  let cores = map getCoreContent xsds
  flatteneds <- mapM (
    \core ->
      case core of
        Elem (Element (QName "schema" _ _) attrs forms _) -> do
          pushNamespaces attrs
          whenDebugging input 0 $ liftIO $ do
            putStrLn "======================================== INPUT"
            bLabelPrintln "Source: " core
            putStrLn "----------------------------------------"
          putLog $ "------------------------------ SOURCE\n" ++ bpp core
            ++ "\n------------------------------ "

          schemaReps <- (decodeXML forms :: XSDQ [ast])
          putLog $ " NESTED INPUT\n" ++ bpp schemaReps
            ++ "\n------------------------------ "
          whenDebugging input 0 $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " schemaReps

          whenDebugging unique 0 $ liftIO $ putStrLn
              "======================================== RENAMED NESTED INPUT"
          renamedSchemaReps <- ensureUniqueNames schemaReps
          putLog $ " RENAMED NESTED INPUT\n" ++ bpp renamedSchemaReps
            ++ "\n------------------------------ "
          whenDebugging unique 0 $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " renamedSchemaReps

          whenDebugging flattening 0 $ liftIO $ putStrLn
            "======================================== FLATTEN"
          ir <- flatten renamedSchemaReps
          putLog $ " FLATTENED INPUT\n" ++ bpp ir
            ++ "\n------------------------------ "
          whenDebugging flattening 0 $ liftIO $ do
            putStrLn "----------------------------------------"
            bLabelPrintln "Final: " ir

          return ir
        _ -> error "Expected top-level <schema> element") cores

  -- Now concatenate the flattened definition lists together, and
  -- convert them all to Haskell declarations.
  let flattened = concat flatteneds
  putLog $ " FULL FLATTENED\n" ++ bpp flattened
    ++ "\n==============================\n"
  whenDebugging flattening 0 $ do
    debugXSDQ
    liftIO $ putStrLn "======================================== FULL FLATTENED"
    liftIO $ putStrLn $ bpp flattened
    liftIO $ putStrLn "----------------------------------------"
    debugXSDQ

  whenDebugging generate 0 $ do
    liftIO $ putStrLn "======================================== GENERATE"
  decls <- xsdDeclsToHaskell flattened
  putLog $ " OUTPUT\n" ++ bpp decls ++ "\n==============================\n"
  whenDebugging generate 0 $ liftIO $ do
    bLabelPrintln "Final: " decls
    putStrLn "======================================== end "

  return decls
