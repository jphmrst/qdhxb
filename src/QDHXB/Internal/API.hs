{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

-- | Top-level XSD-to-Haskell rewriting pipeline and API.
module QDHXB.Internal.API (apiFunctions, API) where

import Language.Haskell.TH (Q, Dec)
import System.IO
import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
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
            whenDebugging names 1 $ do
              st <- liftStatetoXSDQ $ get
              liftIO $ putStrLn $ bpp st
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
  nesteds <- mapM (
    \core ->
      case core of
        Elem (Element (QName "schema" _ _) attrs forms _) -> do
          pushNamespaces attrs
          liftIO $ setSGR [ SetColor Foreground Vivid Black ]
          whenDebugging input 0 $ liftIO $ do
            putStrLn "======================================== INPUT"
          whenDebugging input 4 $ liftIO $ do
            bLabelPrintln "Source: " core
            putStrLn "----------------------------------------"
          putLog $ "------------------------------ SOURCE\n" ++ bpp core
            ++ "\n------------------------------ "

          schemaReps <- (decodeXML forms :: XSDQ [ast])
          putLog $ " NESTED INPUT\n" ++ bpp schemaReps
            ++ "\n------------------------------ "
          whenDebugging input 0 $ liftIO $ do
            putStrLn "Final ----------------------------------------"
            putStrLn $ bpp schemaReps
          checkBreakAfterInput

          return schemaReps
          {-
          whenDebugging unique 0 $ do
            liftIO $ putStrLn
              "======================================== RENAMED NESTED INPUT"
            debugXSDQ
          renamedSchemaReps <- ensureUniqueNames schemaReps
          putLog $ " RENAMED NESTED INPUT\n" ++ bpp renamedSchemaReps
            ++ "\n------------------------------ "
          whenDebugging unique 0 $ do
            debugXSDQ
            liftIO $ do
              putStrLn "Final ----------------------------------------"
              putStrLn $ bpp renamedSchemaReps
          checkBreakAfterUnique

          whenDebugging flattening 0 $ liftIO $ putStrLn
            "======================================== FLATTEN"
          ir <- flatten renamedSchemaReps
          putLog $ " FLATTENED INPUT\n" ++ bpp ir
            ++ "\n------------------------------ "
          whenDebugging flattening 0 $ liftIO $ do
            putStrLn "Final ----------------------------------------"
            putStrLn $ bpp ir
          checkBreakAfterFlatten

          return ir
          -}
        _ -> error "Expected top-level <schema> element") cores

  -- Now concatenate the flattened definition lists together, and
  -- convert them all to Haskell declarations.
  checkBreakAfterAllInput
  let nested = concat nesteds

  whenDebugging unique 0 $ do
    liftIO $ putStrLn
      "======================================== RENAMED NESTED INPUT"
    debugXSDQ
  renamedSchemaReps <- ensureUniqueNames nested
  putLog $ " RENAMED NESTED INPUT\n" ++ bpp renamedSchemaReps
    ++ "\n------------------------------ "
  whenDebugging unique 0 $ do
    debugXSDQ
    liftIO $ do
      putStrLn "Final ----------------------------------------"
      putStrLn $ bpp renamedSchemaReps
  checkBreakAfterUnique

  whenDebugging flattening 0 $ liftIO $ putStrLn
    "======================================== FLATTEN"
  flattened <- flatten renamedSchemaReps
  putLog $ " FLATTENED INPUT\n" ++ bpp flattened
    ++ "\n------------------------------ "
  whenDebugging flattening 0 $ liftIO $ do
    putStrLn "Final ----------------------------------------"
    putStrLn $ bpp flattened
  checkBreakAfterFlatten

  putLog $ " FULL FLATTENED\n" ++ bpp flattened
    ++ "\n==============================\n"
  whenDebugging flattening 0 $ do
    debugXSDQ
    liftIO $ do
      putStrLn "======================================== FULL FLATTENED"
      putStrLn $ bpp flattened
      putStrLn "----------------------------------------"
    debugXSDQ

  whenDebugging generate 0 $
    liftIO $ putStrLn "======================================== GENERATE"
  whenDebugging names 0 $ debugXSDQ
  whenDebugging generate 2 $ do
    debugXSDQ
    liftIO $ putStrLn "----------------------------------------"
  decls <- xsdDeclsToHaskell flattened
  putLog $ " OUTPUT\n" ++ bpp decls ++ "\n==============================\n"
  whenDebugging generate 0 $ liftIO $ do
    putStrLn "Final ----------------------------------------"
    putStrLn $ bpp decls
    putStrLn "======================================== end "

  return decls
