
-- | Top-level calls
module QDHXB.Internal.API (apiFunctions, API) where

import Language.Haskell.TH (Q, Dec)
import System.IO
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.XML.Light.Types (Content(Elem))
import Text.XML.Light.Input (parseXML)
import QDHXB.Utils.XMLLight (isElem)
import QDHXB.Internal.XSDQ (
  XSDQ, runXSDQ, whenResetLog,
  localLoggingStart, localLoggingEnd, putLog, resetLog, whenLogging)
import QDHXB.Internal.AST
import QDHXB.Internal.Pipeline
import QDHXB.Internal.Types
import QDHXB.Options

-- | Shorthand for the two top-level functions based on a given XSD
-- `AST` implementation.
type API = (QDHXBOption -> [String] -> Q [Dec], [String] -> Q [Dec])

-- | Define the two API functions (with and without options) for a
-- given XSD `AST` implementation.
apiFunctions :: AST ast => (String -> [Content] -> XSDQ [ast]) -> API
apiFunctions inputSchemaItems = (qdhxbFn, qdhxbFn')
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
            translateParsedXSD inputSchemaItems xsdContents

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
