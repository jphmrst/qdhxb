
-- | Top-level calls
module QDHXB.Internal.L0.API (qdhxb, qdhxb') where

import Language.Haskell.TH (Q, Dec)
import System.IO
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.XML.Light.Types (Content(Elem))
import Text.XML.Light.Input (parseXML)
import QDHXB.Utils.XMLLight (isElem)
import QDHXB.Internal.XSDQ (
  XSDQ, runXSDQ, whenDebugging, whenResetLog,
  localLoggingStart, localLoggingEnd, putLog, resetLog, whenLogging)
import QDHXB.Internal.L0.Pipeline
import QDHXB.Options

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: QDHXBOption -> [String] -> Q [Dec]
qdhxb opts xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  runXSDQ opts $ do
    whenLogging $ \file -> do
      whenResetLog $ resetLog file
      liftIO $ appendFile file $ "Files: " ++ intercalate ", " xsds ++ "\n"
    xsdContents <- mapM loadContent xsds
    translateParsedXSD xsdContents

-- | Load and translate the given XSD files with the default options.
qdhxb' :: [String] -> Q [Dec]
qdhxb' = qdhxb id

loadContent :: String -> XSDQ [Content]
loadContent xsdFile = do
  localLoggingStart
  putLog $ "============================== " ++ xsdFile ++ "\n"
  xsd <- liftIO $ readFile' xsdFile
  let xml :: [Content]
      xml = parseXML xsd
  let res = filter isElem xml
  localLoggingEnd
  return res
