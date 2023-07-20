
-- | Top-level calls
module QDHXB.Internal.L0.API (qdhxb, qdhxb') where

import Language.Haskell.TH (Q, Dec)
import System.IO
import Control.Monad.IO.Class
import Data.List (intercalate)
import Text.XML.Light.Input (parseXML)
import QDHXB.Utils.XMLLight (isElem)
import QDHXB.Internal.XSDQ (
  XSDQ, runXSDQ, whenDebugging, whenCentralLogging, whenResetLog,
  localLoggingStart, localLoggingEnd, putLog, resetLog)
import QDHXB.Internal.L0.Pipeline
import QDHXB.Options

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: QDHXBOption -> [String] -> Q [Dec]
qdhxb opts xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  runXSDQ opts $ do
    whenCentralLogging $ \file -> do
      whenResetLog $ do
        resetLog file
        liftIO $ appendFile file $ "Files: " ++ intercalate ", " xsds ++ "\n"
    fmap concat $ mapM loadFile xsds

-- | Load and translate the given XSD files with the default options.
qdhxb' :: [String] -> Q [Dec]
qdhxb' = qdhxb id

loadFile :: String -> XSDQ [Dec]
loadFile xsdFile = do
  localLoggingStart xsdFile
  putLog $ "============================== " ++ xsdFile ++ "\n"
  xsd <- liftIO $ readFile' xsdFile
  let xml = parseXML xsd
  res <- xmlToDecs $ filter isElem xml
  localLoggingEnd
  return res
