
-- | Top-level calls
module QDHXB.Internal.API (qdhxb, qdhxb') where

import Language.Haskell.TH (Q, Dec)
import System.IO
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.List (intercalate)
import Text.XML.Light.Input (parseXML)
import QDHXB.Internal.Utils.XMLLight (isElem)
import QDHXB.Internal.XSDQ (XSDQ, runXSDQ, whenDebugging)
import QDHXB.Internal.Pipeline
import QDHXB.Options

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: QDHXBOption -> [String] -> Q [Dec]
qdhxb opts xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  runXSDQ opts $ do
    now <- liftIO $ getCurrentTime
    whenDebugging $ liftIO $ do
      writeFile "last.out" ""
      appendFile "last.out" $
        "QDHXB  -*- mode: text -*-\n"
        ++ "Run at: " ++ show now ++ "\n"
        ++ "Files: " ++ intercalate ", " xsds ++ "\n"
    fmap concat $ mapM loadFile xsds

-- | Load and translate the given XSD files with the default options.
qdhxb' :: [String] -> Q [Dec]
qdhxb' = qdhxb id

loadFile :: String -> XSDQ [Dec]
loadFile xsdFile = do
  liftIO $ appendFile "last.out" $
    "\n============================== " ++ xsdFile ++ "\n"
  xsd <- liftIO $ readFile' xsdFile
  let xml = parseXML xsd
  xmlToDecs $ filter isElem xml
