{-# LANGUAGE TemplateHaskell #-}

-- | Quick-and-Dirty Haskell/XSD bindings
module QDHXB (qdhxb) where

import Language.Haskell.TH (Q, Dec)
-- import System.Directory
import System.IO
import Control.Monad.IO.Class
import Text.XML.Light.Input
import QDHXB.Internal.XSDQ (XSDQ, runXSDQ)
import QDHXB.Manual
import QDHXB.XMLLight

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: [String] -> Q [Dec]
qdhxb xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  runXSDQ $ fmap concat $ mapM loadFile xsds

loadFile :: String -> XSDQ [Dec]
loadFile xsdFile = do
  xsd <- liftIO $ readFile' xsdFile
  let xml = parseXML xsd
  xmlToDecs $ filter isElem xml

