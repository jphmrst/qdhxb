{-# LANGUAGE TemplateHaskell #-}

module QDHXB (qdhxb) where

import Language.Haskell.TH (Q, Dec)
-- import System.Directory
import System.IO
import Control.Monad.IO.Class
import Text.XML.Light.Input
import QDHXB.Manual
import QDHXB.Internal
import QDHXB.XMLLight

qdhxb :: [String] -> Q [Dec]
qdhxb xsds = do
  -- liftIO (getCurrentDirectory >>= putStrLn . show)
  fmap concat $ mapM loadFile xsds

loadFile :: String -> Q [Dec]
loadFile xsdFile = do
  typ <- [t|[Int]|]
  liftIO $ putStrLn $ show typ
  typ2 <- [t|()|]
  liftIO $ putStrLn $ show typ2
  typ2 <- [t|(Char, Int)|]
  liftIO $ putStrLn $ show typ2
  xsd <- liftIO $ readFile' xsdFile
  let xml = parseXML xsd
  xmlToDecs $ filter isElem xml
  -- fmap concat $ mapM
