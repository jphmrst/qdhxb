{-# LANGUAGE TemplateHaskell #-}

module Shiporder0 where
-- import System.IO (readFile')
-- import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Types
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import QDHXB.XMLLight

qdhxb ["shiporder0.xsd"]

testShiporder0 :: TLT IO ()
testShiporder0 = inGroup "XSD shiporder0" $ do
  inGroup "Scalars" $ do
    -- rawXml <- fmap parseXML $ lift $ readFile' "shiporder0a.xml"
    -- lift $ putStrLn $ show rawXml
    -- lift $ putStrLn $ show $ filter isElem rawXml
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "shiporder0b.xml")
    -- lift $ putStrLn $ show x0a
    return ()
  inGroup "XML shiporder1a.xml" $ do
    x1a <- lift $ loadShiporder "shiporder1a.xml"
    -- lift $ putStrLn $ show x1a
    return ()
