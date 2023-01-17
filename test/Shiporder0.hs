{-# LANGUAGE TemplateHaskell #-}

module Shiporder0 where
import Test.TLT
import Text.XML.Light.Types
import Control.Monad.Trans.Class
import QDHXB
import QDHXB.XMLLight

qdhxb ["shiporder0.xsd"]

testShiporder0 :: TLT IO ()
testShiporder0 = inGroup "XSD shiporder0" $ do
  inGroup "XML shiporder0a.xml" $ do
    x0a <- lift $ loadOrderperson "shiporder0a.xml"
    lift $ putStrLn $ show x0a
    return ()
  inGroup "XML shiporder1a.xml" $ do
    x1a <- lift $ loadShiporder "shiporder1a.xml"
    lift $ putStrLn $ show x1a
    return ()
