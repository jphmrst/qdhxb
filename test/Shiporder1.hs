{-# LANGUAGE TemplateHaskell #-}

module Shiporder1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

qdhxb' ["shiporder1.xsd"]

testShiporder1 :: TLT IO ()
testShiporder1 = inGroup "XSD shiporder1" $ do
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShipto "shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (Shiporder "889923" "John Smith"
           (Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ Item "Empire Burlesque" (Just "Special Edition") 1 10.9,
             Item "Hide your heart" Nothing 1 9.9
           ])
         @==- p
