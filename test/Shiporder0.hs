{-# LANGUAGE TemplateHaskell #-}

module Shiporder0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

-- qdhxb useDebugging ["shiporder0.xsd"]
qdhxb' ["shiporder0.xsd"]

testShiporder0 :: TLT IO ()
testShiporder0 = inGroup "XSD shiporder0" $ do
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShipto "shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         ShiptoType_ "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (ShiporderType_ "889923" "John Smith"
           (ShiptoType_ "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ ItemType_ "Empire Burlesque" (Just "Special Edition") 1 10.9,
             ItemType_ "Hide your heart" Nothing 1 9.9
           ])
         @==- p
