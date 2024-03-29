{-# LANGUAGE TemplateHaskell #-}

module Shiporder.Shiporder2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Shiporder/shiporder2.xsd"]
qdhxb' ["test/Shiporder/shiporder2.xsd"]

testShiporder2 :: TLT IO ()
testShiporder2 = inGroup "XSD shiporder2" $ do
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderpersonX9 "test/Shiporder/shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPriceX8 "test/Shiporder/shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShiptoX10 "test/Shiporder/shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         Shiptotype "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "test/Shiporder/shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (Shipordertype "889923" "John Smith"
           (Shiptotype "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ Itemtype "Empire Burlesque" (Just "Special Edition") 1 10.9,
             Itemtype "Hide your heart" Nothing 1 9.9
           ])
         @==- p

  return ()
