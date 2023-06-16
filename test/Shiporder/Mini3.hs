{-# LANGUAGE TemplateHaskell #-}

module Shiporder.Mini3 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO The nesting here seemed to be a problem.
-- qdhxb (useDebugging . logByFile True) ["test/Shiporder/mini3.xsd"]
-- qdhxb' ["test/Shiporder/shiporder0.xsd"]

testShiporderMini3 :: TLT IO ()
testShiporderMini3 = inGroup "XSD shiporder - mini 3" $ do
  {-
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "test/Shiporder/shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "test/Shiporder/shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShipto "test/Shiporder/shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "test/Shiporder/shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (Shiporder "889923" "John Smith"
           (Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ Item "Empire Burlesque" (Just "Special Edition") 1 10.9,
             Item "Hide your heart" Nothing 1 9.9
           ])
         @==- p
  -}
  return ()