{-# LANGUAGE TemplateHaskell #-}

module CustomersOrders.CustomersOrders1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Can't handle using the same name in different XML scopes
qdhxb (useDebugging) ["test/CustomersOrders/customersorders.xsd"]
-- qdhxb' ["test/CustomersOrders/customersorders.xsd"]

testCustomersOrders1 :: TLT IO ()
testCustomersOrders1 = inGroup "XSD customers/orders 1" $ do
  return ()
  {-
  inGroup "Scalars" $ do
    "Correctly decode <orderperson> text in shiporder0a.xml" ~:
      "John Smith" @== (lift $ loadOrderperson "test/shiporder0a.xml")
    "Correctly decode <price> text in shiporder0b.xml" ~:
      10.9 @== (lift $ loadPrice "test/shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShipto "test/shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "test/shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (Shiporder "889923" "John Smith"
           (Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ Item "Empire Burlesque" (Just "Special Edition") 1 10.9,
             Item "Hide your heart" Nothing 1 9.9
           ])
         @==- p
-}
