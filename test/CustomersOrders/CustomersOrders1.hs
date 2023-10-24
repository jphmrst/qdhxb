{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

module CustomersOrders.CustomersOrders1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Write test cases --- might be a problem, but might be with the big MS XML file.
-- import QDHXB.Internal.Debugln
-- qdhxb (setDebugging generate 3) ["test/CustomersOrders/customersorders.xsd"]
qdhxb' ["test/CustomersOrders/customersorders.xsd"]

testCustomersOrders1 :: TLT IO ()
testCustomersOrders1 = inGroup "XSD customers/orders 1" $ do
  {-
  inGroup "Load big file" $ do
    root <- lift $ loadRoot "test/CustomersOrders/customersorders.xml"
    case root of
      Root (Customers cs) (Orders os) -> do
        lift $ putStrLn $
          (show $ length cs) ++ " customers, "
          ++ (show $ length os) ++ " orders"
  -}
  return ()
