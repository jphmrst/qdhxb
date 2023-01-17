import Test.TLT
-- import Control.Monad.IO.Class
import Shiporder0 (testShiporder0)
import Shiporder1 ()
import Shiporder2 ()

main :: IO ()
main = do
  tlt $ do
    testShiporder0
{-
    inGroup "JTMS tests" $ runJTMST $ do
      testEx1
      testEx3
    inGroup "ATMS tests" $ runATMST $ do
      ex1AndTest
    inGroup "LTMS tests" $ runLTMST $ do
      noUncaught "ltmsTest0" $ ltmsTest0
      noUncaught "ltmsTest1" $ ltmsTest1
-}
