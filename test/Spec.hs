import Test.TLT
import Shiporder0()
import Shiporder1()
import Shiporder2()

main :: IO ()
main :: IO ()
main = do
  putStrLn "Test suite currently all compilation-based"
{-
  runSTT $ tlt $ do
    inGroup "JTMS tests" $ runJTMST $ do
      testEx1
      testEx3
    inGroup "ATMS tests" $ runATMST $ do
      ex1AndTest
    inGroup "LTMS tests" $ runLTMST $ do
      noUncaught "ltmsTest0" $ ltmsTest0
      noUncaught "ltmsTest1" $ ltmsTest1
-}
