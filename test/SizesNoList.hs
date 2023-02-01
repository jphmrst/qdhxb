{-# LANGUAGE TemplateHaskell #-}

module SizesNoList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

-- qdhxb useDebugging ["sizesNoList.xsd"]
qdhxb' ["sizesNoList.xsd"]

testSizesNoList :: TLT IO ()
testSizesNoList = inGroup "XSD sizesNoList" $ do
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "sizesNoList0a.xml"
    lift $ putStrLn $ show p
  {-
  inGroup "SizesNoList 1" $ do
    p <- lift $ loadSizes "sizesNoList1.xml"
    lift $ putStrLn $ show p
    return ()
  -}
  {-
    "Correctly decode <sizesNoList> in sizesNoList1.xml" ~: 10 @==- p
  inGroup "SizesNoList 2" $ do
    p <- lift $ loadSizesNoList "sizesNoList2.xml"
    "Correctly decode <sizesNoList> in sizesNoList2.xml" ~: 55 @==- p
  -}
