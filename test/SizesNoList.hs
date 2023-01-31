{-# LANGUAGE TemplateHaskell #-}

module SizesNoList where
import Data.Time.Calendar
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

qdhxb useDebugging ["sizesNoList.xsd"]
-- qdhxb' ["sizesNoList.xsd"]

testSizesNoList :: TLT IO ()
testSizesNoList = return () {- inGroup "XSD sizesNoList" $ do
  inGroup "SizesNoList 1" $ do
    p <- lift $ loadSizesNoList "sizesNoList1.xml"
    return 0  -} {-
    "Correctly decode <sizesNoList> in sizesNoList1.xml" ~: 10 @==- p
  inGroup "SizesNoList 2" $ do
    p <- lift $ loadSizesNoList "sizesNoList2.xml"
    "Correctly decode <sizesNoList> in sizesNoList2.xml" ~: 55 @==- p
-}
