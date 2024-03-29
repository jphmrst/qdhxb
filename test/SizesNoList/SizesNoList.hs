{-# LANGUAGE TemplateHaskell #-}

module SizesNoList.SizesNoList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/SizesNoList/sizesNoList.xsd"]
qdhxb' ["test/SizesNoList/sizesNoList.xsd"]

testSizesNoList :: TLT IO ()
testSizesNoList = inGroup "XSD sizesNoList" $ do
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesNoList0a.xml" ~:
      SizeTypeSizesNoList2Union1SimpleRestrinteger 12 @==- p
  inGroup "SizesNoList 0b" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesNoList0a.xml" ~:
      SizeTypeSizesNoList2Union2SimpleRestrtoken "small" @==- p
  return ()
