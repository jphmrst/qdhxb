{-# LANGUAGE TemplateHaskell #-}

module SizesNoList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/sizesNoList.xsd"]
qdhxb' ["test/sizesNoList.xsd"]

testSizesNoList :: TLT IO ()
testSizesNoList = inGroup "XSD sizesNoList" $ do
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "test/sizesNoList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesNoList0a.xml" ~:
      SizeTypeTop2Union1SimpleRestrinteger 12 @==- p
  inGroup "SizesNoList 0b" $ do
    p <- lift $ loadSizes "test/sizesNoList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesNoList0a.xml" ~:
      SizeTypeTop2Union2SimpleRestrtoken "small" @==- p
