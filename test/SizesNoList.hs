{-# LANGUAGE TemplateHaskell #-}

module SizesNoList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["sizesNoList.xsd"]
qdhxb' ["sizesNoList.xsd"]

testSizesNoList :: TLT IO ()
testSizesNoList = inGroup "XSD sizesNoList" $ do
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "sizesNoList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesNoList0a.xml" ~:
      SizeTypeInteger 12 @==- p
  inGroup "SizesNoList 0b" $ do
    p <- lift $ loadSizes "sizesNoList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesNoList0a.xml" ~:
      SizeTypeToken "small" @==- p
