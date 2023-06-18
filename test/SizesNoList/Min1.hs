{-# LANGUAGE TemplateHaskell #-}

module SizesNoList.Min1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/SizesNoList/min-1.xsd"]
qdhxb' ["test/SizesNoList/min-1.xsd"]

testSNLmin1 :: TLT IO ()
testSNLmin1 = inGroup "XSD sizesNoList/min-1" $ do
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesNoList0a.xml" ~:
      SizeTypeAlt1 12 @==- p
  inGroup "SizesNoList 0b" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesNoList0a.xml" ~:
      SizeTypeAlt2 "small" @==- p
  return ()
