{-# LANGUAGE TemplateHaskell #-}

module SizesAndList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["sizesAndList.xsd"]
-- qdhxb' ["sizesAndList.xsd"]

testSizesAndList :: TLT IO ()
testSizesAndList = inGroup "XSD sizesAndList" $ do
  return ()
  {-
  inGroup "SizesAndList 0a" $ do
    p <- lift $ loadSizes "sizesAndList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesAndList0a.xml" ~:
      SizeTypeInteger 12 @==- p
  -}
  {-
  inGroup "SizesAndList 0b" $ do
    p <- lift $ loadSizes "sizesAndList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesAndList0a.xml" ~:
      SizeTypeToken "small" @==- p
  -}
  {-
  inGroup "SizesAndList 1" $ do
    p <- lift $ loadSizes "sizesAndList1.xml"
    lift $ putStrLn $ show p
    return ()
  -}
  {-
    "Correctly decode <sizesAndList> in sizesAndList1.xml" ~: 10 @==- p
  inGroup "SizesAndList 2" $ do
    p <- lift $ loadSizesAndList "sizesAndList2.xml"
    "Correctly decode <sizesAndList> in sizesAndList2.xml" ~: 55 @==- p
  -}
