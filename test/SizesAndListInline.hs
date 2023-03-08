{-# LANGUAGE TemplateHaskell #-}

module SizesAndListInline where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

qdhxb useDebugging ["sizesAndListInline.xsd"]
-- qdhxb' ["sizesAndListInline.xsd"]

testSizesAndListInline :: TLT IO ()
testSizesAndListInline = inGroup "XSD sizesAndListInline" $ do
  inGroup "SizesAndList 0a" $ do
    p <- lift $ loadSizes "sizesAndListInline1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode sizesAndList1.xml" ~:
      [SizeTypeInteger 12,
       SizeTypeToken "medium",
       SizeTypeToken "large",
       SizeTypeInteger 9,
       SizeTypeInteger 10] @==- p
