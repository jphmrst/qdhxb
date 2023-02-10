{-# LANGUAGE TemplateHaskell #-}

module SizesAndList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["sizesAndList.xsd"]
qdhxb' ["sizesAndList.xsd"]

testSizesAndList :: TLT IO ()
testSizesAndList = inGroup "XSD sizesAndList" $ do
  inGroup "SizesAndList 0a" $ do
    p <- lift $ loadSizes "sizesAndList1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode sizesAndList1.xml" ~:
      [SizeTypeInteger 12,
       SizeTypeToken "medium",
       SizeTypeToken "large",
       SizeTypeInteger 9,
       SizeTypeInteger 10] @==- p
