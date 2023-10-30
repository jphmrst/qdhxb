{-# LANGUAGE TemplateHaskell #-}

module SizesAndList.SizesAndList where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/SizesAndList/sizesAndList.xsd"]
qdhxb' ["test/SizesAndList/sizesAndList.xsd"]

testSizesAndList :: TLT IO ()
testSizesAndList = inGroup "XSD sizesAndList" $ do
  inGroup "SizesAndList 0a" $ do
    p <- lift $ loadSizes "test/SizesAndList/sizesAndList1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode sizesAndList1.xml" ~:
      [SizeTypeSizesAndList2Union1SimpleRestrinteger 12,
       SizeTypeSizesAndList2Union2SimpleRestrtoken "medium",
       SizeTypeSizesAndList2Union2SimpleRestrtoken "large",
       SizeTypeSizesAndList2Union1SimpleRestrinteger 9,
       SizeTypeSizesAndList2Union1SimpleRestrinteger 10] @==- p
  return ()
