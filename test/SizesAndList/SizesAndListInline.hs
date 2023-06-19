{-# LANGUAGE TemplateHaskell #-}

module SizesAndList.SizesAndListInline where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/SizesAndList/sizesAndListInline.xsd"]
qdhxb' ["test/SizesAndList/sizesAndListInline.xsd"]

testSizesAndListInline :: TLT IO ()
testSizesAndListInline = inGroup "XSD sizesAndListInline" $ do
  inGroup "SizesAndList 0a" $ do
    p <- lift $ loadSizes "test/SizesAndList/sizesAndListInline1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode sizesAndList1.xml" ~:
      ["1", "m", "l", "9", "A"] @==- p
  return ()
