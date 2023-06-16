{-# LANGUAGE TemplateHaskell #-}

module Shiporder.Mini1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Shiporder/mini1.xsd"]
qdhxb' ["test/Shiporder/mini1.xsd"]

testShiporderMini1 :: TLT IO ()
testShiporderMini1 = inGroup "XSD shiporder/mini 1" $ do
  inGroup "Mini 1-a" $ do
    "Correctly decode <toptag> text in mini1a.xml" ~:
      Toptag "sam" (Parent "a1" "b2")
        @== (lift $ loadToptag "test/Shiporder/mini1a.xml")
  return ()
