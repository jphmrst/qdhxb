{-# LANGUAGE TemplateHaskell #-}

module Shiporder.Mini2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Shiporder/mini2.xsd"]
qdhxb' ["test/Shiporder/mini2.xsd"]

testShiporderMini2 :: TLT IO ()
testShiporderMini2 = inGroup "XSD shiporder - mini 2" $ do
  do p <- lift $ loadTop "test/Shiporder/mini2a.xml"
     "Correctly decode <top> in mini2a.xml" ~:
       Top (Just $ Alpha "BBbb" "GGgg") @==- p
  return ()
