{-# LANGUAGE TemplateHaskell #-}

module Shiporder.Mini3 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- The nesting here seemed to be a problem at one point.
-- qdhxb (useDebugging . logByFile True) ["test/Shiporder/mini3.xsd"]
qdhxb' ["test/Shiporder/mini3.xsd"]

testShiporderMini3 :: TLT IO ()
testShiporderMini3 = inGroup "XSD shiporder - mini 3" $ do
  do p <- lift $ loadTop "test/Shiporder/mini2a.xml"
     "Correctly decode <top> in mini2a.xml" ~:
       Top (Just $ Alpha "BBbb" "GGgg") @==- p
  return ()
