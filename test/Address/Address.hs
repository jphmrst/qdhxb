{-# LANGUAGE TemplateHaskell #-}

module Address.Address where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

qdhxb useDebugging ["test/Address/address.xsd"]
--- qdhxb' ["test/Address/address.xsd"]

testAddress :: TLT IO ()
testAddress = inGroup "XSD address" $ do
  inGroup "Address 1" $ do
    p <- lift $ loadAddress "test/Address/address1.xml"
    lift $ putStrLn $ show p
    -- "Correctly decode <address> in address1.xml" ~: "JPM" @==- p
