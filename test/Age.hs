{-# LANGUAGE TemplateHaskell #-}

module Age where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/age.xsd"]
qdhxb' ["test/age.xsd"]

testAge :: TLT IO ()
testAge = inGroup "XSD age" $ do
  inGroup "Age 1" $ do
    p <- lift $ loadAge "test/age1.xml"
    "Correctly decode <age> in age1.xml" ~: 10 @==- p
  inGroup "Age 2" $ do
    p <- lift $ loadAge "test/age2.xml"
    "Correctly decode <age> in age2.xml" ~: 55 @==- p
