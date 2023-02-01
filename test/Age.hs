{-# LANGUAGE TemplateHaskell #-}

module Age where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

-- qdhxb useDebugging ["age.xsd"]
-- qdhxb' ["age.xsd"]

testAge :: TLT IO ()
testAge = return () {- inGroup "XSD age" $ do
  inGroup "Age 1" $ do
    p <- lift $ loadAge "age1.xml"
    "Correctly decode <age> in age1.xml" ~: 10 @==- p
  inGroup "Age 2" $ do
    p <- lift $ loadAge "age2.xml"
    "Correctly decode <age> in age2.xml" ~: 55 @==- p
-}
