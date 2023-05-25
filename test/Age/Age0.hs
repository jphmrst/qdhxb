{-# LANGUAGE TemplateHaskell #-}

module Age.Age0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Age/age0.xsd"]
qdhxb' ["test/Age/age0.xsd"]

testAge0 :: TLT IO ()
testAge0 = inGroup "XSD age" $ do
  inGroup "Age 1" $ do
    p <- lift $ loadAge "test/Age/age1.xml"
    "Correctly decode <age> in age1.xml" ~: 10 @==- p
  inGroup "Age 2" $ do
    p <- lift $ loadAge "test/Age/age2.xml"
    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
