{-# LANGUAGE TemplateHaskell #-}

module PersonAttr2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

qdhxb useDebugging ["personattr2.xsd"]
-- qdhxb' ["personattr2.xsd"]

testPersonAttr2 :: TLT IO ()
testPersonAttr2 = inGroup "XSD PersonAttr 2" $ do
  inGroup "PersonAttr 2" $ do
    p <- lift $ loadPerson "personattr1.xml"
    lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType (PersonattrAttrType "Alpha" 4) @==- p
