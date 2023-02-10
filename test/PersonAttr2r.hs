{-# LANGUAGE TemplateHaskell #-}

module PersonAttr2r where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["personattr2.xsd"]
qdhxb' ["personattr2r.xsd"]

testPersonAttr2r :: TLT IO ()
testPersonAttr2r = inGroup "XSD PersonAttr 2r (for omitted attributes)" $ do
  inGroup "PersonAttr 1-M" $ do
    p <- lift $ loadPerson "personattr1m.xml"
    "Correctly decode <person> in personattr1m.xml"
      ~: PersonType (PersonattrAttrType Nothing (Just 4)) @==- p
