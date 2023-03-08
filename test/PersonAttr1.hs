{-# LANGUAGE TemplateHaskell #-}

module PersonAttr1 (testPersonAttr1) where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["personattr1.xsd"]
qdhxb' ["personattr1.xsd"]

testPersonAttr1 :: TLT IO ()
testPersonAttr1 = inGroup "XSD PersonAttr 1" $ do
  inGroup "PersonAttr 1" $ do
    p <- lift $ loadPerson "personattr1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType (PersonattrAttrType (Just "Alpha") (Just 4)) @==- p
