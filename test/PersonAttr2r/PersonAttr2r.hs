{-# LANGUAGE TemplateHaskell #-}

module PersonAttr2r.PersonAttr2r where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Type error in generated code
-- qdhxb (useDebugging . logByFile True) ["test/PersonAttr2r/personattr2r.xsd"]
-- qdhxb' ["test/PersonAttr2r/personattr2r.xsd"]

testPersonAttr2r :: TLT IO ()
testPersonAttr2r = inGroup "XSD PersonAttr 2r (for omitted attributes)" $ do
  {-
  inGroup "PersonAttr 1-M" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1m.xml"
    "Correctly decode <person> in personattr1m.xml"
      ~: PersonType (PersonattrAttrType Nothing (Just 4)) @==- p
  -}
  return ()
