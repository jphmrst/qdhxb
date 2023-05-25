{-# LANGUAGE TemplateHaskell #-}

module PersonAttr2.PersonAttr2Inline where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/PersonAttr2/personattr2inline.xsd"]
-- qdhxb' ["test/PersonAttr2/personattr2inline.xsd"]

testPersonAttr2Inline :: TLT IO ()
testPersonAttr2Inline = inGroup "XSD PersonAttr 2" $ do
  {-
  inGroup "PersonAttr 2 - inline" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType (PersonattrAttrType (Just "Alpha") (Just 4)) @==- p
  inGroup "PersonAttr 1-M" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1m.xml"
    "Correctly decode <person> in personattr1m.xml"
      ~: PersonType (PersonattrAttrType Nothing (Just 4)) @==- p
  -}
  return ()
