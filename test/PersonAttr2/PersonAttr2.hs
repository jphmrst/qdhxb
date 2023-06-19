{-# LANGUAGE TemplateHaskell #-}

module PersonAttr2.PersonAttr2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/PersonAttr2/personattr2.xsd"]
qdhxb' ["test/PersonAttr2/personattr2.xsd"]

testPersonAttr2 :: TLT IO ()
testPersonAttr2 = inGroup "XSD PersonAttr 2" $ do
  inGroup "PersonAttr 2" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType (Just $ Personattr (Just "Alpha") (Just 4)) @==- p
  inGroup "PersonAttr 1-M" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1m.xml"
    "Correctly decode <person> in personattr1m.xml"
      ~: PersonType (Just $ Personattr Nothing (Just 4)) @==- p
  return ()

