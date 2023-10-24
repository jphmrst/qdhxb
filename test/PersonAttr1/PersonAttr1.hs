{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- Turn this back on when more stable

module PersonAttr1.PersonAttr1 (testPersonAttr1) where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- import QDHXB.Internal.Debugln
-- qdhxb (setDebugging generate 4) ["test/PersonAttr1/personattr1.xsd"]
qdhxb' ["test/PersonAttr1/personattr1.xsd"]

testPersonAttr1 :: TLT IO ()
testPersonAttr1 = inGroup "XSD PersonAttr 1" $ do
  inGroup "PersonAttr 1" $ do
    p <- lift $ loadPerson "test/PersonAttr1/personattr1.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType (Just $ PersonattrX1 (Just "Alpha") (Just 4)) @==- p
  return ()
