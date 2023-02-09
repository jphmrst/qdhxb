{-# LANGUAGE TemplateHaskell #-}

module PersonAttr1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["personattr1.xsd"]
-- qdhxb' ["personattr1.xsd"]

testAge :: TLT IO ()
testAge = inGroup "XSD PersonAttr 1" $ do
  return () {-
  inGroup "PersonAttr 1" $ do
    p <- lift $ loadPerson "personattr1.xml"
    lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType "Alpha" 4 @==- p
  -}