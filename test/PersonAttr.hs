{-# LANGUAGE TemplateHaskell #-}

module PersonAttr where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["personattr.xsd"]
-- qdhxb' ["personattr.xsd"]

testAge :: TLT IO ()
testAge = inGroup "XSD PersonAttr" $ do
  return () {-
  inGroup "PersonAttr 1" $ do
    p <- lift $ loadPerson "personattr1.xml"
    lift $ putStrLn $ show p
    "Correctly decode <person> in personattr1.xml"
      ~: PersonType "Alpha" 4 @==- p
  -}
