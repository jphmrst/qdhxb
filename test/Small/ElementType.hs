{-# LANGUAGE TemplateHaskell #-}

module Small.ElementType where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Small/element-type.xsd"]
-- qdhxb (useDebugging) ["test/Small/element-type.xsd"]
qdhxb' ["test/Small/element-type.xsd"]

testElementType :: TLT IO ()
testElementType = inGroup "XSD element-type" $ do
  inGroup "ET 1" $ do
    p <- lift $ loadE1 "test/Small/et1.xml"
    "Correctly decode <e1> in et1.xml" ~: "alpha" @==- p
  return ()

