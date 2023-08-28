{-# LANGUAGE TemplateHaskell #-}

module Small.ElementTypeAttr where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Defining simpleContent inside complexType --- bootstrap first
-- qdhxb (useDebugging . logByFile True) ["test/Small/element-type-attr.xsd"]
-- qdhxb (useDebugging) ["test/Small/element-type-attr.xsd"]
-- qdhxb' ["test/Small/element-type-attr.xsd"]

testElementTypeAttr :: TLT IO ()
testElementTypeAttr = inGroup "XSD element-type-attr" $ do
--  inGroup "ETA 1" $ do
--    p <- lift $ loadE1 "test/Small/eta1.xml"
--    "Correctly decode <e1> in et1a.xml" ~: "alpha" @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
