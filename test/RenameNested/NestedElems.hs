{-# LANGUAGE TemplateHaskell #-}

module RenameNested.NestedElems where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/RenameNested/nestedelems.xsd"]
-- qdhxb (useDebugging) ["test/RenameNested/nestedelems.xsd"]
qdhxb' ["test/RenameNested/nestedelems.xsd"]

testNestedElems :: TLT IO ()
testNestedElems = inGroup "XSD nestedelems" $ do
--  inGroup "Age 1" $ do
--    p <- lift $ loadAge "test/RenameNested/age1.xml"
--    "Correctly decode <age> in age1.xml" ~: 10 @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
