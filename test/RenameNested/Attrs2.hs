{-# LANGUAGE TemplateHaskell #-}

module RenameNested.Attrs2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO POSTBOOTSTRAP simpleContent inside complexType, with multiple attributes of the same name
-- qdhxb (useDebugging . logByFile True) ["test/RenameNested/attrs2.xsd"]
-- qdhxb (useDebugging) ["test/RenameNested/attrs2.xsd"]
-- qdhxb' ["test/RenameNested/attrs2.xsd"]

testNestedAttrs2 :: TLT IO ()
testNestedAttrs2 = inGroup "XSD renaming nested --- attrs 2" $ do
--  inGroup "Age 1" $ do
--    p <- lift $ loadAge "test/RenameNested/age1.xml"
--    "Correctly decode <age> in age1.xml" ~: 10 @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
