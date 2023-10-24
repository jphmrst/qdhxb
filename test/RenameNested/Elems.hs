{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

module RenameNested.Elems where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/RenameNested/elems.xsd"]
-- qdhxb (useDebugging) ["test/RenameNested/elems.xsd"]
qdhxb' ["test/RenameNested/elems.xsd"]

testNestedElems :: TLT IO ()
testNestedElems = inGroup "XSD nestedelems" $ do
--  inGroup "Age 1" $ do
--    p <- lift $ loadAge "test/RenameNested/age1.xml"
--    "Correctly decode <age> in age1.xml" ~: 10 @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
