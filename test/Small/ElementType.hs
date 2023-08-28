{-# LANGUAGE TemplateHaskell #-}

module Small.ElementType where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Test for renaming multiple attributes of the same name
-- qdhxb (useDebugging . logByFile True) ["test/Small/element-type.xsd"]
-- qdhxb (useDebugging) ["test/Small/element-type.xsd"]
-- qdhxb' ["test/Small/element-type.xsd"]

testElementType :: TLT IO ()
testElementType = inGroup "XSD element-type" $ do
--  inGroup "Age 1" $ do
--    p <- lift $ loadAge "test/RenameNested/age1.xml"
--    "Correctly decode <age> in age1.xml" ~: 10 @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()

