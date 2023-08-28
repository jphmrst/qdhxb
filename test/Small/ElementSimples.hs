{-# LANGUAGE TemplateHaskell #-}

module Small.ElementSimples where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Small test for an element with simple type/content --- bootstrap first
-- qdhxb (useDebugging . logByFile True) ["test/Small/element-simples.xsd"]
-- qdhxb (useDebugging) ["test/Small/element-simples.xsd"]
-- qdhxb' ["test/Small/element-simples.xsd"]

testElementSimples :: TLT IO ()
testElementSimples = inGroup "XSD element-simples" $ do

--  inGroup "ET 1" $ do
--    p <- lift $ loadE1 "test/Small/es1.xml"
--    "Correctly decode <e1> in es1.xml" ~: "alpha" @==- p

--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
