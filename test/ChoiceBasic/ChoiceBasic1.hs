{-# LANGUAGE TemplateHaskell #-}

module ChoiceBasic.ChoiceBasic1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/ChoiceBasic/choice-basic1.xsd"]
-- qdhxb' ["test/ChoiceBasic/choice-basic1.xsd"]

testCB1 :: TLT IO ()
testCB1 = do
  {-
  inChoice "XSD choice basics" $ do
    inChoice "Age 1" $ do
      p <- lift $ loadAge "test/ChoiceBasic/choice-basic01.xml"
      "Correctly decode <age> in age1.xml" ~: 10 @==- p
    inChoice "Age 2" $ do
      p <- lift $ loadAge "test/ChoiceBasic/choice-basic02.xml"
      "Correctly decode <age> in age2.xml" ~: 55 @==- p
  -}
  return ()

