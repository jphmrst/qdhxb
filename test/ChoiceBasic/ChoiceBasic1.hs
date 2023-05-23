{-# LANGUAGE TemplateHaskell #-}

module ChoiceBasic.ChoiceBasic1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- The spec says <sequence> can contain <choice>, but revisit how that
-- flattens --- probably should not leave a TypeRef, but instead an
-- ElementRef.

-- qdhxb useDebugging ["test/ChoiceBasic/choice-basic1.xsd"]
-- qdhxb' ["test/ChoiceBasic/choice-basic1.xsd"]

testCB1 :: TLT IO ()
testCB1 = do
    {-
  inGroup "XSD choice basics 1" $ do
    inGroup "Age 1" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb1a.xml"
      "Correctly decode <age> in cb1a.xml" ~:
        ChoiceType [Top2ComplexSeq2ChoiceNuma 2] "" @==- p
    inGroup "Age 2" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb1b.xml"
      "Correctly decode <age> in cb1b.xml" ~: 55 @==- p
    -}
  return ()
