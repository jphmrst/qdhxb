{-# LANGUAGE TemplateHaskell #-}

module ChoiceBasic.ChoiceBasic2 where
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/ChoiceBasic/choice-basic2.xsd"]
qdhxb' ["test/ChoiceBasic/choice-basic2.xsd"]

-- TODO Add actual tests here
testCB2 :: TLT IO ()
testCB2 = inGroup "XSD choice basics 2" $ do
  {-
  inGroup "Age 1" $ do
    p <- lift $ loadCh "test/ChoiceBasic/cb1a.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <age> in cb1a.xml" ~:
      ChoiceType (Top2ComplexSeq2ChoiceNuma 4) "NameStr"
        @==- p
  inGroup "Age 2" $ do
    p <- lift $ loadCh "test/ChoiceBasic/cb1b.xml"
    "Correctly decode <age> in cb1b.xml" ~:
      ChoiceType (Top2ComplexSeq2ChoiceNumz "zzz") "NameStr" @==- p
  -}
  return ()

