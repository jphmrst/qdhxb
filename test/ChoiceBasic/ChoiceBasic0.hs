{-# LANGUAGE TemplateHaskell #-}

module ChoiceBasic.ChoiceBasic0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/ChoiceBasic/choice-basic0.xsd"]
qdhxb' ["test/ChoiceBasic/choice-basic0.xsd"]

testCB0 :: TLT IO ()
testCB0 = do
  inGroup "XSD choice basics 0" $ do
    {-
    lift $ do
      ch <- loadCh "test/ChoiceBasic/cb1a.xml"
      putStrLn $ show ch
    -}
    inGroup "CB 1a" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb1a.xml"
      "Correctly decode <ch>2</ch> in cb1a.xml" ~: ChoiceTypeNuma 2 @==- p
    inGroup "CB 1b" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb1b.xml"
      "Correctly decode <ch>zz</ch> in cb1b.xml" ~: ChoiceTypeNumz "zz" @==- p
  return ()
