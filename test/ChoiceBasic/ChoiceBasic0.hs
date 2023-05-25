{-# LANGUAGE TemplateHaskell #-}

module ChoiceBasic.ChoiceBasic0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/ChoiceBasic/choice-basic0.xsd"]
qdhxb' ["test/ChoiceBasic/choice-basic0.xsd"]

testCB0 :: TLT IO ()
testCB0 = do
  inGroup "XSD choice basics 0" $ do
    {-
    lift $ do
      ch <- loadCh "test/ChoiceBasic/cb0a.xml"
      putStrLn $ show ch
    -}
    inGroup "CB 1a" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb0a.xml"
      "Correctly decode <ch>2</ch> in cb0a.xml" ~: ChoiceTypeNuma 2 @==- p
    inGroup "CB 1b" $ do
      p <- lift $ loadCh "test/ChoiceBasic/cb0b.xml"
      "Correctly decode <ch>zz</ch> in cb0b.xml" ~: ChoiceTypeNumz "zz" @==- p
  return ()
