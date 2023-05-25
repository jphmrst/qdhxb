{-# LANGUAGE TemplateHaskell #-}

module SequenceBasic.SequenceBasic2 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/SequenceBasic/seqbasic2.xsd"]
qdhxb' ["test/SequenceBasic/seqbasic2.xsd"]

testSequencebasic2 :: TLT IO ()
testSequencebasic2 = inGroup "XSD seqbasic2" $ do
  "Correctly decode <orderperson> text in seqbasic0a.xml" ~:
    (Seqbasic Nothing "xyz789" 59)
      @== (lift $ loadSeqbasic "test/SequenceBasic/seqbasic0a.xml")
  "Correctly decode <orderperson> text in seqbasic1a.xml" ~:
    (Seqbasic (Just "abc123") "xyz789" 59)
      @== (lift $ loadSeqbasic "test/SequenceBasic/seqbasic1a.xml")

  return ()
