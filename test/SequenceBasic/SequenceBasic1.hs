{-# LANGUAGE TemplateHaskell #-}

module SequenceBasic.SequenceBasic1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

qdhxb useDebugging ["test/SequenceBasic/seqbasic1.xsd"]
-- qdhxb' ["test/SequenceBasic/seqbasic1.xsd"]

testSequencebasic1 :: TLT IO ()
testSequencebasic1 = inGroup "XSD seqbasic1" $ do
  "Correctly decode <orderperson> text in seqbasic1a.xml" ~:
    (Seqbasic "abc123" "xyz789" 59) @==
      (lift $ loadSeqbasic "test/SequenceBasic/seqbasic1a.xml")
  return ()
