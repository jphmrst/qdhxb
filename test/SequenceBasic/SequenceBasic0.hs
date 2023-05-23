{-# LANGUAGE TemplateHaskell #-}

module SequenceBasic.SequenceBasic0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

qdhxb useDebugging ["test/SequenceBasic/seqbasic0.xsd"]
-- qdhxb' ["test/SequenceBasic/seqbasic0.xsd"]

testSequencebasic0 :: TLT IO ()
testSequencebasic0 = inGroup "XSD seqbasic0" $ do
  "Correctly decode <orderperson> text in seqbasic0a.xml" ~:
    (Seqbasic "xyz789" 59) @==
      (lift $ loadSeqbasic "test/SequenceBasic/seqbasic0a.xml")
  return ()

