{-# LANGUAGE TemplateHaskell #-}

module NestAttrNestType.Nest0 where
import QDHXB
import qualified QDHXB.Expansions

{- -- Add back when loading/testing
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB.Internal.L0 (qdhxb)
import QDHXB.Internal.Debugln
import QDHXB.Options
-}

qdhxb (
  id
  -- . setDebugging input 3
  -- . setDebugging names 3
  -- . setDebugging unique 4
  -- . setDebugging flattening 2
  -- . setDebugging generate 2
  ) [
  "test/NestAttrNestType/nestAttrNestType.xsd"
  ]
-- qdhxb' ["test/NestAttrNestType/seqbasic0.xsd"]

victory :: Int
victory = 1

{-
testSequencebasic0 :: TLT IO ()
testSequencebasic0 = inGroup "XSD seqbasic0" $ do
  "Correctly decode <orderperson> text in seqbasic0a.xml" ~:
    (Seqbasic "xyz789" 59) @==
      (lift $ loadSeqbasic "test/SequenceBasic/seqbasic0a.xml")
  return ()
-}
