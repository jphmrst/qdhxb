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
  {-
  "Correctly decode <orderperson> text in seqbasic0a.xml" ~:
    (Seqbasic "xyz789" 59) @== (lift $ loadSeqbasic "test/SequenceBasic/seqbasic0a.xml")

  "Correctly decode <price> text in shiporder0b.xml" ~:
    10.9 @== (lift $ loadPrice "test/Shiporder/shiporder0b.xml")
  inGroup "Structures" $ do
    do p <- lift $ loadShipto "test/Shiporder/shiporder0c.xml"
       "Correctly decode <shipto> in shiporder0c.xml" ~:
         Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway"
           @==- p
    do p <- lift $ loadShiporder "test/Shiporder/shiporder1a.xml"
       "Correctly decode <shiporder> in shiporder1a.xml" ~:
         (Shiporder "889923" "John Smith"
           (Shipto "Ola Nordmann" "Langgt 23" "4000 Stavanger" "Norway")
           [ Item "Empire Burlesque" (Just "Special Edition") 1 10.9,
             Item "Hide your heart" Nothing 1 9.9
           ])
         @==- p
  -}
  return ()
