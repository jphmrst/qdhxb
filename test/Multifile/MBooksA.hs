{-# LANGUAGE TemplateHaskell #-}

module Multifile.MBooksA where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) [
--   "test/Multifile/books1.xsd", "test/Multifile/books2.xsd"]
qdhxb' ["test/Multifile/books1.xsd", "test/Multifile/books2.xsd"]

testMBookA :: TLT IO ()
testMBookA = inGroup "XSD books 0" $ do
  -- Will need much pruning down from original
  inGroup "Scalars" $ do
    "Sample in books.xml" ~:
      BookForm (Just "bk001") "The First Book"
        @== (lift $ loadBook "test/Books/book1.xml")
  return ()
