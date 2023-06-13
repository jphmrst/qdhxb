{-# LANGUAGE TemplateHaskell #-}

module Books.BooksMod0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Books/books-mod0.xsd"]
qdhxb' ["test/Books/books-mod0.xsd"]

testBooksMod0 :: TLT IO ()
testBooksMod0 = inGroup "XSD books 0" $ do
   -- Will need much pruning down from original
  inGroup "Scalars" $ do
    "Sample in books.xml" ~:
      BooksForm [
        BookForm (Just "bk001") "The First Book",
        BookForm (Just "bk002") "The Poet's First Poem"
        ]
        @== (lift $ loadBooks "test/Books/books.xml")
  return ()
