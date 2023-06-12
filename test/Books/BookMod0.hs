{-# LANGUAGE TemplateHaskell #-}

module Books.BookMod0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Books/book-mod0.xsd"]
qdhxb' ["test/Books/book-mod0.xsd"]

testBookMod0 :: TLT IO ()
testBookMod0 = inGroup "XSD book (single book) 0" $ do
  inGroup "Book" $ do
    "Sample in book1.xml" ~:
      BookForm (Just "bk001") "The First Book"
        @== (lift $ loadBook "test/Books/book1.xml")
    "Sample in book2.xml" ~:
      BookForm (Just "bk002") "The Poet's First Poem"
        @== (lift $ loadBook "test/Books/book2.xml")
  return ()
