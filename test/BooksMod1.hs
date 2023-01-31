{-# LANGUAGE TemplateHaskell #-}

module BooksMod1 where
import Data.Time.Calendar
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

qdhxb useDebugging ["books-mod1.xsd"]
-- qdhxb' ["books-mod1.xsd"]

testBooksMod1 :: TLT IO ()
testBooksMod1 = inGroup "XSD books 1" $ do
  inGroup "Scalars" $ do
    "Sample in books.xml" ~:
      BooksForm [
        BookForm (Just "bk001") "Writer" "The First Book" "Fiction"
                 44.95 (read "2000-10-01") "An amazing story of nothing.",
        BookForm (Just "bk002") "Poet" "The Poet's First Poem" "Poem"
                 24.95 (read "2005-11-09") "Least poetic poems."
                ]
        @== (lift $ loadBooks "books.xml")
  return ()
