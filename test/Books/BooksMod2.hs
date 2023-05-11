{-# LANGUAGE TemplateHaskell #-}

module Books.BooksMod2 where
import Data.Time.Calendar
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/Books/books-mod2.xsd"]
-- qdhxb' ["test/Books/books-mod2.xsd"]

testBooksMod2 :: TLT IO ()
testBooksMod2 = inGroup "XSD books 2 --- int attr" $ do
  {-
  inGroup "Scalars" $ do
    "Sample in booksInt.xml" ~:
      BooksForm [
        BookForm (Just 2001) "Writer" "The First Book" "Fiction"
                 44.95 (read "2000-10-01") "An amazing story of nothing.",
        BookForm (Just 3004) "Poet" "The Poet's First Poem" "Poem"
                 24.95 (read "2005-11-09") "Least poetic poems."
                ]
        @== (lift $ loadBooks "test/Books/booksInt.xml")
  -}
  return ()
