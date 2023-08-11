{-# LANGUAGE TemplateHaskell #-}

module Books.Books1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions
import Data.Time.Calendar (fromGregorian)

-- qdhxb (useDebugging . logByFile True) ["test/Books/books.xsd"]
qdhxb' ["test/Books/books.xsd"]

testBooks1 :: TLT IO ()
testBooks1 = inGroup "XSD books 1" $ do
  "Correctly decode single <book> in book1.xml" ~:
    (BookForm (Just "bk001") "Writer" "The First Book" "Fiction"
              44.95 (fromGregorian 2000 10 1)
              "An amazing story of nothing.")
      @== (lift $ loadBookX1 "test/Books/book1.xml")
  return ()
