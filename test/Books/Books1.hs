{-# LANGUAGE TemplateHaskell #-}

module Books.Books1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Books/books.xsd"]
-- qdhxb' ["test/Books/books.xsd"]

testBooks1 :: TLT IO ()
testBooks1 = inGroup "XSD books 1" $ do
  return ()
