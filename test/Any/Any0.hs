{-# LANGUAGE TemplateHaskell #-}

module Any.Any0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Any/any0.xsd"]
-- qdhxb' ["test/Any/any0.xsd"]

testAny0 :: TLT IO ()
testAny0 = inGroup "XSD any0" $ do
  return ()
