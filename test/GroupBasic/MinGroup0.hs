{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.MinGroup0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/GroupBasic/min-group-1.xsd"]
qdhxb' ["test/GroupBasic/min-group-1.xsd"]

testMinGroup0 :: TLT IO ()
testMinGroup0 = inGroup "Min group 0" $ do
  return ()
