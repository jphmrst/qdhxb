{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.GroupBasic1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/GroupBasic/group-basic1.xsd"]
qdhxb' ["test/GroupBasic/group-basic1.xsd"]

testGB1 :: TLT IO ()
testGB1 = return ()
