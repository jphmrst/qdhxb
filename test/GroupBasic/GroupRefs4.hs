{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.GroupRefs4 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Attributes defined in xs: not loading correctly

import QDHXB.Internal.Debugln
qdhxb (setDebugging generate 3) ["test/GroupBasic/group-refs2.xsd"]

{-
qdhxb' ["test/GroupBasic/group-refs2.xsd"]
-}

testGB4 :: TLT IO ()
testGB4 = do
  return ()
