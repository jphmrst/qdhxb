{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

module GroupBasic.GroupRefs3 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

{-
import QDHXB.Internal.Debugln
qdhxb (setDebugging generate 3) ["test/GroupBasic/group-refs.xsd"]
-}
qdhxb' ["test/GroupBasic/group-refs.xsd"]

testGB3 :: TLT IO ()
testGB3 = do
  return ()
