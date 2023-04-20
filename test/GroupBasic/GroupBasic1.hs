{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.GroupBasic1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/GroupBasic/group-basic1.xsd"]
-- qdhxb' ["test/GroupBasic/group-basic1.xsd"]

testGB1 :: TLT IO ()
testGB1 = return ()
{-
inGroup "XSD age" $ do
  inGroup "Age 1" $ do
    p <- lift $ loadAge "test/GroupBasic/group-basic01.xml"
    "Correctly decode <age> in age1.xml" ~: 10 @==- p
  inGroup "Age 2" $ do
    p <- lift $ loadAge "test/GroupBasic/group-basic02.xml"
    "Correctly decode <age> in age2.xml" ~: 55 @==- p
-}
