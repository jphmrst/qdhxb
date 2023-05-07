{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.GroupBasic0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/GroupBasic/group-basic0.xsd"]
qdhxb' ["test/GroupBasic/group-basic0.xsd"]

testGB0 :: TLT IO ()
testGB0 = inGroup "Group basic 0" $ do
  lift $ putStrLn "Group basic 0"
  inGroup "GB0 - A" $ do
    p <- lift $ loadCombo "test/GroupBasic/gb0a.xml"
    lift $ putStrLn $ show p
    "Correctly decode <age> in age1.xml" ~:
      (ComboType (Just "status-test") (NumsGroup 44 "z44")) @==- p
