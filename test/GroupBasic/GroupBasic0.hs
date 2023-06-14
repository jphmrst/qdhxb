{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.GroupBasic0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- TODO Type error in generated code
-- qdhxb (useDebugging . logByFile True) ["test/GroupBasic/group-basic0.xsd"]
-- qdhxb' ["test/GroupBasic/group-basic0.xsd"]

testGB0 :: TLT IO ()
testGB0 = inGroup "Group basic 0" $ do
  {-
  inGroup "GB0 - A" $ do
    p <- lift $ loadCombo "test/GroupBasic/gb0a.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <group> in gb0a.xml" ~:
      (ComboType (Just "status-test") (NumsGroup 44 "z44")) @==- p
  inGroup "GB0 - B" $ do
    p <- lift $ loadCombo "test/GroupBasic/gb0b.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <group> in gb0b.xml" ~:
      (ComboType Nothing (NumsGroup 44 "z44")) @==- p
  -}
  return ()
