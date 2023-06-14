{-# LANGUAGE TemplateHaskell #-}

module GroupBasic.MinGroup1 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/GroupBasic/min-group-2.xsd"]
qdhxb' ["test/GroupBasic/min-group-2.xsd"]

testMinGroup1 :: TLT IO ()
testMinGroup1 = inGroup "Min group 1" $ do
  inGroup "Min group 1 - A" $ do
    p <- lift $ loadCombo "test/GroupBasic/min1a.xml"
    -- lift $ putStrLn $ show p
    "Correctly decode <group> in min1a.xml" ~:
      ComboType (TheGroupGroupContent "abc123") @==- p
  return ()

