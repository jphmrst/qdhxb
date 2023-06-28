{-# LANGUAGE TemplateHaskell #-}

module Any.Any0 where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["test/Any/any0.xsd"]
qdhxb' ["test/Any/any0.xsd"]

testAny0 :: TLT IO ()
testAny0 = inGroup "XSD any0" $ do
  top1 <- lift $ loadTop "test/Any/any0a.xml"
  -- lift $ putStrLn $ show top1
  case top1 of
    Top name _ -> "Checking only the name from any0a.xml" ~: "Omega" @==- name
  return ()
