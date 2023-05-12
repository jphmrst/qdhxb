{-# LANGUAGE TemplateHaskell #-}

module Initials.Initials where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/Initials/initials.xsd"]
qdhxb' ["test/Initials/initials.xsd"]

testInitials :: TLT IO ()
testInitials = inGroup "XSD initials" $ do
  inGroup "Initials 1" $ do
    p <- lift $ loadInitials "test/Initials/initials1.xml"
    "Correctly decode <initials> in initials1.xml" ~: "JPM" @==- p
  inGroup "Initials 2" $ do
    p <- lift $ loadInitials "test/Initials/initials2.xml"
    "Correctly decode <initials> in initials2.xml" ~: "MJM" @==- p
  inGroup "Initials X" $ do
    p <- lift $ loadInitials "test/Initials/initialsX.xml"
    "Correctly decode <initials> in initials2.xml" ~: "JM" @==- p
  return ()

