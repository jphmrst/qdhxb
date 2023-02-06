{-# LANGUAGE TemplateHaskell #-}

module Initials where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB

-- qdhxb useDebugging ["initials.xsd"]
-- qdhxb' ["initials.xsd"]

testInitials :: TLT IO ()
testInitials = return () {- inGroup "XSD initials" $ do
  inGroup "Initials 1" $ do
    p <- lift $ loadInitials "initials1.xml"
    "Correctly decode <initials> in initials1.xml" ~: "JPM" @==- p
  inGroup "Initials 2" $ do
    p <- lift $ loadInitials "initials2.xml"
    "Correctly decode <initials> in initials2.xml" ~: "MJM" @==- p
  inGroup "Initials X" $ do
    p <- lift $ loadInitials "initialsX.xml"
    "Correctly decode <initials> in initials2.xml" ~: "JM" @==- p
-}
