{-# LANGUAGE TemplateHaskell #-}

module Rename.Initials where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import QDHXB.Options
import QDHXB.Internal.Debugln
import qualified QDHXB.Expansions

-- import QDHXB.Internal.Debugln
-- qdhxb (setDebugging unique 3 . setDebugging flattening 3)
--   ["test/Initials/initials.xsd"]
qdhxb (
  renameGeneratedType "Initials" "InitialsRep"
  -- . setDebugging generate 3
  ) ["test/Initials/initials.xsd"]

testInitialsR :: TLT IO ()
testInitialsR = inGroup "XSD initials" $ do
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

