{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

module RenameNested.Attrs where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import qualified QDHXB.Expansions
import Data.Symbol

{-
import QDHXB.Internal.Debugln
qdhxb (setDebugging unique 0 . setDebugging input 0 . setDebugging flattening 0 . setDebugging generate 0) ["test/RenameNested/attrs.xsd"]
-}
qdhxb' ["test/RenameNested/attrs.xsd"]

testNestedAttrs :: TLT IO ()
testNestedAttrs = inGroup "XSD renaming nested --- attrs" $ do
--  inGroup "Age 1" $ do
--    p <- lift $ loadAge "test/RenameNested/age1.xml"
--    "Correctly decode <age> in age1.xml" ~: 10 @==- p
--  inGroup "Age 2" $ do
--    p <- lift $ loadAge "test/RenameNested/age2.xml"
--    "Correctly decode <age> in age2.xml" ~: 55 @==- p
  return ()
