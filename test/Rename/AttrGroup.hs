{-# LANGUAGE TemplateHaskell #-}

module Rename.AttrGroup where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import QDHXB.Options
import QDHXB.Internal.Debugln
import qualified QDHXB.Expansions

qdhxb (
  renameGeneratedType "SpecialAttrsX5" "SpecialAttrs"
  . setDebugging generate 0
  -- . setDebugging names 4
  ) ["test/Rename/attrgroup.xsd"]

test :: TLT IO ()
test = inGroup "XSD sizesNoList/min-1" $ do
  {-
  inGroup "SizesNoList 0a" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0a.xml"
    "Correctly decode <sizes>12</sizes> in sizesNoList0a.xml" ~:
      SizeForm1 12 @==- p
  inGroup "SizesNoList 0b" $ do
    p <- lift $ loadSizes "test/SizesNoList/sizesNoList0b.xml"
    "Correctly decode <sizes>small</sizes> in sizesNoList0a.xml" ~:
      SizeTypeAlt2 "small" @==- p
  -}
  return ()
