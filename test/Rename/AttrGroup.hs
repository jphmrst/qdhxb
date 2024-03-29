{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable
{-# OPTIONS_GHC -Wno-unused-matches #-} -- This will be trickier --- need
                                        -- to use a wildcard argument
                                        -- for tryDecode in an
                                        -- attribute group with no
                                        -- members.

module Rename.AttrGroup where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB
import QDHXB.Options
import QDHXB.Internal.Debugln
import qualified QDHXB.Expansions

qdhxb (
  renameType "SpecialAttrsX5" "SpecialAttrs"
  -- . setDebugging generate 0
  -- . setDebugging names 4
  ) ["test/Rename/attrgroup.xsd"]

-- | This test is for compilation only; the XSD file under test does
-- not include any loadable @<element>@s.
test :: TLT IO ()
test = inGroup "XSD sizesNoList/min-1" $ do
  return ()
