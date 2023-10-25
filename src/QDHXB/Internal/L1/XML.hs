{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- |The notional XML XSD specification.
module QDHXB.Internal.L1.XML(
  module QDHXB.Internal.L1.XML
  ) where

import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions

import QDHXB.Internal.Debugln
qdhxb (
  renameType "BaseX6" "Base"
  . renameType "Top6AttrAttrSimpleRestrNCName" "SpaceImpl"
  . renameType "Top4AttrAttrUnion1SimpleRestrstring" "LangImpl"
  . renameType "IdX9" "Id"
  . renameType "Top4AttrAttrUnion" "LangForms"
  . renameType "SpecialAttrsX5" "SpecialAttrs"
  . renameConstructor "SpecialAttrsX5" "SpecialAttrs"
  . renameConstructor "Top4AttrAttrUnionTop4AttrAttrUnion1SimpleRestrstring" "LangAsString"
  . renameConstructor "Top4AttrAttrUnionLanguage" "LangAsLanguage"
  -- . setDebugging input 1
  -- . setDebugging unique 1
  -- . setDebugging names 4
  -- . setDebugging flattening 1
  -- . setDebugging generate 3
  ) ["src/QDHXB/xml.xsd"]

{-
-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
-}
