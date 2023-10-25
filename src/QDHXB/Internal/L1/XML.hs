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
  renameGeneratedType "BaseX6" "Base"
  . renameGeneratedType "Top6AttrAttrSimpleRestrNCName" "SpaceImpl"
  . renameGeneratedType "Top4AttrAttrUnion1SimpleRestrstring" "LangImpl"
  . renameGeneratedType "IdX9" "Id"
  . renameGeneratedType "Top4AttrAttrUnion" "LangForms"
  -- . renameGeneratedType "SpecialAttrsX5" "SpecialAttrs"
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
