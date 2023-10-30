{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- | The notional XML XSD specification.
--
-- = Translation of XSD into Haskell
--
-- The names extracted from the @xml.xsd@ XSD file are:
--
--  [@lang@ attribute]:
--
--      [@LangForms@]: Overall union implementation,
--                     renamed from @Xml4AttrAttrUnion@
--
--          [@LangImpl@]: Implementation of
--                        (anonymous) nested union simple type, renamed from
--                        @Xml4AttrAttrUnion1SimpleRestrstring@
--
--              [@LangAsString@]: Constructor, renamed from
--                  @Xml4AttrAttrUnionXml4AttrAttrUnion1SimpleRestrstring@.
--              [@LangAsLanguage@]: Constructor, renamed from
--                  @Xml4AttrAttrUnionLanguage@.
--
--  [@Space@ attribute]:
--
--      [@SpaceImpl@]: Renamed from @Xml6AttrAttrSimpleRestrNCName@
--
--  [@Base@ attribute]: Renamed from @BaseX6@
--
--  [@Id@ attribute]: Renamed from @IdX9@
--
--  [@SpecialAttrs@ group type and constructor]:
--      Renamed from @SpecialAttrsX5@

module QDHXB.Internal.L1.XML(
  module QDHXB.Internal.L1.XML
  ) where

import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions

import QDHXB.Internal.Debugln
qdhxb (
  renameType "BaseX6" "Base"
  . renameType "Xml6AttrAttrSimpleRestrNCName" "SpaceImpl"
  . renameType "Xml4AttrAttrUnion1SimpleRestrstring" "LangImpl"
  . renameType "IdX9" "Id"
  . renameType "Xml4AttrAttrUnion" "LangForms"
  . renameTypeAndConstructor "SpecialAttrsX5" "SpecialAttrs"
  . renameConstructor "Xml4AttrAttrUnionXml4AttrAttrUnion1SimpleRestrstring" "LangAsString"
  . renameConstructor "Xml4AttrAttrUnionLanguage" "LangAsLanguage"
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
