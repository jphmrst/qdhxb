{-# LANGUAGE TemplateHaskell #-}

-- |The notional XML XSD specification.
module QDHXB.Internal.L1.XML(
  module QDHXB.Internal.L1.XML
  ) where

import QDHXB.Internal.L0
-- import QDHXB.Options
import qualified QDHXB.Expansions

{-
import QDHXB.Internal.Debugln
qdhxb (
  renameGeneratedType "BaseX6" "BaseImpl"
  -- . setDebugging input 1
  -- . setDebugging unique 1
  -- . setDebugging names 4
  -- . setDebugging flattening 1
  -- . setDebugging generate 1
  ) ["src/QDHXB/xml.xsd"]
-}

{-
qdhxb (
  renameGeneratedType "BaseX6" "BaseImpl"
  ) ["src/QDHXB/xml.xsd"]
-}

qdhxb' ["src/QDHXB/xml.xsd"]

{-
-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
-}
