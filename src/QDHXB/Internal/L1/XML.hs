{-# LANGUAGE TemplateHaskell #-}

-- |The notional XML XSD specification.
module QDHXB.Internal.L1.XML where
import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions

{-
import QDHXB.Internal.Debugln
qdhxb (setDebugging input 1
       . setDebugging unique 1
       . setDebugging flattening 1
       . setDebugging generate 1)
  ["src/QDHXB/xml.xsd"]
-}
qdhxb' ["src/QDHXB/xml.xsd"]

{-
-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
-}
