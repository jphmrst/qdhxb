{-# LANGUAGE TemplateHaskell #-}

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.L1.NestedTypes(
  module QDHXB.Internal.L1.NestedTypes
  ) where

import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
import qualified QDHXB.Internal.L1.XML

-- Note that datatypes.xsd and xsd.xsd are mutually dependent, so both
-- must be loaded at the same call to `qdhxb`.  Recall (see ) that all
-- files are first parsed and flattened, and then all generated
-- together, so that all types etc. in the loaded files are known at
-- the final generation phase.

qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
         (defaultModule "QDHXB.Internal.L1.XML")
      ) [
  "src/QDHXB/datatypes.xsd", "src/QDHXB/xsd.xsd"
  ]

{-
import QDHXB.Internal.Debugln
qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
           (defaultModule "QDHXB.Internal.L1.XML")
        -- . setDebugging input 0
        -- . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 1
        . setDebugging generate 3
        -- . breakAfterAllInput
      )
  ["src/QDHXB/xsd.xsd", "src/QDHXB/datatypes.xsd"]
-}

{-
import QDHXB.Internal.Debugln
qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
           (defaultModule "QDHXB.Internal.L1.XML")
        -- . setDebugging input 0
        -- . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 1
        . setDebugging generate 3)
  ["src/QDHXB/xsd.xsd", "src/QDHXB/datatypes.xsd"]
-}
