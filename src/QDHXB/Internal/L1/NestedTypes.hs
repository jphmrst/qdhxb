{-# LANGUAGE TemplateHaskell #-}

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.L1.NestedTypes(victory) where
import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
import qualified QDHXB.Internal.L1.XML


-- Note that datatypes.xsd and xsd.xsd are mutually dependent, so both
-- must be loaded at the same call to `qdhxb`.  Recall (see ) that all
-- files are first parsed and flattened, and then all generated
-- together, so that all types etc. in the loaded files are known at
-- the final generation phase.

{-
qdhxb (withXmlNamespacePrefix "xml"
      ) [
  "src/QDHXB/datatypes.xsd", "src/QDHXB/xsd.xsd"
  ]
-}

import QDHXB.Internal.Debugln
qdhxb (setDebugging input 1 .
        --setDebugging unique 1 .
        --setDebugging flattening 1 .
        setDebugging generate 1)
  ["src/QDHXB/xsd.xsd", "src/QDHXB/datatypes.xsd"]

-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
