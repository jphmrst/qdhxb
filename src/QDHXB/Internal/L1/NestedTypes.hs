{-# LANGUAGE TemplateHaskell #-}

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.L1.NestedTypes(victory) where
import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
-- import qualified QDHXB.Internal.L1.XML

-- qdhxb (useXmlBindings . withXmlNamespacePrefix "xml"
--         . useDebugging
--         . logToFile "nestedTypes.log"
--       ) [
--   "src/QDHXB/datatypes.xsd",
--   "src/QDHXB/xsd.xsd"
--   ]

-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
