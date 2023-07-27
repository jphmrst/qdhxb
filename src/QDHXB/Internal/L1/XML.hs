{-# LANGUAGE TemplateHaskell #-}

-- |The notional XML XSD specification.
module QDHXB.Internal.L1.XML(victory) where
import QDHXB.Internal.L0.API
import QDHXB.Options
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["src/QDHXB/xml.xsd"]
-- qdhxb' ["src/QDHXB/xml.xsd"]

-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
