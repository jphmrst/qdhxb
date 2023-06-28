{-# LANGUAGE TemplateHaskell #-}

-- |The full XSD specification, bootstrapped here for the full
-- exported implementation.
module QDHXB.Internal.Fullspec(victory) where
import QDHXB.Internal.API
import QDHXB.Options
import qualified QDHXB.Expansions

-- qdhxb (useDebugging . logByFile True) ["src/QDHXB/xsd.xsd"]
-- qdhxb' ["src/QDHXB/xsd.xsd"]

-- |Placeholder, until the full XSD specification can actually be
-- generated.
victory :: String
victory = "hooray"
