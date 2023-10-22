{-# LANGUAGE TemplateHaskell #-}

-- | Replicating an error in compiling group assertions in the XSD
-- spec.
module Assertions.Assertion01(testAssertion01) where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
import qualified QDHXB.Internal.L1.XML

import QDHXB.Internal.Debugln
qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
           (defaultModule "QDHXB.Internal.L1.XML")
        -- . setDebugging input 0
        -- . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 1
        . setDebugging generate 3
      )
  -- ["src/QDHXB/xsd.xsd", "src/QDHXB/datatypes.xsd"]
  -- ["test/Assertions/datatypes-fragment.xsd","test/Assertions/assertions.xsd"]
  ["test/Assertions/xsd.xsd","test/Assertions/datatypes.xsd"]

-- |Placeholder, until the full XSD specification can actually be
-- generated.
testAssertion01 :: TLT IO ()
testAssertion01 = return ()
