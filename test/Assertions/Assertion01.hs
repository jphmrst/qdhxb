{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn this back on when more stable

-- | Replicating an error in compiling group assertions in the XSD
-- spec.
module Assertions.Assertion01(testAssertion01) where
import Control.Monad.Trans.Class
import Test.TLT
import QDHXB.Internal.L0
import QDHXB.Options
import qualified QDHXB.Expansions
import qualified QDHXB.Internal.L1.XML

{- Generating:

data Top20GroupSeq4Choice
    = Top20GroupSeq4ChoiceFacetX11 String
    | QDHXB.Expansions.Content QDHXB.Expansions.Content
    deriving (Eq, Show)

via

| Generating from (k) at XSD line 167 on ChoiceDefn xs:Top20GroupSeq4Choice
| - Calling mapM (makeChoiceConstructor xs:Top20GroupSeq4Choice)
|           on (xs:facetX11,
|               ElementRef xs:facetX11 lower bound=1 upper bound=1)
|                (xs:XAny, Raw XML)

-}

{-
import QDHXB.Internal.Debugln
qdhxb (forNamespace "http://www.w3.org/XML/1998/namespace"
           (defaultModule "QDHXB.Internal.L1.XML")
        -- . setDebugging input 0
        . setDebugging names 3
        -- . setDebugging unique 4
        -- . setDebugging flattening 3
        -- . breakAfterFlatten
        . setDebugging generate 3
      )
  -- ["src/QDHXB/xsd.xsd", "src/QDHXB/datatypes.xsd"]
  -- ["test/Assertions/datatypes-fragment.xsd","test/Assertions/assertions.xsd"]
  ["test/Assertions/xsd.xsd","test/Assertions/datatypes.xsd"]
-}

-- |Placeholder, until the full XSD specification can actually be
-- generated.
testAssertion01 :: TLT IO ()
testAssertion01 = return ()
