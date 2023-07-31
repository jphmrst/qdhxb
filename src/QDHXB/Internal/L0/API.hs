{-# LANGUAGE TypeApplications #-}

-- | Top-level calls
module QDHXB.Internal.L0.API (qdhxb, qdhxb') where

import Language.Haskell.TH (Q, Dec)
import QDHXB.Options
import QDHXB.Internal.API
import QDHXB.Internal.AST
import QDHXB.Internal.L0.Input
import QDHXB.Internal.L0.NestedTypes

-- | Load the given XSD files, translating each into Haskell
-- declarations.
qdhxb :: QDHXBOption -> [String] -> Q [Dec]
-- | Load and translate the given XSD files with the default options.
qdhxb' :: [String] -> Q [Dec]
(qdhxb, qdhxb') = apiFunctions @DataScheme inputSchemaItems
