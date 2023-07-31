-- | Internal representation directly reflecting XSD code, in
-- particular allowing nested definitions.
module QDHXB.Internal.L0.Flatten (flattenSchemaItems) where

-- import System.Directory
import Text.XML.Light.Types
import Text.XML.Light.Output
import QDHXB.Internal.Generate
import QDHXB.Utils.Misc (applyFst, applySnd)
import QDHXB.Utils.BPP
import QDHXB.Utils.Misc (pickOrCombine)
import QDHXB.Utils.TH (firstToUpper)
import QDHXB.Utils.XMLLight (withSuffix)
import QDHXB.Internal.L0.NestedTypes
import QDHXB.Internal.AST
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

-- |Rewrite internally-represented XSD definitions, flattening any
-- nested definitions.
flattenSchemaItems :: [DataScheme] -> XSDQ [Definition]
flattenSchemaItems = flatten
