
-- | Options for QDHXB calls.
module QDHXB.Options.NamespaceOptions (
  defaultModule
  )
where

import QDHXB.Options.NamespaceOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.  (This option is not yet implemented --- all
-- generated types are via either @type@ or @data@.)
defaultModule :: String -> NamespaceOption
defaultModule url opts = opts { optDefaultModule = Just url }
