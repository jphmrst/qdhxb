
-- | Options for QDHXB calls.
module QDHXB.Options (
  -- * Options storage type
  QDHXBOptionSet(QDHXBOptionSet),
  -- ** Component accessors
  optUseNewType, optDebugging, optDebuggingDoc,
  -- ** Defaults
  defaultOptionSet,

  -- * Options
  --
  -- | These options to the top-level function are re-exported from
  -- the @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType, useDebugging, useDebuggingDoc
  )
where

import QDHXB.Internal.Utils.BPP

-- | The assortment of values to which options may be set.
data QDHXBOptionSet = QDHXBOptionSet {
  -- ^ Option settings, corresponding to `optUseNewType`,
  -- `optDebugging`.
  optUseNewType :: Bool, -- ^ Whether @newtype@ (if `True`) or @type@
                         -- (if `False`) should be used for type alias
                         -- declarations.  The default is `True` for
                         -- @newtype@.
  optDebugging :: Bool,  -- ^ Activates debugging output.  `False` by
                         -- default.
  optDebuggingDoc :: Bool  -- ^ Activates debugging output for Haddock
                           -- documentation string.  Has no effect
                           -- unless the basic `optDebugging` option
                           -- is alos set; is `False` by default.
  }

instance Blockable QDHXBOptionSet where
  block (QDHXBOptionSet optUseNewType optDebugging optDebuggingDoc) =
    stringToBlock "Options: "
    `stack2` (stringToBlock $ "- useNewType " ++ show optUseNewType)
    `stack2` (stringToBlock $ "- debugging " ++ show optDebugging)
    `stack2` (stringToBlock $ "- debuggingDocStrings " ++ show optDebuggingDoc)

-- | The default set of options settings.
defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSet True False False

-- | Type of one configuration step for options to the @qdhxb@
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.
useNewType :: QDHXBOption
useNewType (QDHXBOptionSet _ dbg dbgDoc) = QDHXBOptionSet True dbg dbgDoc

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
noUseNewType :: QDHXBOption
noUseNewType (QDHXBOptionSet _ dbg dbgDoc) = QDHXBOptionSet False dbg dbgDoc

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
useDebugging :: QDHXBOption
useDebugging (QDHXBOptionSet nt _ dbgDoc) = QDHXBOptionSet nt True dbgDoc

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
useDebuggingDoc :: QDHXBOption
useDebuggingDoc (QDHXBOptionSet nt dbg _) = QDHXBOptionSet nt dbg True
