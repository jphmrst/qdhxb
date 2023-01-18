
-- | Options for QDHXB calls.
module QDHXB.Options (
  -- * Options storage type
  QDHXBOptionSet(QDHXBOptionSet),
  -- ** Component accessors
  optUseNewType, optDebugging,
  -- ** Defaults
  defaultOptionSet,

  -- * Options
  --
  -- | These options for the `qdhxb` function are re-exported from the
  -- @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType, useDebugging
  )
where

-- | The assortment of values to which options may be set.
data QDHXBOptionSet = QDHXBOptionSet {
  -- ^ Option settings, corresponding to `optUseNewType`,
  -- `optDebugging`.
  optUseNewType :: Bool, -- ^ Whether @newtype@ (if `True`) or @type@
                         -- (if `False`) should be used for type alias
                         -- declarations.  The default is `True` for
                         -- @newtype@.
  optDebugging :: Bool  -- ^ Activates debugging output.  `False` by
                        -- default.
  }

defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSet True False

-- | Type of one configuration step for options to the `qdhxb`
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.
useNewType :: QDHXBOption
useNewType (QDHXBOptionSet _ dbg) = QDHXBOptionSet True dbg

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
noUseNewType :: QDHXBOption
noUseNewType (QDHXBOptionSet _ dbg) = QDHXBOptionSet False dbg

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
useDebugging :: QDHXBOption
useDebugging (QDHXBOptionSet nt _) = QDHXBOptionSet nt True
