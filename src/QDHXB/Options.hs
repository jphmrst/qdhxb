
-- | Options for QDHXB calls.
module QDHXB.Options (
  -- * Storage type
  QDHXBOptionSet(optUseNewType), defaultOptionSet,

  -- * Options
  --
  -- | These options for the `qdhxb` function are re-exported from the
  -- @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType
  )
where

-- | The assortment of values to which options may be set.
data QDHXBOptionSet = QDHXBOptionSet {
  optUseNewType :: Bool -- ^ Whether @newtype@ (if `True`) or @type@
                        -- (if `False`) should be used for type alias
                        -- declarations.  The default is `True` for
                        -- @newtype@.
  }

defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSet True

-- | Type of one configuration step for options to the `qdhxb`
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.
useNewType :: QDHXBOption
useNewType (QDHXBOptionSet _) = QDHXBOptionSet True

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.
noUseNewType :: QDHXBOption
noUseNewType (QDHXBOptionSet _) = QDHXBOptionSet False
