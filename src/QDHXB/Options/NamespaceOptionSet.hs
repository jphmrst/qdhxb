{-# LANGUAGE FlexibleInstances #-}

-- | Options for QDHXB calls.
module QDHXB.Options.NamespaceOptionSet (
  -- * Options storage type
  NamespaceOptionSet(..),
  defaultNamespaceOptionSet,
  NamespaceOption
  )
where

import QDHXB.Utils.BPP

-- | The assortment of values to which options may be set.
data NamespaceOptionSet = NamespaceOptionSetRecord {
  -- ^ Option settings.
  optDefaultModule :: Maybe String  -- ^ Name of the module where
                                    -- QDHXB should look by default
                                    -- for the translation of XSD
                                    -- structures defined in a
                                    -- namespace.
  }

instance Blockable NamespaceOptionSet where
  block (NamespaceOptionSetRecord defaultModule) =
    stringToBlock "Namespace options: "
    `stack2` (stringToBlock $ "- default module " ++ show defaultModule)
instance Blockable (String, NamespaceOptionSet) where
  block = horizontalMapPairFn
instance Blockable [(String, NamespaceOptionSet)] where
  block = verticalBlockListFn

-- | The default set of options settings.
defaultNamespaceOptionSet :: NamespaceOptionSet
defaultNamespaceOptionSet = NamespaceOptionSetRecord Nothing

-- | Type of one configuration step for options to the @qdhxb@
-- function.  Combine them with function composition.
type NamespaceOption = NamespaceOptionSet -> NamespaceOptionSet
