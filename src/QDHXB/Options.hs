
-- | Options for QDHXB calls.
module QDHXB.Options (
  -- * Options storage type
  QDHXBOptionSet(..),
  defaultOptionSet,

  -- * Options
  --
  -- | These options to the top-level function are re-exported from
  -- the @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType,
  setDebugging, useDebuggingDoc,
  withXmlNamespacePrefix, logToFile,
  -- ** Built-in types
  useXmlBindings
  )
where

import QDHXB.Utils.BPP
import QDHXB.Utils.Debugln.New (Subject)

-- | The assortment of values to which options may be set.
data QDHXBOptionSet = QDHXBOptionSet {
  -- ^ Option settings, corresponding to `optUseNewType`,
  -- `optDebugging`.
  optUseNewType :: Bool, -- ^ Whether @newtype@ (if `True`) or @type@
                         -- (if `False`) should be used for type alias
                         -- declarations.  The default is `True` for
                         -- @newtype@.
  optAddXmlBindings :: Bool, -- ^ If `True`, includes certain types
                             -- from the XSD specification of XML.
                             -- The default is `False`.
  optXmlNamespacePrefixes :: [String], -- ^ Additional prefixes to be
                                       -- associated with the XML/XSD
                                       -- namespace.
  optDebugging :: [(Subject,Int)],  -- ^ Debugging settings.  @[]@ by
                                   -- default.
  optDebuggingDoc :: Bool,  -- ^ Activates debugging output for
                            -- Haddock documentation string.  Has no
                            -- effect unless the basic `optDebugging`
                            -- option is alos set; is `False` by
                            -- default.
  optLogToFile :: Maybe String,  -- ^ Activates logging for a call to
                                 -- @qdhxb@.
  optResetLogging :: Bool  -- ^ Indicates whether log files should be
                           -- reset at each call to QDHXB, or whether
                           -- they should be extended.
  }

instance Blockable QDHXBOptionSet where
  block (QDHXBOptionSet newTypes xmlBuiltins xmlNsPrefixes
                        debuggingSettings debuggingDocOn
                        logFile resetLogging) =
    stringToBlock "Options: "
    `stack2` (stringToBlock $ "- useNewType " ++ show newTypes)
    `stack2` (stringToBlock $ "- xmlBuiltins " ++ show xmlBuiltins)
    `stack2` (stringToBlock $ "- xmlNamespacePrefixes " ++ show xmlNsPrefixes)
    `stack2` (stringToBlock $ "- debugging " ++ show debuggingSettings)
    `stack2` (stringToBlock $ "- debuggingDocStrings " ++ show debuggingDocOn)
    `stack2` (stringToBlock $ "- logFile " ++ show logFile)
    `stack2` (stringToBlock $ "- resetLogging " ++ show resetLogging)

-- | The default set of options settings.
defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSet True False [] [] False Nothing True

-- | Type of one configuration step for options to the @qdhxb@
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.  (This option is not yet implemented --- all
-- generated types are via either @type@ or @data@.)
useNewType :: QDHXBOption
useNewType opts = opts { optAddXmlBindings = True }

-- | Include certain names from the XSD spec for XML in the
useXmlBindings :: QDHXBOption
useXmlBindings opts = opts { optAddXmlBindings = True }

-- | Include certain names from the XSD spec for XML in the
withXmlNamespacePrefix :: String -> QDHXBOption
withXmlNamespacePrefix prefix opts =
  opts { optXmlNamespacePrefixes = prefix : optXmlNamespacePrefixes opts }

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.  (No other possibilities are currently
-- available.)
noUseNewType :: QDHXBOption
noUseNewType opts = opts { optAddXmlBindings = True }

-- | Write debugging information to the screen.
setDebugging :: Subject -> Int -> QDHXBOption
setDebugging subj lv opts =
  opts { optDebugging = (subj, lv) : optDebugging opts }

-- | Include XSD comments/Haddock documentation strings in
-- debugging/log output.
useDebuggingDoc :: QDHXBOption
useDebuggingDoc opts = opts { optDebuggingDoc = True }

-- | Update the option set to specify whether per-XSD file logging
-- should be selected.
logToFile :: String -> QDHXBOption
logToFile s opts = opts { optLogToFile = Just s }
