
-- | Options for QDHXB calls.
module QDHXB.Options.TranslationOptions (
  -- * Options
  --
  -- | These options to the top-level function are re-exported from
  -- the @QDHXB.Options@ module.
  -- ** Cleaning up translated names.
  -- | It is often necessary to clean up the automatically created
  -- and disambiguated names assigned in generated Haskell code.
  renameGeneratedType, renameConstructor,
  -- ** Structure of renamed types
  useNewType, noUseNewType,
  setDebugging, useDebuggingDoc,
  withXmlNamespacePrefix, logToFile,
  forNamespace,
  -- ** Built-in types
  useXmlBindings,
  -- ** Debugging
  breakAfterInput, breakAfterUnique, breakAfterFlatten, breakAfterAllInput
  )
where

import QDHXB.Utils.Debugln (Subject)
import QDHXB.Options.NamespaceOptionSet
import QDHXB.Options.TranslationOptionSet

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

-- | Set options for referring to a separately-translated namespace.
forNamespace :: String -> NamespaceOption -> QDHXBOption
forNamespace url namespaceOpt opts =
  opts { optNamespaceSpecs = (url, namespaceOpt) : optNamespaceSpecs opts }

-- | Set the option of breaking execution after the input pass.
breakAfterInput :: QDHXBOption
breakAfterInput opts = opts { optBreakAfterInput = True }

-- | Set the option of breaking execution after the unique renaming
-- pass.
breakAfterUnique :: QDHXBOption
breakAfterUnique opts = opts { optBreakAfterUnique = True }

-- | Set the option of breaking execution after the flatten pass.
breakAfterFlatten :: QDHXBOption
breakAfterFlatten opts = opts { optBreakAfterFlatten = True }

-- | Set the option of breaking execution after the input pass.
breakAfterAllInput :: QDHXBOption
breakAfterAllInput opts = opts { optBreakAfterAllInput = True }

-- | Specify a before/after renaming of generated Haskell types.
renameGeneratedType :: String -> String -> QDHXBOption
renameGeneratedType before after opts = opts {
  optTypeRenames = (before, after) : optTypeRenames opts
  }

-- | Specify a before/after renaming of generated Haskell types.
renameConstructor :: String -> String -> QDHXBOption
renameConstructor before after opts = opts {
  optConstructorRenames = (before, after) : optConstructorRenames opts
  }
