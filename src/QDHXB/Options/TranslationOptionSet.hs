
-- | Options for QDHXB calls.
module QDHXB.Options.TranslationOptionSet (
  -- * Options storage type
  QDHXBOptionSet(..),
  defaultOptionSet,
  QDHXBOption,
  finalizeNamespaceOptions
  )
where

import QDHXB.Utils.BPP
import QDHXB.Options.NamespaceOptionSet
import QDHXB.Utils.Debugln (Subject)

-- | The assortment of values to which options may be set.
data QDHXBOptionSet = QDHXBOptionSetRecord {
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
  optResetLogging :: Bool, -- ^ Indicates whether log files should be
                           -- reset at each call to QDHXB, or whether
                           -- they should be extended.
  optDefaultNamespaceOptionSet :: NamespaceOptionSet, -- ^ Default
                                                      -- option set
                                                      -- for
                                                      -- externally-defined
                                                      -- namespaces.
  optNamespaceSpecs :: [(String, NamespaceOption)], -- ^ Options for
                                                    -- declared
                                                    -- namespaces.
  optNamespaces :: [(String, NamespaceOptionSet)]  -- ^ Final
                                                   -- namespace option
                                                   -- sets.
  }

instance Blockable QDHXBOptionSet where
  block (QDHXBOptionSetRecord newTypes xmlBuiltins xmlNsPrefixes
                              debuggingSettings debuggingDocOn
                              logFile resetLogging dftNamespaceOpt
                              namespaceSpecs _) =
    stringToBlock "Options: "
    `stack2` (stringToBlock $ "- useNewType " ++ show newTypes)
    `stack2` (stringToBlock $ "- xmlBuiltins " ++ show xmlBuiltins)
    `stack2` (stringToBlock $ "- xmlNamespacePrefixes " ++ show xmlNsPrefixes)
    `stack2` (stringToBlock $ "- debugging " ++ show debuggingSettings)
    `stack2` (stringToBlock $ "- debuggingDocStrings " ++ show debuggingDocOn)
    `stack2` (stringToBlock $ "- logFile " ++ show logFile)
    `stack2` (stringToBlock $ "- resetLogging " ++ show resetLogging)
    -- `stack2` (stringToBlock $ "- namespaceSpecs " ++ show namespaceSpecs)

-- | The default set of options settings.
defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSetRecord True False [] [] False Nothing True
                                        defaultNamespaceOptionSet [] []

-- | Type of one configuration step for options to the @qdhxb@
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Use the `optNamespaceSpecs` and `optDefaultNamespaceOptionSet`
-- settings of a `QDHXBOptionSet` to initialize the `optNamespaces`
-- field.
finalizeNamespaceOptions :: QDHXBOptionSet -> QDHXBOptionSet
finalizeNamespaceOptions opts =
  let dftOptSet :: NamespaceOptionSet
      dftOptSet = optDefaultNamespaceOptionSet opts
      pairs :: [(String, NamespaceOption)]
      pairs = optNamespaceSpecs opts
  in process_pairs pairs dftOptSet opts

  where process_pairs ::
          [(String, NamespaceOption)] -> NamespaceOptionSet ->
            QDHXBOptionSet -> QDHXBOptionSet
        process_pairs [] _ opts = opts
        process_pairs ((url, fn) : pairs) dft opts =
          process_pairs pairs dft $
            opts { optNamespaces = (url, fn dft) : optNamespaces opts }
