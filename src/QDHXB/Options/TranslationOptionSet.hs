
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
  optNamespaces :: [(String, NamespaceOptionSet)], -- ^ Final
                                                   -- namespace option
                                                   -- sets.
  optBreakAfterInput :: Bool, -- ^ Debugging option for breaking after
                              -- the input phase.  Will cause any call
                              -- to fail if set, but will seriously
                              -- cut the vomit of tracing output.
  optBreakAfterUnique :: Bool, -- ^ Debugging option for breaking
                               -- after the unique renaming.  Will
                               -- cause any call to fail if set, but
                               -- will seriously cut the vomit of
                               -- tracing output.
  optBreakAfterFlatten :: Bool, -- ^ Debugging option for breaking
                                -- after flattening.  Will cause any
                                -- call to fail if set, but will
                                -- seriously cut the vomit of tracing
                                -- output.
  optBreakAfterAllInput :: Bool, -- ^ Debugging option for breaking
                                 -- after all files are input (and
                                 -- before any generation).  Will
                                 -- cause any call to fail if set, but
                                 -- will seriously cut the vomit of
                                 -- tracing output.
  optTypeRenames :: [(String,String)]  -- ^ Before/after renaming
                                       -- options for generated
                                       -- Haskell types.
  }

instance Blockable QDHXBOptionSet where
  block (QDHXBOptionSetRecord newTypes xmlBuiltins xmlNsPrefixes
                              debuggingSettings debuggingDocOn
                              logFile resetLogging _dftNamespaceOpt
                              _ namespaces breakAfterInput
                              breakAfterUnique breakAfterFlatten
                              breakAfterAllInput
                              typeRenames) =
    stringToBlock "Options: "
    `stack2` (stringToBlock $ "- useNewType " ++ show newTypes)
    `stack2` (stringToBlock $ "- xmlBuiltins " ++ show xmlBuiltins)
    `stack2` (stringToBlock $ "- xmlNamespacePrefixes " ++ show xmlNsPrefixes)
    `stack2` (stringToBlock $ "- debugging " ++ show debuggingSettings)
    `stack2` (stringToBlock $ "- debuggingDocStrings " ++ show debuggingDocOn)
    `stack2` (stringToBlock $ "- logFile " ++ show logFile)
    `stack2` (stringToBlock $ "- resetLogging " ++ show resetLogging)
    `stack2` (stringToBlock $ "- namespaces " ++ bpp namespaces)
    `stack2` (stringToBlock $
                "- break after file input " ++ show breakAfterInput)
    `stack2` (stringToBlock $
                "- break after uniqueness pass " ++ show breakAfterUnique)
    `stack2` (stringToBlock $
                "- break after flatten pass " ++ show breakAfterFlatten)
    `stack2` (stringToBlock $
                "- break after all input pass " ++ show breakAfterAllInput)
    `stack2` (stringToBlock $ "- type renamings " ++ show typeRenames)


-- | The default set of options settings.
defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSetRecord True False [] [] False Nothing True
                                        defaultNamespaceOptionSet [] []
                                        False False False False
                                        []

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
        process_pairs [] _ os = os
        process_pairs ((url, fn) : pairs) dft os =
          process_pairs pairs dft $
            os { optNamespaces = (url, fn dft) : optNamespaces os }
