
-- | Options for QDHXB calls.
module QDHXB.Options (
  -- * Options storage type
  QDHXBOptionSet(QDHXBOptionSet),
  -- ** Component accessors
  optUseNewType, optDebugging, optDebuggingDoc, optByFileLogging,
  optResetLogging, optLogFileMaker, optLogCentralFile,
  defaultOptionSet,

  -- * Options
  --
  -- | These options to the top-level function are re-exported from
  -- the @QDHXB.Options@ module.
  QDHXBOption,
  -- ** Structure of renamed types
  useNewType, noUseNewType, useDebugging, useDebuggingDoc,
  logByFile, logCentral
  )
where

import QDHXB.Utils.BPP

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
  optDebuggingDoc :: Bool,  -- ^ Activates debugging output for
                            -- Haddock documentation string.  Has no
                            -- effect unless the basic `optDebugging`
                            -- option is alos set; is `False` by
                            -- default.
  optByFileLogging :: Bool,  -- ^ Activates logging for each XSD file.
  optResetLogging :: Bool,  -- ^ Indicates whether log files should be
                            -- reset at each call to QDHXB, or whether
                            -- they should be extended.
  optLogFileMaker :: String -> String,  -- ^ Function which, given the
                                        -- file name of an XSD file to
                                        -- be processed, returns the
                                        -- name of the file to which a
                                        -- log of the translation
                                        -- should be written.
  optLogCentralFile :: Maybe String   -- ^ Name of a central file to
                                      -- which all logging details
                                      -- should be written.
  }

instance Blockable QDHXBOptionSet where
  block (QDHXBOptionSet newTypes debuggingOn debuggingDocOn byFileLogging
                        resetLogging _logFileMaker logCentralFile) =
    stringToBlock "Options: "
    `stack2` (stringToBlock $ "- useNewType " ++ show newTypes)
    `stack2` (stringToBlock $ "- debugging " ++ show debuggingOn)
    `stack2` (stringToBlock $ "- debuggingDocStrings " ++ show debuggingDocOn)
    `stack2` (stringToBlock $ "- byFileLogging " ++ show byFileLogging)
    `stack2` (stringToBlock $ "- resetLogging " ++ show resetLogging)
    `stack2` (stringToBlock $ "- logCentralFile " ++ show logCentralFile)

-- | The default set of options settings.
defaultOptionSet :: QDHXBOptionSet
defaultOptionSet = QDHXBOptionSet True False False False True
                                  defaultLogFileMaker Nothing

-- | The default way of building a log file name --- replace a
-- trailing @".xsd"@ with @".log"@ if possible, or else append
-- @".log"@.
defaultLogFileMaker :: String -> String
defaultLogFileMaker str = case reverse str of
                            'd':'s':'x':'.':rts ->
                              reverse rts ++ ".log"
                            'D':'S':'X':'.':rts ->
                              reverse rts ++ ".log"
                            _ -> str ++ ".log"

-- | Type of one configuration step for options to the @qdhxb@
-- function.  Combine them with function composition.
type QDHXBOption = QDHXBOptionSet -> QDHXBOptionSet

-- | Generate opaque types (@newtype@) when one type is simply a
-- renaming of another.  (This option is not yet implemented --- all
-- generated types are via either @type@ or @data@.)
useNewType :: QDHXBOption
useNewType (QDHXBOptionSet _ dbg dbgDoc ifLogByFile
                           resetLogs logFileMaker logCentralFile) =
  QDHXBOptionSet True dbg dbgDoc ifLogByFile
                 resetLogs logFileMaker logCentralFile

-- | Generate types aliases (@type@) when one type is simply a
-- renaming of another.  (No other possibilities are currently
-- available.)
noUseNewType :: QDHXBOption
noUseNewType (QDHXBOptionSet _ dbg dbgDoc ifLogByFile
                             resetLogs logFileMaker logCentralFile) =
  QDHXBOptionSet False dbg dbgDoc ifLogByFile
                 resetLogs logFileMaker logCentralFile

-- | Write debugging information to the screen.
useDebugging :: QDHXBOption
useDebugging (QDHXBOptionSet nt _ dbgDoc ifLogByFile
                             resetLogs logFileMaker logCentralFile) =
  QDHXBOptionSet nt True dbgDoc ifLogByFile
                 resetLogs logFileMaker logCentralFile

-- | Include XSD comments/Haddock documentation strings in
-- debugging/log output.
useDebuggingDoc :: QDHXBOption
useDebuggingDoc (QDHXBOptionSet nt dbg _ ifLogByFile
                                resetLogs logFileMaker logCentralFile) =
  QDHXBOptionSet nt dbg True ifLogByFile resetLogs logFileMaker logCentralFile

-- | Update the option set to specify whether per-XSD file logging
-- should be selected.
logByFile :: Bool -> QDHXBOption
logByFile b (QDHXBOptionSet nt dbg dbgDoc _
                            resetLogs logFileMaker logCentralFile) =
  QDHXBOptionSet nt dbg dbgDoc b resetLogs logFileMaker logCentralFile

-- | Update the option set to name a file for single-file logging.
logCentral :: String -> QDHXBOption
logCentral s (QDHXBOptionSet nt dbg dbgDoc ifLogByFile
                                 resetLogs logFileMaker _) =
  QDHXBOptionSet nt dbg dbgDoc ifLogByFile resetLogs logFileMaker $ Just s
