{-# LANGUAGE TemplateHaskell #-}

-- | Test for Haddock documentation generation.
module QDHXB.HaddockTest where
import Data.Time.Calendar
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["books-mod1.xsd"]
qdhxb' ["books-mod1.xsd"]

