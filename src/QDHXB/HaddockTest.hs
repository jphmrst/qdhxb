{-# LANGUAGE TemplateHaskell #-}

-- | Test for Haddock documentation generation.
module QDHXB.HaddockTest where
import Data.Time.Calendar
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["test/Books/books-mod1.xsd"]
qdhxb' ["test/Books/books-mod1.xsd"]

