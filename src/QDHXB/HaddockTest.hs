{-# LANGUAGE TemplateHaskell #-}

module QDHXB.HaddockTest where
import Data.Time.Calendar
import QDHXB
import qualified QDHXB.Expansions

-- qdhxb useDebugging ["books-mod1.xsd"]
qdhxb' ["books-mod1.xsd"]

