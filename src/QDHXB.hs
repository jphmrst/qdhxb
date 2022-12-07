{-# LANGUAGE TemplateHaskell #-}

module QDHXB (qdhxb) where

import Language.Haskell.TH

qdhxb :: Q [Dec]
qdhxb = [d| x :: Int; x = 1 |]

