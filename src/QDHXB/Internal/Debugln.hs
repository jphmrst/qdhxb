{-# LANGUAGE TemplateHaskell #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
module QDHXB.Internal.Debugln (
  makeDebuglnFns, makeDebuglnFnsFor, makeDebuglnFnsFixed,
  makeDebuglnBPPFns, makeDebuglnBPPFnsFor, makeDebuglnBPPFnsFixed) where

import Language.Haskell.TH
import QDHXB.Utils.Debugln.BPP

makeDebuglnAndBPPBinders True
