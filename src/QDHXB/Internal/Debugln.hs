{-# LANGUAGE TemplateHaskell #-}

-- | Project instance of `QDHXB.Internal.Debugln` tracing macro
-- generator.
module QDHXB.Internal.Debugln (
  makeDebuglnFns, makeDebuglnFnsFor, makeDebuglnFnsFixed,
  makeDebuglnBPPFns, makeDebuglnBPPFnsFor, makeDebuglnBPPFnsFixed) where

import QDHXB.Utils.Debugln.BPP
makeDebuglnAndBPPBinders False
