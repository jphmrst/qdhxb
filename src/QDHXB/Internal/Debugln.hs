{-# LANGUAGE TemplateHaskell #-}

-- | Project instance of `QDHXB.Internal.Debugln` tracing macro
-- generator.
module QDHXB.Internal.Debugln (
  makeDebuglnFns, makeDebuglnFnsFor, makeDebuglnFnsFixed,
  makeDebuglnBPPFns, makeDebuglnBPPFnsFor, makeDebuglnBPPFnsFixed,
  unique, xsdq, l0, flattening, generate, input)
where

import QDHXB.Utils.Debugln
import QDHXB.Utils.Debugln.BPP
makeDebuglnAndBPPBinders True

unique, xsdq, l0, flattening, generate, input :: Symbol
unique = intern "unique"
xsdq = intern "xsdq"
l0 = intern "l0"
flattening = intern "flattening"
generate = intern "generate"
input = intern "input"
