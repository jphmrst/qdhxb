{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances#-}
{-# LANGUAGE KindSignatures, UndecidableInstances, TypeApplications, ScopedTypeVariables #-}

-- | Project instance of `QDHXB.Internal.Debugln` tracing macro
-- generator.
module QDHXB.Internal.Debugln (module QDHXB.Internal.Debugln) where

import QDHXB.Utils.Debugln
import QDHXB.Utils.Debugln.BPP
makeDebuglnDefs True
makeDebuglnBPPDefs True

xsdq, flattening, generate, input, unique, blocks, l0 :: Subject
xsdq = subject "xsdq"
flattening = subject "flatten"
generate = subject "generate"
input = subject "input"
unique = subject "unique"
blocks = subject "block"
l0 = subject "L0"
