{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Project instance of `QDHXB.Internal.Debugln` tracing macro
-- generator.
module QDHXB.Internal.Debugln (module QDHXB.Internal.Debugln) where

import QDHXB.Utils.Debugln
import QDHXB.Utils.Debugln.BPP
makeDebuglnDefs True
makeDebuglnBPPDefs True

xsdq, flattening, generate, input, unique, blocks, l0 :: Subject
-- | Canonical instance for a QDHXB debugging subject.
xsdq = subject "xsdq"
-- | Canonical instance for a QDHXB debugging subject.
flattening = subject "flatten"
-- | Canonical instance for a QDHXB debugging subject.
generate = subject "generate"
-- | Canonical instance for a QDHXB debugging subject.
input = subject "input"
-- | Canonical instance for a QDHXB debugging subject.
unique = subject "unique"
-- | Canonical instance for a QDHXB debugging subject.
blocks = subject "block"
-- | Canonical instance for a QDHXB debugging subject.
l0 = subject "L0"
