{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances#-}
{-# LANGUAGE KindSignatures, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.
module QDHXB.Utils.Debugln (
  -- * Core control
  Debugln(..), MonadDebugln, liftDebugln, runDebugln, getIndentation,
  -- * File-local definitions
  makeDebuglnBinders,
  -- * Internal functions used in macro expansions
  dbgLn_impl, dbgPt_impl,
  getDebugging_impl,
  whenDebugging_impl, whenAnyDebugging_impl, ifAnyDebugging_impl
  )
where

import QDHXB.Utils.Debugln.Class
import QDHXB.Utils.Debugln.Output
import QDHXB.Utils.Debugln.Binders
