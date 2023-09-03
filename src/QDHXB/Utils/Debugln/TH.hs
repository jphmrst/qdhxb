{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

-- | Template Haskell definitions for quoted expressions and types
-- used in the various `QDHXB.Utils.Debugln` modules.
module QDHXB.Utils.Debugln.TH (module QDHXB.Utils.Debugln.TH)
where

import Language.Haskell.TH
import Data.Symbol
import Control.Monad.IO.Class
import QDHXB.Utils.Debugln.Class

pick1of2 :: Exp
pick1of2 = LamE [VarP xName, WildP] $ VarE xName where xName = mkName "xx"

pick2of2 :: Exp
pick2of2 = LamE [WildP, VarP xName] $ VarE xName where xName = mkName "yy"

constFalse :: Exp
constFalse = AppE constVarE (ConE 'False)

idQ :: Exp
idQ = VarE $ mkName "id"

idQ1 :: Exp
idQ1 = AppE constVarE idQ

idQ2 :: Exp
idQ2 = AppE constVarE idQ1

idQ3 :: Exp
idQ3 = AppE constVarE idQ2

idQ4 :: Exp
idQ4 = AppE constVarE idQ3

returnFalse :: Exp
returnFalse = AppE (VarE $ mkName "return") (ConE 'False)

returnVoid :: Exp
returnVoid = AppE (VarE 'return) (TupE [])

constReturnVoid :: Exp
constReturnVoid = AppE constVarE returnVoid

noop0 :: Exp
noop0 = AppE (VarE $ mkName "return") (TupE [])

noop1 :: Exp
noop1 = AppE constVarE noop0

noop2 :: Exp
noop2 = AppE constVarE noop1

noop3 :: Exp
noop3 = AppE constVarE noop2

noop4 :: Exp
noop4 = AppE constVarE noop3

noop5 :: Exp
noop5 = AppE constVarE noop4

returnId1 :: Exp
returnId1 = VarE $ mkName "return"

returnId2 :: Exp
returnId2 = AppE constVarE returnId1

returnId3 :: Exp
returnId3 = AppE constVarE returnId2

returnId4 :: Exp
returnId4 = AppE constVarE returnId3

returnId5 :: Exp
returnId5 = AppE constVarE returnId4

constVarE :: Exp
constVarE = VarE $ mkName "const"

addSymbolIntArgs :: Type -> Type
addSymbolIntArgs (ForallT binders ctxt typ) = ForallT binders ctxt $
  AppT (AppT ArrowT (ConT ''Symbol)) $ AppT (AppT ArrowT (ConT ''Int)) typ
addSymbolIntArgs _ =
  error "Unexpected argument (non-ForallT) to addSymbolIntArgs"

addIntArg :: Type -> Type
addIntArg (ForallT binders ctxt typ) = ForallT binders ctxt $
  AppT (AppT ArrowT (ConT ''Int)) typ
addIntArg _ =
  error "Unexpected argument (non-ForallT) to addIntArgs"

qualBoolCompTypeIO :: Q Type
qualBoolCompTypeIO = [t| forall m n .
                               (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m) =>
                                 m Bool |]

qualBoolCompType :: Q Type
qualBoolCompType =
  [t| forall m n . QDHXB.Utils.Debugln.Class.MonadDebugln m n => m Bool |]

qualCompToCompTypeIO :: Q Type
qualCompToCompTypeIO = [t| forall m n a .
                               (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m) =>
                                 m a -> m a |]

qualCompToCompType :: Q Type
qualCompToCompType = [t| forall m n a .
                               QDHXB.Utils.Debugln.Class.MonadDebugln m n =>
                                 m a -> m a |]

qualVoidCompToVoidCompType :: Q Type
qualVoidCompToVoidCompType = [t| forall m n .
                               QDHXB.Utils.Debugln.Class.MonadDebugln m n =>
                                 m () -> m () |]

qualVoidCompToVoidCompTypeIO :: Q Type
qualVoidCompToVoidCompTypeIO = [t| forall m n .
                               (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m) =>
                                 m () -> m () |]

qualTwoCompsToCompTypeIO :: Q Type
qualTwoCompsToCompTypeIO = [t| forall m n a .
                           (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m) =>
                             m a -> m a -> m a |]

qualStringToVoidTypeIO :: Q Type
qualStringToVoidTypeIO = [t| forall m n .
                  (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
                   Control.Monad.IO.Class.MonadIO m) =>
                     String -> m () |]
