{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.
module QDHXB.Utils.Debugln.TH where

import Language.Haskell.TH (
  Q, Dec, mkName, DocLoc(DeclDoc), putDoc, Name,
  Exp(AppE, VarE, LitE, ConE, TupE, LamE),
  Lit(StringL, IntegerL),
  Type(ForallT, AppT, ArrowT, ConT),
  Pat(WildP, VarP))
import Data.Symbol
import Control.Monad.IO.Class
import QDHXB.Utils.Debugln.Class

pick1of2 :: Exp
pick1of2 = LamE [VarP xName, WildP] $ VarE xName where xName = mkName "xx"

pick2of2 :: Exp
pick2of2 = LamE [WildP, VarP xName] $ VarE xName where xName = mkName "yy"

constFalse :: Exp
constFalse = AppE (VarE $ mkName "const") (ConE 'False)

returnFalse :: Exp
returnFalse = AppE (VarE $ mkName "return") (ConE 'False)

returnVoid :: Exp
returnVoid = AppE (VarE 'return) (TupE [])

constReturnVoid :: Exp
constReturnVoid = AppE (VarE $ mkName "const") returnVoid

noop0 :: Exp
noop0 = AppE (VarE $ mkName "return") (TupE [])

noop1 :: Exp
noop1 = AppE constVarE noop0

noop2 :: Exp
noop2 = AppE constVarE noop1

returnId1 :: Exp
returnId1 = VarE $ mkName "return"

returnId2 :: Exp
returnId2 = AppE constVarE returnId1

constVarE :: Exp
constVarE = VarE $ mkName "const"

addSymbolIntArgs :: Type -> Type
addSymbolIntArgs (ForallT binders cxt typ) = ForallT binders cxt $
  AppT (AppT ArrowT (ConT ''Symbol)) $ AppT (AppT ArrowT (ConT ''Int)) typ

addIntArg :: Type -> Type
addIntArg (ForallT binders cxt typ) = ForallT binders cxt $
  AppT (AppT ArrowT (ConT ''Int)) typ

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
