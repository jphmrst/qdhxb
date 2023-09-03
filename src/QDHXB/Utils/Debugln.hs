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
  -- * Internal and `Data.Symbol` functions used in macro expansions
  module QDHXB.Utils.Debugln.Output, Symbol, intern
  )
where

import Data.Symbol (Symbol, intern)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import QDHXB.Utils.Debugln.Class
import QDHXB.Utils.Debugln.Output
import QDHXB.Utils.Debugln.TH

-- | Bind the names @makeDebuglnFns@, @makeDebuglnFnsForSubject@, and
-- @makeDebuglnFnsFixed@ to create debugging trace functions which
-- respect the given @switch@.
--
-- Functions created by @makeDebuglnFns@ take parameters for both the
-- subject (as a `Data.Symbol`) and the detail level (as an `Int`).
--
-- Functions created by @makeDebuglnFnsForSubject@ take a parameter
-- for the detail level (as an `Int`) only; the symbol passed to
-- @makeDebuglnFnsForSubject@ is then hardcoded to the function calls.
--
-- Functions created by @makeDebuglnFnsFixed@ use both the symbol and
-- level value passed to @makeDebuglnFnsFixed@.
makeDebuglnBinders :: Bool -> Q [Dec]
makeDebuglnBinders switch = do
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnFns")
    "Declare debugger functions to be used without specialization."
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnFnsFor")
    "Create bindings of `QDHXB.Utils.Debugln` functions fixed to a particular subject.  The `String` argument should be the underlying name of the subject symbol.  The valid entries of the second argument list are: @\"dbgLn\"@, and @\"dbgPt\"@."
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnFnsFixed")
    "Create bindings of `QDHXB.Utils.Debugln` functions fixed to a particular subject and base detail volume.  The `String` argument should be the underlying name of the subject symbol.  The valid entries of the second argument list are: @\"dbgLn\"@ and @\"dbgPt\"@."
  [d|

   makeDebuglnFns :: [String] -> Q [Dec]
   makeDebuglnFns =  fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgLn" = buildDelegator "dbgLn" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgLn_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp)
           f' "dbgPt" = buildDelegator "dbgPt" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgPt_impl")
             "Output the given line as a bulleted item in the current level of indentation."
             $(return switchExp)
           f' "whenDebugging" = buildDelegator "whenDebugging"
             qualVoidCompToVoidCompType constReturnVoid
             (mkName "QDHXB.Utils.Debugln.whenDebugging_impl")
             "Pick from subordinated blocks based on whether the debugging master switch is on."
             $(return switchExp)
           f' "indenting" = buildMirror "indenting"
             qualCompToCompType idQ
             (mkName "QDHXB.Utils.Debugln.indenting_impl")
             "Add a level of indentation to debugging output."
             $(return switchExp)
           f' "boxed" = buildMirror "boxed"
             qualCompToCompTypeIO idQ
             (mkName "QDHXB.Utils.Debugln.boxed_impl")
             "Add a level of indentation to debugging output."
             $(return switchExp)
           f' "ifAnyDebugging" = buildMirror "ifAnyDebugging"
             qualTwoCompsToCompTypeIO pick2of2
             (mkName "QDHXB.Utils.Debugln.ifAnyDebugging_impl")
             "Pick from subordinated blocks based on whether the debugging master switch is on."
             $(return switchExp)
           f' "whenAnyDebugging" = buildMirror "whenAnyDebugging"
             qualVoidCompToVoidCompType constReturnVoid
             (mkName "QDHXB.Utils.Debugln.whenAnyDebugging_impl")
             "Pick from subordinated blocks based on whether the debugging master switch is on."
             $(return switchExp)
           f' str = error $
             "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

           buildDelegator ::
             String -> Q Type -> Exp -> Name -> String -> Bool -> Q [Dec]
           buildDelegator fn baseTypQ noop impl doc sw = do
             typ <- fmap addSymbolIntArgs baseTypQ
             addModFinalizer $ putDoc (DeclDoc $ mkName fn) doc
             return [
               SigD nam typ,
               ValD (VarP nam)
                 (NormalB $
                  if sw
                  then (VarE impl)
                  else AppE constVarE $ AppE constVarE noop)
                 []
               ]
             where nam = mkName fn

           buildMirror ::
             String -> Q Type -> Exp -> Name -> String -> Bool -> Q [Dec]
           buildMirror fn baseTypQ noop impl doc sw = do
             typ <- baseTypQ
             addModFinalizer $ putDoc (DeclDoc $ mkName fn) doc
             return [
               SigD nam typ,
               ValD (VarP nam)
                 (NormalB $
                  if sw
                  then (VarE impl)
                  else noop)
                 []
               ]
             where nam = mkName fn

   makeDebuglnFnsFor :: String -> [String] -> Q [Dec]
   makeDebuglnFnsFor subj = fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgLn" = buildDelegator "dbgLn" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgLn_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp) subj
           f' "dbgPt" = buildDelegator "dbgPt" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgPt_impl")
             "Output the given line as a bulleted item in the current level of indentation."
             $(return switchExp) subj
           f' "whenDebugging" = buildDelegator "whenDebugging"
             qualVoidCompToVoidCompType constReturnVoid
             (mkName "QDHXB.Utils.Debugln.whenDebugging_impl")
             "Pick from subordinated blocks based on whether the debugging master switch is on."
             $(return switchExp) subj
           f' str = error $
             "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

           buildDelegator ::
             String -> Q Type -> Exp -> Name -> String -> Bool -> String ->
               Q [Dec]
           buildDelegator fn baseTypQ noop impl doc sw s = do
             typ <- fmap addIntArg baseTypQ
             addModFinalizer $ putDoc (DeclDoc $ mkName fn) doc
             return [
               SigD nam typ,
               ValD (VarP nam)
                 (NormalB $
                  if sw
                  then AppE (VarE impl)
                            (AppE (VarE $ mkName "intern")
                                  (LitE $ StringL s))
                  else AppE constVarE noop)
                 []
               ]
             where nam = mkName fn

   makeDebuglnFnsFixed :: String -> Integer -> [String] -> Q [Dec]
   makeDebuglnFnsFixed subj base =  fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgLn" = buildDelegator "dbgLn" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgLn_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp) subj base
           f' "dbgPt" = buildDelegator "dbgPt" qualStringToVoidTypeIO
             noop1
             (mkName "QDHXB.Utils.Debugln.dbgPt_impl")
             "Output the given line as a bulleted item in the current level of indentation."
             $(return switchExp) subj base
           f' "whenDebugging" = buildDelegator "whenDebugging"
             qualVoidCompToVoidCompType constReturnVoid
             (mkName "QDHXB.Utils.Debugln.whenDebugging_impl")
             "Pick from subordinated blocks based on whether the debugging master switch is on."
             $(return switchExp) subj base
           f' str = error $
             "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

           buildDelegator ::
             String -> Q Type -> Exp -> Name -> String -> Bool ->
               String -> Integer ->
                 Q [Dec]
           buildDelegator fn baseTypQ noop impl doc sw s lv = do
             typ <- baseTypQ
             addModFinalizer $ putDoc (DeclDoc $ mkName fn) doc
             return [
               SigD nam typ,
               ValD (VarP nam)
                 (NormalB $
                  if sw
                  then AppE (AppE (VarE impl)
                                  (AppE (VarE $ mkName "intern")
                                        (LitE $ StringL s)))
                            (LitE $ IntegerL lv)
                  else noop)
                 []
               ]
             where nam = mkName fn
    |]
  where switchExp = ConE $ mkName $ if switch then "True" else "False"
