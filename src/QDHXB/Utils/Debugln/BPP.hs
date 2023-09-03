{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

-- | Debugging messages involving `Blockable` values.
module QDHXB.Utils.Debugln.BPP (
  makeDebuglnBPPBinders, makeDebuglnAndBPPBinders,
  dbgBlock_impl, dbgBLabel_impl, dbgBLabelPt_impl,
  dbgResult_impl, dbgResultM_impl,
  dbgBLabelFn1_impl, dbgBLabelFn2_impl, dbgBLabelFn3_impl,
  dbgResultFn1_impl, dbgResultFn2_impl, dbgResultFn3_impl,
  dbgResultFn1M_impl, dbgResultFn2M_impl, dbgResultFn3M_impl
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import Data.Symbol
import Control.Monad.IO.Class
import QDHXB.Utils.BPP
import QDHXB.Utils.Debugln
import QDHXB.Utils.Debugln.Class
import QDHXB.Utils.Debugln.TH

-- | Bind the names @makeDebuglnBPPFns@, @makeDebuglnBPPFnsFor@, and
-- @makeDebuglnBPPFnsFixed@ to create debugging trace functions which
-- respect the given @switch@.
--
-- Functions created by @makeDebuglnBPPFns@ take parameters for both
-- the subject (as a `Data.Symbol`) and the detail level (as an
-- `Int`).
--
-- Functions created by @makeDebuglnBPPFnsFor@ take a parameter for
-- the detail level (as an `Int`) only; the symbol passed to
-- @makeDebuglnFnsForSubject@ is then hardcoded to the function calls.
--
-- Functions created by @makeDebuglnBPPFnsFixed@ use both the symbol
-- and level value passed to @makeDebuglnBPPFnsFixed@.
makeDebuglnBPPBinders :: Bool -> Q [Dec]
makeDebuglnBPPBinders switch = do
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnBPPFns")
    "Declare debugger functions to be used without specialization."
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnBPPFnsFor")
    "Create bindings of `QDHXB.Utils.Debugln`/`QDHXB.Utils.BPP` functions fixed to a particular subject.  The `String` argument should be the underlying name of the subject symbol.  The valid entries of the second argument list are: @\"dbgLn\"@, and @\"dbgPt\"@."
  addModFinalizer $ putDoc (DeclDoc $ mkName "makeDebuglnBPPFnsFixed")
    "Create bindings of `QDHXB.Utils.Debugln`/`QDHXB.Utils.BPP` functions fixed to a particular subject and base detail volume.  The `String` argument should be the underlying name of the subject symbol.  The valid entries of the second argument list are: @\"dbgLn\"@ and @\"dbgPt\"@."
  [d|

   makeDebuglnBPPFns :: [String] -> Q [Dec]
   makeDebuglnBPPFns =  fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgBlock" = buildDelegator "dbgBlock"
             blockToVoidCompTypeIO noop1
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBlock_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp)
           f' "dbgBLabel" = buildDelegator "dbgBLabel"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabel_impl")
             "Format and output the given value at the current level of indentation, with the given leading label."
             $(return switchExp)
           f' "dbgBLabelPt" = buildDelegator "dbgBLabelPt"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelPt_impl")
             "Format and output the given value as a bullet point at the current level of indentation, with the given leading label."
             $(return switchExp)
           f' "dbgResult" = buildDelegator "dbgResult"
             blockableValToValCompTypeIO returnId2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
             $(return switchExp)

           f' "dbgBLabelFn1" = buildDelegator "dbgBLabelFn1"
             blockableFn1resToCompTypeIO noop3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn1_impl")
             "Given a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgBLabelFn2" = buildDelegator "dbgBLabelFn2"
             blockableFn2resToCompTypeIO noop4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn2_impl")
             "Given a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgBLabelFn3" = buildDelegator "dbgBLabelFn3"
             blockableFn3resToCompTypeIO noop5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn3_impl")
             "Given a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)

           f' "dbgResultFn1" = buildDelegator "dbgResultFn1"
             blockableFn1resToFn1CompTypeIO returnId3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgResultFn2" = buildDelegator "dbgResultFn2"
             blockableFn2resToFn2CompTypeIO returnId4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgResultFn3" = buildDelegator "dbgResultFn3"
             blockableFn3resToFn3CompTypeIO returnId5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)

           f' "dbgResultM" = buildDelegator "dbgResultM"
             blockableLabelledCompToCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultM_impl")
             "Given a monadic computation whose result will be taken as the overall result, emit debugging information about that result if debugging mode is on."
             $(return switchExp)

           f' "dbgResultFn1M" = buildDelegator "dbgResultFn1M"
             blockableFn1CompToFn1CompTypeIO idQ2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1M_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgResultFn2M" = buildDelegator "dbgResultFn2M"
             blockableFn2resToFn2CompTypeIO idQ3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2M_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)
           f' "dbgResultFn3M" = buildDelegator "dbgResultFn3M"
             blockableFn3resToFn3CompTypeIO idQ4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3M_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp)

           f' str = error $
             "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

           buildDelegator ::
             String -> Q Type -> Exp -> Name -> String -> Bool -> Q [Dec]
           buildDelegator fn baseTypQ noop impl doc sw = do
             typ <- baseTypQ
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

   makeDebuglnBPPFnsFor :: String -> [String] -> Q [Dec]
   makeDebuglnBPPFnsFor subj = fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgBlock" = buildDelegator "dbgBlock"
             blockToVoidCompTypeIO noop1
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBlock_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp) subj
           f' "dbgBLabel" = buildDelegator "dbgBLabel"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabel_impl")
             "Format and output the given value at the current level of indentation, with the given leading label."
             $(return switchExp) subj
           f' "dbgBLabelPt" = buildDelegator "dbgBLabelPt"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelPt_impl")
             "Format and output the given value as a bullet point at the current level of indentation, with the given leading label."
             $(return switchExp) subj
           f' "dbgResult" = buildDelegator "dbgResult"
             blockableValToValCompTypeIO returnId2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj

           f' "dbgBLabelFn1" = buildDelegator "dbgBLabelFn1"
             blockableFn1resToCompTypeIO noop3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn1_impl")
             "Given a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgBLabelFn2" = buildDelegator "dbgBLabelFn2"
             blockableFn2resToCompTypeIO noop4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn2_impl")
             "Given a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgBLabelFn3" = buildDelegator "dbgBLabelFn3"
             blockableFn3resToCompTypeIO noop5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn3_impl")
             "Given a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj

           f' "dbgResultFn1" = buildDelegator "dbgResultFn1"
             blockableFn1resToFn1CompTypeIO returnId3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgResultFn2" = buildDelegator "dbgResultFn2"
             blockableFn2resToFn2CompTypeIO returnId4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgResultFn3" = buildDelegator "dbgResultFn3"
             blockableFn3resToFn3CompTypeIO returnId5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj

           f' "dbgResultM" = buildDelegator "dbgResultM"
             blockableLabelledCompToCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultM_impl")
             "Given a monadic computation whose result will be taken as the overall result, emit debugging information about that result if debugging mode is on."
             $(return switchExp) subj

           f' "dbgResultFn1M" = buildDelegator "dbgResultFn1M"
             blockableFn1CompToFn1CompTypeIO idQ2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1M_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgResultFn2M" = buildDelegator "dbgResultFn2M"
             blockableFn2resToFn2CompTypeIO idQ3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2M_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj
           f' "dbgResultFn3M" = buildDelegator "dbgResultFn3M"
             blockableFn3resToFn3CompTypeIO idQ4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3M_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
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

   makeDebuglnBPPFnsFixed :: String -> Integer -> [String] -> Q [Dec]
   makeDebuglnBPPFnsFixed subj base =  fmap concat . mapM f'
     where f' :: String -> Q [Dec]
           f' "dbgBlock" = buildDelegator "dbgBlock"
             blockToVoidCompTypeIO noop1
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBlock_impl")
             "Output the given line in the current level of indentation."
             $(return switchExp) subj base
           f' "dbgBLabel" = buildDelegator "dbgBLabel"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabel_impl")
             "Format and output the given value at the current level of indentation, with the given leading label."
             $(return switchExp) subj base
           f' "dbgBLabelPt" = buildDelegator "dbgBLabelPt"
             blockableToVoidCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelPt_impl")
             "Format and output the given value as a bullet point at the current level of indentation, with the given leading label."
             $(return switchExp) subj base
           f' "dbgResult" = buildDelegator "dbgResult"
             blockableValToValCompTypeIO returnId2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgBLabelFn1" = buildDelegator "dbgBLabelFn1"
             blockableFn1resToCompTypeIO noop3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn1_impl")
             "Given a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgBLabelFn2" = buildDelegator "dbgBLabelFn2"
             blockableFn2resToCompTypeIO noop4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn2_impl")
             "Given a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgBLabelFn3" = buildDelegator "dbgBLabelFn3"
             blockableFn3resToCompTypeIO noop5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgBLabelFn3_impl")
             "Given a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base

           f' "dbgResultFn1" = buildDelegator "dbgResultFn1"
             blockableFn1resToFn1CompTypeIO returnId3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgResultFn2" = buildDelegator "dbgResultFn2"
             blockableFn2resToFn2CompTypeIO returnId4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgResultFn3" = buildDelegator "dbgResultFn3"
             blockableFn3resToFn3CompTypeIO returnId5
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base

           f' "dbgResultM" = buildDelegator "dbgResultM"
             blockableLabelledCompToCompTypeIO idQ1
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultM_impl")
             "Given a monadic computation whose result will be taken as the overall result, emit debugging information about that result if debugging mode is on."
             $(return switchExp) subj base

           f' "dbgResultFn1M" = buildDelegator "dbgResultFn1M"
             blockableFn1CompToFn1CompTypeIO idQ2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn1M_impl")
             "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgResultFn2M" = buildDelegator "dbgResultFn2M"
             blockableFn2resToFn2CompTypeIO idQ3
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn2M_impl")
             "Given a computation returning a function of twu arguments, emit debugging information about it if debugging mode is on."
             $(return switchExp) subj base
           f' "dbgResultFn3M" = buildDelegator "dbgResultFn3M"
             blockableFn3resToFn3CompTypeIO idQ4
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResultFn3M_impl")
             "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
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

-- | Introduce both the core and BPP bindings functions (see
-- `makeDebuglnBinders` and `makeDebuglnBPPBinders`).
makeDebuglnAndBPPBinders :: Bool -> Q [Dec]
makeDebuglnAndBPPBinders switch = do
  coreDecs <- makeDebuglnBinders switch
  bppDecs <- makeDebuglnBPPBinders switch
  return $ coreDecs ++ bppDecs

-- |Output the given line in the current level of indentation.
dbgBlock_impl :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> Block -> m ()
dbgBlock_impl subj base b = dbgLn_impl subj base $ outBlock b

blockToVoidCompTypeIO :: Q Type
blockToVoidCompTypeIO =
  [t| forall m n . (MonadDebugln m n, MonadIO m) => Block -> m () |]

-- | Format and output the given value at the current level of
-- indentation, with the given leading label.
dbgBLabel_impl ::
  (MonadDebugln m n, MonadIO m, Blockable c) =>
    Symbol -> Int -> String -> c -> m ()
dbgBLabel_impl subj base s m =
  dbgLn_impl subj base $ outBlock $ labelBlock s $ block m

blockableToVoidCompTypeIO :: Q Type
blockableToVoidCompTypeIO =
  [t| forall m n c .
      (MonadDebugln m n, MonadIO m, Blockable c) => String -> c -> m () |]

-- | Format and output the given value as a bullet point at the
-- current level of indentation, with the given leading label.
dbgBLabelPt_impl ::
  (MonadDebugln m n, MonadIO m, Blockable c) =>
    Symbol -> Int -> String -> c -> m ()
dbgBLabelPt_impl subj base s m =
  dbgPt_impl subj base $ outBlock $ labelBlock s $ block m

-- | Given a result to be returned from a computation, emit debugging
-- information about it if debugging mode is on.
dbgResult_impl ::
  (MonadDebugln m n, MonadIO m, Blockable a) =>
    Symbol -> Int -> String -> a -> m a
{-# INLINE dbgResult_impl #-}
dbgResult_impl subj base msg res = do
  dbgBLabel_impl subj base (msg ++ " ") res
  return res

blockableValToValCompTypeIO :: Q Type
blockableValToValCompTypeIO =
  [t| forall m n a .
      (MonadDebugln m n, MonadIO m, Blockable a) => String -> a -> m a |]

-- |Given a function of one argument, emit debugging information about
-- it if debugging mode is on.
dbgBLabelFn1_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> (a -> r) -> m ()
dbgBLabelFn1_impl subj base label arg fn =
  dbgBLabel_impl subj base label $ fn arg

-- |Given a function of two arguments, emit debugging information
-- about it if debugging mode is on.
dbgBLabelFn2_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> (a -> b -> r) -> m ()
dbgBLabelFn2_impl subj base label a1 a2 fn =
  dbgBLabel_impl subj base label $ fn a1 a2

-- |Given a function of three arguments, emit debugging information
-- about it if debugging mode is on.
dbgBLabelFn3_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) -> m ()
dbgBLabelFn3_impl subj base label a1 a2 a3 fn =
  dbgBLabel_impl subj base label $ fn a1 a2 a3

blockableFn1resToCompTypeIO :: Q Type
blockableFn1resToCompTypeIO =
  [t| forall m n a r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> (a -> r) -> m () |]

blockableFn2resToCompTypeIO :: Q Type
blockableFn2resToCompTypeIO =
  [t| forall m n a b r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> (a -> b -> r) -> m () |]

blockableFn3resToCompTypeIO :: Q Type
blockableFn3resToCompTypeIO =
  [t| forall m n a b c r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> c -> (a -> b -> c -> r) -> m () |]

-- |Given a computation returning a function of one argument, emit
-- debugging information about it if debugging mode is on.
dbgResultFn1_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> (a -> r) -> m (a -> r)
dbgResultFn1_impl subj base label arg fn = do
  dbgBLabel_impl subj base label $ fn arg
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn2_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
dbgResultFn2_impl subj base label a1 a2 fn = do
  dbgBLabel_impl subj base label $ fn a1 a2
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn3_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) ->
      m (a -> b -> c -> r)
dbgResultFn3_impl subj base label a1 a2 a3 fn = do
  dbgBLabel_impl subj base label $ fn a1 a2 a3
  return fn

blockableFn1resToFn1CompTypeIO :: Q Type
blockableFn1resToFn1CompTypeIO =
  [t| forall m n a r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> (a -> r) -> m (a -> r) |]

blockableFn2resToFn2CompTypeIO :: Q Type
blockableFn2resToFn2CompTypeIO =
  [t| forall m n a b r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> (a -> b -> r) -> m (a -> b -> r) |]

blockableFn3resToFn3CompTypeIO :: Q Type
blockableFn3resToFn3CompTypeIO =
  [t| forall m n a b c r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> c -> (a -> b -> c -> r) -> m (a -> b -> c -> r) |]

-- |Given a monadic computation whose result will be taken as the
-- overall result, emit debugging information about that result if
-- debugging mode is on.
dbgResultM_impl ::
  (MonadDebugln m n, MonadIO m, Blockable a) =>
    Symbol -> Int -> String -> m a -> m a
{-# INLINE dbgResultM_impl #-}
dbgResultM_impl subj base msg resM = do
  res <- resM
  dbgBLabel_impl subj base (msg ++ " ") res
  return res

blockableLabelledCompToCompTypeIO :: Q Type
blockableLabelledCompToCompTypeIO =
  [t| forall m n a .
      (QDHXB.Utils.Debugln.Class.MonadDebugln m n,
       Control.Monad.IO.Class.MonadIO m,
       Blockable a) =>
        String -> m a -> m a |]

-- |Given a computation returning a function of one argument, emit
-- debugging information about it if debugging mode is on.
dbgResultFn1M_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> m (a -> r) -> m (a -> r)
dbgResultFn1M_impl subj base label arg fnM = do
  fn <- fnM
  dbgBLabel_impl subj base label $ fn arg
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn2M_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> m (a -> b -> r) -> m (a -> b -> r)
dbgResultFn2M_impl subj base label a1 a2 fnM = do
  fn <- fnM
  dbgBLabel_impl subj base label $ fn a1 a2
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn3M_impl ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> m (a -> b -> c -> r) ->
      m (a -> b -> c -> r)
dbgResultFn3M_impl subj base label a1 a2 a3 fnM = do
  fn <- fnM
  dbgBLabel_impl subj base label $ fn a1 a2 a3
  return fn

blockableFn1CompToFn1CompTypeIO :: Q Type
blockableFn1CompToFn1CompTypeIO =
  [t| forall m n a r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> m (a -> r) -> m (a -> r) |]

blockableFn2CompToFn2CompTypeIO :: Q Type
blockableFn2CompToFn2CompTypeIO =
  [t| forall m n a b r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> m (a -> b -> r) -> m (a -> b -> r) |]

blockableFn3CompToFn3CompTypeIO :: Q Type
blockableFn3CompToFn3CompTypeIO =
  [t| forall m n a b c r .
      (MonadDebugln m n, MonadIO m, Blockable r) =>
        String -> a -> b -> c -> m (a -> b -> c -> r) -> m (a -> b -> c -> r) |]
