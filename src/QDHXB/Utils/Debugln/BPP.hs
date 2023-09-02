{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

-- | Debugging messages involving `Blockable` values.
module QDHXB.Utils.Debugln.BPP (
  makeDebuglnBPPBinders, makeDebuglnAndBPPBinders,
  dbgBlock_impl, dbgBLabel_impl, dbgBLabelPt_impl,
  dbgResult_impl, dbgResultM_impl,
  dbgBLabelFn1_impl, dbgBLabelFn2_impl, dbgBLabelFn3_impl,
  dbgResultFn1_impl, dbgResultFn2_impl, dbgResultFn3_impl,
  dbgResultFn1M_impl, dbgResultFn2M_impl, dbgResultFn3M_impl,
  fileLocalDebuglnBlockSubject, fileLocalDebuglnBlockCall
  )
where

import Language.Haskell.TH (
  Q, Dec, mkName, DocLoc(DeclDoc), putDoc, Name,
  Exp(AppE, VarE, LitE, ConE, TupE, LamE),
  Lit(StringL, IntegerL),
  Type(ForallT, AppT, ArrowT, ConT),
  Pat(WildP, VarP))
import Language.Haskell.TH.Syntax (addModFinalizer)
import Data.Symbol
import Control.Monad.IO.Class
import QDHXB.Utils.BPP
import QDHXB.Utils.Debugln
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
             blockableValToValCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
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
                  else noop)
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
             blockableValToValCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
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
                            (AppE (VarE $ mkName "Data.Symbol.intern")
                                  (LitE $ StringL s))
                  else noop)
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
             blockableValToValCompTypeIO noop2
             (mkName "QDHXB.Utils.Debugln.BPP.dbgResult_impl")
             "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
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
                                  (AppE (VarE $ mkName "Data.Symbol.intern")
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

-- |Given a monadic computation which will return the result to be
-- returned from another computation, emit debugging information about
-- the result if debugging mode is on.
dbgResultM_impl ::
  (MonadDebugln m n, MonadIO m, Blockable a) =>
    Symbol -> Int -> String -> m a -> m a
{-# INLINE dbgResultM_impl #-}
dbgResultM_impl subj base msg resM = do
  res <- resM
  dbgBLabel_impl subj base (msg ++ " ") res
  return res

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


-- | Create bindings of `QDHXB.Utils.DebuglnBlock` functions fixed to
-- a particular subject.  The `String` argument should be the
-- underlying name of the subject symbol.  The valid entries of the
-- second argument list are: @"dbgBlock"@, @"dbgBLabel"@,
-- @"dbgBLabelPt_impl"@, @"dbgBLabelFn1_impl"@, @"dbgBLabelFn2_impl"@,
-- @"dbgBLabelFn3_impl"@, @"dbgResult@, @"dbgResultM_impl@,
-- @"dbgResultFn1_impl"@, @"dbgResultFn2_impl"@,
-- @"dbgResultFn3_impl"@, @"dbgResultFn1M_impl"@,
-- @"dbgResultFn2M_impl"@, @"dbgResultFn3M_impl"@, @""@, @""@, and
-- @""@.
fileLocalDebuglnBlockSubject :: String -> [String] -> Q [Dec]
fileLocalDebuglnBlockSubject subj = fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgBlock" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBlock")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgBlock_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                           Control.Monad.IO.Class.MonadIO m) =>
                            Int -> QDHXB.Utils.BPP.Block -> m ()
              dbgBlock_impl = QDHXB.Utils.Debugln.BPP.dbgBlock_impl
                                $(return subjExp)
            |]
        f' "dbgBLabel" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabel")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabel :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable c) =>
                             Int -> String -> c -> m ()
              dbgBLabel = QDHXB.Utils.Debugln.BPP.dbgBLabel_impl
                            $(return subjExp)
            |]
        f' "dbgBLabelPt_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelPt_impl")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabelPt_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                              Control.Monad.IO.Class.MonadIO m,
                              QDHXB.Utils.BPP.Blockable c) =>
                               Int -> String -> c -> m ()
              dbgBLabelPt_impl = QDHXB.Utils.Debugln.BPP.dbgBLabelPt_impl
                              $(return subjExp)
            |]
        f' "dbgBLabelFn1_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn1_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn1_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> (a -> r) -> m ()
              dbgBLabelFn1_impl = QDHXB.Utils.Debugln.BPP.dbgBLabelFn1_impl
                               $(return subjExp)
            |]
        f' "dbgBLabelFn2_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn2_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn2_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> b -> (a -> b -> r) -> m ()
              dbgBLabelFn2_impl = QDHXB.Utils.Debugln.BPP.dbgBLabelFn2_impl
                               $(return subjExp)
            |]
        f' "dbgBLabelFn3_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn3_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn3_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String ->
                                  a -> b -> c -> (a -> b -> c -> r) -> m ()
              dbgBLabelFn3_impl = QDHXB.Utils.Debugln.BPP.dbgBLabelFn3_impl
                               $(return subjExp)
            |]
        f' "dbgResult" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResult")
            "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
          [d| dbgResult :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable a) =>
                             Int -> String -> a -> m a
              dbgResult = QDHXB.Utils.Debugln.BPP.dbgResult_impl
                            $(return subjExp)
            |]
        f' "dbgResultM_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultM_impl")
            "For a computation returning some result, emit debugging information about the result if debugging mode is on."
          [d| dbgResultM_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                             Control.Monad.IO.Class.MonadIO m,
                             QDHXB.Utils.BPP.Blockable a) =>
                               Int -> String -> m a -> m a
              dbgResultM_impl = QDHXB.Utils.Debugln.BPP.dbgResultM_impl
                             $(return subjExp)
            |]
        f' "dbgResultFn1_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> (a -> r) -> m (a -> r)
              dbgResultFn1_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn1_impl
                               $(return subjExp)
            |]
        f' "dbgResultFn2_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> b -> (a -> b -> r) ->
                                  m (a -> b -> r)
              dbgResultFn2_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn2_impl
                               $(return subjExp)
            |]
        f' "dbgResultFn3_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String ->
                                  a -> b -> c -> (a -> b -> c -> r) ->
                                    m (a -> b -> c -> r)
              dbgResultFn3_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn3_impl
                               $(return subjExp)
            |]
        f' "dbgResultFn1M_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1M_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1M_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String -> a -> m (a -> r) -> m (a -> r)
              dbgResultFn1M_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn1M_impl
                                $(return subjExp)
            |]
        f' "dbgResultFn2M_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2M_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2M_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String -> a -> b -> m (a -> b -> r) ->
                                   m (a -> b -> r)
              dbgResultFn2M_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn2M_impl
                                $(return subjExp)
            |]
        f' "dbgResultFn3M_impl" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3M_impl")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3M_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String ->
                                   a -> b -> c -> m (a -> b -> c -> r) ->
                                     m (a -> b -> c -> r)
              dbgResultFn3M_impl = QDHXB.Utils.Debugln.BPP.dbgResultFn3M_impl
                                $(return subjExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.DebuglnBlock"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)

-- | Create bindings of `QDHXB.Utils.DebuglnBlock` functions fixed to
-- a particular subject and base detail volume.  The `String` argument
-- should be the underlying name of the subject symbol.  The valid
-- entries of the second argument list are: @"dbgBlock"@,
-- @"dbgBLabel"@, @"dbgBLabelPt_impl"@, @"dbgBLabelFn1_impl"@,
-- @"dbgBLabelFn2_impl"@, @"dbgBLabelFn3_impl"@, @"dbgResult"@,
-- @"dbgResultM_impl"@, @"dbgResultFn1_impl"@, @"dbgResultFn2_impl"@,
-- @"dbgResultFn3_impl"@, @"dbgResultFn1M_impl"@,
-- @"dbgResultFn2M_impl"@, @"dbgResultFn3M_impl"@, @""@, @""@, and
-- @""@.
fileLocalDebuglnBlockCall :: String -> Integer -> [String] -> Q [Dec]
fileLocalDebuglnBlockCall subj base =  fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgBlock" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBlock")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgBlock_impl :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                           Control.Monad.IO.Class.MonadIO m) =>
                            QDHXB.Utils.BPP.Block -> m ()
              dbgBlock_impl = QDHXB.Utils.Debugln.BPP.dbgBlock_impl
                           $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabel" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabel")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabel :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable c) =>
                             String -> c -> m ()
              dbgBLabel = QDHXB.Utils.Debugln.BPP.dbgBLabel_impl
                            $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelPt" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelPt")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabelPt :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                              Control.Monad.IO.Class.MonadIO m,
                              QDHXB.Utils.BPP.Blockable c) =>
                               String -> c -> m ()
              dbgBLabelPt = QDHXB.Utils.Debugln.BPP.dbgBLabelPt_impl
                              $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelFn1" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn1")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn1 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> (a -> r) -> m ()
              dbgBLabelFn1 = QDHXB.Utils.Debugln.BPP.dbgBLabelFn1_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelFn2" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn2")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn2 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> b -> (a -> b -> r) -> m ()
              dbgBLabelFn2 = QDHXB.Utils.Debugln.BPP.dbgBLabelFn2_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelFn3" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn3")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn3 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String ->
                                  a -> b -> c -> (a -> b -> c -> r) -> m ()
              dbgBLabelFn3 = QDHXB.Utils.Debugln.BPP.dbgBLabelFn3_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgResult" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResult")
            "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
          [d| dbgResult :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable a) => String -> a -> m a
              dbgResult = QDHXB.Utils.Debugln.BPP.dbgResult_impl
                            $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultM" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultM")
            "For a computation returning some result, emit debugging information about the result if debugging mode is on."
          [d| dbgResultM :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                             Control.Monad.IO.Class.MonadIO m,
                             QDHXB.Utils.BPP.Blockable a) =>
                               String -> m a -> m a
              dbgResultM = QDHXB.Utils.Debugln.BPP.dbgResultM_impl
                             $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn1" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> (a -> r) ->
                                  m (a -> r)
              dbgResultFn1 = QDHXB.Utils.Debugln.BPP.dbgResultFn1_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn2" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> b -> (a -> b -> r) ->
                                  m (a -> b -> r)
              dbgResultFn2 = QDHXB.Utils.Debugln.BPP.dbgResultFn2_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn3" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String ->
                                  a -> b -> c -> (a -> b -> c -> r) ->
                                    m (a -> b -> c -> r)
              dbgResultFn3 = QDHXB.Utils.Debugln.BPP.dbgResultFn3_impl
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn1M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                 Control.Monad.IO.Class.MonadIO m,
                                 QDHXB.Utils.BPP.Blockable r) =>
                                  String -> a -> m (a -> r) ->
                                    m (a -> r)
              dbgResultFn1M = QDHXB.Utils.Debugln.BPP.dbgResultFn1M_impl
                                $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn2M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 String -> a -> b -> m (a -> b -> r) ->
                                   m (a -> b -> r)
              dbgResultFn2M = QDHXB.Utils.Debugln.BPP.dbgResultFn2M_impl
                                $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultFn3M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 String ->
                                   a -> b -> c -> m (a -> b -> c -> r) ->
                                     m (a -> b -> c -> r)
              dbgResultFn3M = QDHXB.Utils.Debugln.BPP.dbgResultFn3M_impl
                                $(return subjExp) $(return baseExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.DebuglnBlock"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)
        baseExp = LitE $ IntegerL base

        -- subj = intern subjName
