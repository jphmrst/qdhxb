{-# LANGUAGE TemplateHaskell #-}

-- | Debugging messages involving `Blockable` values.
module QDHXB.Utils.DebuglnBlock (
  dbgBlock, dbgBLabel, dbgBLabelPt,
  dbgResult, dbgResultM,
  dbgBLabelFn1, dbgBLabelFn2, dbgBLabelFn3,
  dbgResultFn1, dbgResultFn2, dbgResultFn3,
  dbgResultFn1M, dbgResultFn2M, dbgResultFn3M,
  fileLocalDebuglnBlockSubject, fileLocalDebuglnBlockCall
  )
where

import Language.Haskell.TH (
  Q, Dec, Exp(AppE, VarE, LitE), Lit(StringL, IntegerL),
  mkName, DocLoc(DeclDoc), putDoc)
import Language.Haskell.TH.Syntax (addModFinalizer)
import Data.Symbol
import Control.Monad.IO.Class
import QDHXB.Utils.Debugln
import QDHXB.Utils.BPP

-- |Output the given line in the current level of indentation.
dbgBlock :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> Block -> m ()
dbgBlock subj base b = dbgLn subj base $ outBlock b

-- | Format and output the given value at the current level of
-- indentation, with the given leading label.
dbgBLabel ::
  (MonadDebugln m n, MonadIO m, Blockable c) =>
    Symbol -> Int -> String -> c -> m ()
dbgBLabel subj base s m = dbgLn subj base $ outBlock $ labelBlock s $ block m

-- | Format and output the given value at the current level of
-- indentation, with the given leading label.
dbgBLabelPt ::
  (MonadDebugln m n, MonadIO m, Blockable c) =>
    Symbol -> Int -> String -> c -> m ()
dbgBLabelPt subj base s m =
  dbgPt subj base $ outBlock $ labelBlock s $ block m

-- |Given a result to be returned from a computation, emit debugging
-- information about it if debugging mode is on.
dbgResult ::
  (MonadDebugln m n, MonadIO m, Blockable a) =>
    Symbol -> Int -> String -> a -> m a
{-# INLINE dbgResult #-}
dbgResult subj base msg res = do
  dbgBLabel subj base (msg ++ " ") res
  return res

-- |Given a function of one argument, emit debugging information about
-- it if debugging mode is on.
dbgBLabelFn1 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> (a -> r) -> m ()
dbgBLabelFn1 subj base label arg fn = dbgBLabel subj base label $ fn arg

-- |Given a function of two arguments, emit debugging information
-- about it if debugging mode is on.
dbgBLabelFn2 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> (a -> b -> r) -> m ()
dbgBLabelFn2 subj base label a1 a2 fn = dbgBLabel subj base label $ fn a1 a2

-- |Given a function of three arguments, emit debugging information
-- about it if debugging mode is on.
dbgBLabelFn3 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) -> m ()
dbgBLabelFn3 subj base label a1 a2 a3 fn =
  dbgBLabel subj base label $ fn a1 a2 a3

-- |Given a computation returning a function of one argument, emit
-- debugging information about it if debugging mode is on.
dbgResultFn1 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> (a -> r) -> m (a -> r)
dbgResultFn1 subj base label arg fn = do
  dbgBLabel subj base label $ fn arg
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn2 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
dbgResultFn2 subj base label a1 a2 fn = do
  dbgBLabel subj base label $ fn a1 a2
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn3 ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) ->
      m (a -> b -> c -> r)
dbgResultFn3 subj base label a1 a2 a3 fn = do
  dbgBLabel subj base label $ fn a1 a2 a3
  return fn

-- |Given a monadic computation which will return the result to be
-- returned from another computation, emit debugging information about
-- the result if debugging mode is on.
dbgResultM ::
  (MonadDebugln m n, MonadIO m, Blockable a) =>
    Symbol -> Int -> String -> m a -> m a
{-# INLINE dbgResultM #-}
dbgResultM subj base msg resM = do
  res <- resM
  dbgBLabel subj base (msg ++ " ") res
  return res

-- |Given a computation returning a function of one argument, emit
-- debugging information about it if debugging mode is on.
dbgResultFn1M ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> m (a -> r) -> m (a -> r)
dbgResultFn1M subj base label arg fnM = do
  fn <- fnM
  dbgBLabel subj base label $ fn arg
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn2M ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> m (a -> b -> r) -> m (a -> b -> r)
dbgResultFn2M subj base label a1 a2 fnM = do
  fn <- fnM
  dbgBLabel subj base label $ fn a1 a2
  return fn

-- |Given a computation returning a function of two arguments, emit
-- debugging information about it if debugging mode is on.
dbgResultFn3M ::
  (MonadDebugln m n, MonadIO m, Blockable r) =>
    Symbol -> Int -> String -> a -> b -> c -> m (a -> b -> c -> r) ->
      m (a -> b -> c -> r)
dbgResultFn3M subj base label a1 a2 a3 fnM = do
  fn <- fnM
  dbgBLabel subj base label $ fn a1 a2 a3
  return fn


-- | Create bindings of `QDHXB.Utils.DebuglnBlock` functions fixed to
-- a particular subject.  The `String` argument should be the
-- underlying name of the subject symbol.  The valid entries of the
-- second argument list are: @"dbgBlock"@, @"dbgBLabel"@,
-- @"dbgBLabelPt"@, @"dbgBLabelFn1"@, @"dbgBLabelFn2"@,
-- @"dbgBLabelFn3"@, @"dbgResult@, @"dbgResultM@, @"dbgResultFn1"@,
-- @"dbgResultFn2"@, @"dbgResultFn3"@, @"dbgResultFn1M"@,
-- @"dbgResultFn2M"@, @"dbgResultFn3M"@, @""@, @""@, and @""@.
fileLocalDebuglnBlockSubject :: String -> [String] -> Q [Dec]
fileLocalDebuglnBlockSubject subj = fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgBlock" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBlock")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgBlock :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                           Control.Monad.IO.Class.MonadIO m) =>
                            Int -> QDHXB.Utils.BPP.Block -> m ()
              dbgBlock = QDHXB.Utils.DebuglnBlock.dbgBlock $(return subjExp)
            |]
        f' "dbgBLabel" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabel")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabel :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable c) =>
                             Int -> String -> c -> m ()
              dbgBLabel = QDHXB.Utils.DebuglnBlock.dbgBLabel $(return subjExp)
            |]
        f' "dbgBLabelPt" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelPt")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabelPt :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                              Control.Monad.IO.Class.MonadIO m,
                              QDHXB.Utils.BPP.Blockable c) =>
                               Int -> String -> c -> m ()
              dbgBLabelPt = QDHXB.Utils.DebuglnBlock.dbgBLabelPt
                              $(return subjExp)
            |]
        f' "dbgBLabelFn1" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn1")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn1 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> (a -> r) -> m ()
              dbgBLabelFn1 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn1
                               $(return subjExp)
            |]
        f' "dbgBLabelFn2" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn2")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn2 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> b -> (a -> b -> r) -> m ()
              dbgBLabelFn2 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn2
                               $(return subjExp)
            |]
        f' "dbgBLabelFn3" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn3")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn3 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String ->
                                  a -> b -> c -> (a -> b -> c -> r) -> m ()
              dbgBLabelFn3 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn3
                               $(return subjExp)
            |]
        f' "dbgResult" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResult")
            "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
          [d| dbgResult :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable a) =>
                             Int -> String -> a -> m a
              dbgResult = QDHXB.Utils.DebuglnBlock.dbgResult
                            $(return subjExp)
            |]
        f' "dbgResultM" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultM")
            "For a computation returning some result, emit debugging information about the result if debugging mode is on."
          [d| dbgResultM :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                             Control.Monad.IO.Class.MonadIO m,
                             QDHXB.Utils.BPP.Blockable a) =>
                               Int -> String -> m a -> m a
              dbgResultM = QDHXB.Utils.DebuglnBlock.dbgResultM
                             $(return subjExp)
            |]
        f' "dbgResultFn1" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> (a -> r) -> m (a -> r)
              dbgResultFn1 = QDHXB.Utils.DebuglnBlock.dbgResultFn1
                               $(return subjExp)
            |]
        f' "dbgResultFn2" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String -> a -> b -> (a -> b -> r) ->
                                  m (a -> b -> r)
              dbgResultFn2 = QDHXB.Utils.DebuglnBlock.dbgResultFn2
                               $(return subjExp)
            |]
        f' "dbgResultFn3" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                Int -> String ->
                                  a -> b -> c -> (a -> b -> c -> r) ->
                                    m (a -> b -> c -> r)
              dbgResultFn3 = QDHXB.Utils.DebuglnBlock.dbgResultFn3
                               $(return subjExp)
            |]
        f' "dbgResultFn1M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn1M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn1M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String -> a -> m (a -> r) -> m (a -> r)
              dbgResultFn1M = QDHXB.Utils.DebuglnBlock.dbgResultFn1M
                                $(return subjExp)
            |]
        f' "dbgResultFn2M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn2M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn2M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String -> a -> b -> m (a -> b -> r) ->
                                   m (a -> b -> r)
              dbgResultFn2M = QDHXB.Utils.DebuglnBlock.dbgResultFn2M
                                $(return subjExp)
            |]
        f' "dbgResultFn3M" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultFn3M")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgResultFn3M :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                                Control.Monad.IO.Class.MonadIO m,
                                QDHXB.Utils.BPP.Blockable r) =>
                                 Int -> String ->
                                   a -> b -> c -> m (a -> b -> c -> r) ->
                                     m (a -> b -> c -> r)
              dbgResultFn3M = QDHXB.Utils.DebuglnBlock.dbgResultFn3M
                                $(return subjExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.DebuglnBlock"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)

-- | Create bindings of `QDHXB.Utils.DebuglnBlock` functions fixed to
-- a particular subject and base detail volume.  The `String` argument
-- should be the underlying name of the subject symbol.  The valid
-- entries of the second argument list are: @"dbgBlock"@,
-- @"dbgBLabel"@, @"dbgBLabelPt"@, @"dbgBLabelFn1"@, @"dbgBLabelFn2"@,
-- @"dbgBLabelFn3"@, @"dbgResult"@, @"dbgResultM"@, @"dbgResultFn1"@,
-- @"dbgResultFn2"@, @"dbgResultFn3"@, @"dbgResultFn1M"@,
-- @"dbgResultFn2M"@, @"dbgResultFn3M"@, @""@, @""@, and @""@.
fileLocalDebuglnBlockCall :: String -> Integer -> [String] -> Q [Dec]
fileLocalDebuglnBlockCall subj base =  fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgBlock" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBlock")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgBlock :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                           Control.Monad.IO.Class.MonadIO m) =>
                            QDHXB.Utils.BPP.Block -> m ()
              dbgBlock = QDHXB.Utils.DebuglnBlock.dbgBlock
                           $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabel" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabel")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabel :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable c) =>
                             String -> c -> m ()
              dbgBLabel = QDHXB.Utils.DebuglnBlock.dbgBLabel
                            $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelPt" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelPt")
            "Format and output the given value at the current level of indentation, with the given leading label."
          [d| dbgBLabelPt :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                              Control.Monad.IO.Class.MonadIO m,
                              QDHXB.Utils.BPP.Blockable c) =>
                               String -> c -> m ()
              dbgBLabelPt = QDHXB.Utils.DebuglnBlock.dbgBLabelPt
                              $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelFn1" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn1")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn1 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> (a -> r) -> m ()
              dbgBLabelFn1 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn1
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgBLabelFn2" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgBLabelFn2")
            "Given a function of one argument, emit debugging information about it if debugging mode is on."
          [d| dbgBLabelFn2 :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                               Control.Monad.IO.Class.MonadIO m,
                               QDHXB.Utils.BPP.Blockable r) =>
                                String -> a -> b -> (a -> b -> r) -> m ()
              dbgBLabelFn2 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn2
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
              dbgBLabelFn3 = QDHXB.Utils.DebuglnBlock.dbgBLabelFn3
                               $(return subjExp) $(return baseExp)
            |]
        f' "dbgResult" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResult")
            "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
          [d| dbgResult :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                            Control.Monad.IO.Class.MonadIO m,
                            QDHXB.Utils.BPP.Blockable a) => String -> a -> m a
              dbgResult = QDHXB.Utils.DebuglnBlock.dbgResult
                            $(return subjExp) $(return baseExp)
            |]
        f' "dbgResultM" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgResultM")
            "For a computation returning some result, emit debugging information about the result if debugging mode is on."
          [d| dbgResultM :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                             Control.Monad.IO.Class.MonadIO m,
                             QDHXB.Utils.BPP.Blockable a) =>
                               String -> m a -> m a
              dbgResultM = QDHXB.Utils.DebuglnBlock.dbgResultM
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
              dbgResultFn1 = QDHXB.Utils.DebuglnBlock.dbgResultFn1
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
              dbgResultFn2 = QDHXB.Utils.DebuglnBlock.dbgResultFn2
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
              dbgResultFn3 = QDHXB.Utils.DebuglnBlock.dbgResultFn3
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
              dbgResultFn1M = QDHXB.Utils.DebuglnBlock.dbgResultFn1M
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
              dbgResultFn2M = QDHXB.Utils.DebuglnBlock.dbgResultFn2M
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
              dbgResultFn3M = QDHXB.Utils.DebuglnBlock.dbgResultFn3M
                                $(return subjExp) $(return baseExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.DebuglnBlock"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)
        baseExp = LitE $ IntegerL base

        -- subj = intern subjName
