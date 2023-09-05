{-# LANGUAGE TemplateHaskell, ExplicitForAll, TypeApplications, ScopedTypeVariables #-}

-- | Debugging messages involving `Blockable` values.
module QDHXB.Utils.Debugln.BPP (
  makeDebuglnBPPDefs, makeDebuglnAndBPPDefs
  {-
  makeDebuglnBPPBinders, makeDebuglnAndBPPBinders,
  dbgBlock_impl, dbgBLabel_impl, dbgBLabelPt_impl,
  dbgResult_impl, dbgResultM_impl,
  dbgBLabelFn1_impl, dbgBLabelFn2_impl, dbgBLabelFn3_impl,
  dbgResultFn1_impl, dbgResultFn2_impl, dbgResultFn3_impl,
  dbgResultFn1M_impl, dbgResultFn2M_impl, dbgResultFn3M_impl
  -}
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import Control.Monad.IO.Class
import QDHXB.Utils.BPP
-- import QDHXB.Utils.Debugln
import QDHXB.Utils.Debugln
-- import QDHXB.Utils.Debugln.Class
-- import QDHXB.Utils.Debugln.TH

-- | Introduce @Debugln@ tracing functions for `Blockable` values into
-- a module.  The @switch@ argument determines whether any
-- output-generating code should actually be produced.
makeDebuglnBPPDefs :: Bool -> Q [Dec]
makeDebuglnBPPDefs switch =   do
  declDoc "dbgBlock" "Output the given line in the current level of indentation."
  declDoc "dbgBLabel" "Format and output the given value at the current level of indentation, with the given leading label."
  declDoc "dbgBLabelPt" "Format and output the given value as a bullet point at the current level of indentation, with the given leading label."
  declDoc "dbgResult" "Given a result to be returned from a computation, emit debugging information about it if debugging mode is on."
  declDoc "dbgBLabelFn1" "Given a function of one argument, emit debugging information about it if debugging mode is on."
  declDoc "dbgBLabelFn2" "Given a function of two arguments, emit debugging information about it if debugging mode is on."
  declDoc "dbgBLabelFn3" "Given a function of three arguments, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultFn1" "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultFn2" "Given a computation returning a function of two arguments, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultFn3" "Given a computation returning a function of three arguments, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultM" "Given a monadic computation whose result will be taken as the overall result, emit debugging information about that result if debugging mode is on."
  declDoc "dbgResultFn1M" "Given a computation returning a function of one argument, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultFn2M" "Given a computation returning a function of two arguments, emit debugging information about it if debugging mode is on."
  declDoc "dbgResultFn3M" "Given a computation returning a function of two arguments, emit debugging information about it if debugging mode is on."
  monadDebugLnMaybe <- lookupTypeName "MonadDebugln"
  let monadDebugLn =
        maybe (error "MonadDebugln not defined") id monadDebugLnMaybe
      md = return $ ConT monadDebugLn
  if switch
  then
    [d|
      dbgBlock :: forall m n . ($md m n, MonadIO m) =>
                    Subject -> Int -> Block -> m ()
      dbgBlock subj base b = dbgLn @m @n subj base $ outBlock b

      dbgBLabel :: forall m n c . ($md m n, MonadIO m, Blockable c) =>
                     Subject -> Int -> String -> c -> m ()
      dbgBLabel subj base s m =
        dbgLn @m @n subj base $ outBlock $ labelBlock s $ block m

      dbgBLabelPt :: ($md m n, MonadIO m, Blockable c) =>
                       Subject -> Int -> String -> c -> m ()
      dbgBLabelPt subj base s m =
        dbgPt subj base $ outBlock $ labelBlock s $ block m

      dbgResult :: ($md m n, MonadIO m, Blockable a) =>
                     Subject -> Int -> String -> a -> m a
      {-# INLINE dbgResult #-}
      dbgResult subj base msg res = do
        dbgBLabel subj base (msg ++ " ") res
        return res

      dbgBLabelFn1 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> (a -> r) -> m ()
      dbgBLabelFn1 subj base label arg fn =
        dbgBLabel subj base label $ fn arg

      dbgBLabelFn2 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> (a -> b -> r) -> m ()
      dbgBLabelFn2 subj base label a1 a2 fn =
        dbgBLabel subj base label $ fn a1 a2

      dbgBLabelFn3 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) -> m ()
      dbgBLabelFn3 subj base label a1 a2 a3 fn =
        dbgBLabel subj base label $ fn a1 a2 a3

      dbgResultFn1 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> (a -> r) -> m (a -> r)
      dbgResultFn1 subj base label arg fn = do
        dbgBLabel subj base label $ fn arg
        return fn

      dbgResultFn2 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
      dbgResultFn2 subj base label a1 a2 fn = do
        dbgBLabel subj base label $ fn a1 a2
        return fn

      dbgResultFn3 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) ->
            m (a -> b -> c -> r)
      dbgResultFn3 subj base label a1 a2 a3 fn = do
        dbgBLabel subj base label $ fn a1 a2 a3
        return fn

      dbgResultM ::
        ($md m n, MonadIO m, Blockable a) =>
          Subject -> Int -> String -> m a -> m a
      {-# INLINE dbgResultM #-}
      dbgResultM subj base msg resM = do
        res <- resM
        dbgBLabel subj base (msg ++ " ") res
        return res

      dbgResultFn1M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> m (a -> r) -> m (a -> r)
      dbgResultFn1M subj base label arg fnM = do
        fn <- fnM
        dbgBLabel subj base label $ fn arg
        return fn

      dbgResultFn2M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> m (a -> b -> r) ->
            m (a -> b -> r)
      dbgResultFn2M subj base label a1 a2 fnM = do
        fn <- fnM
        dbgBLabel subj base label $ fn a1 a2
        return fn

      dbgResultFn3M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> m (a -> b -> c -> r) ->
            m (a -> b -> c -> r)
      dbgResultFn3M subj base label a1 a2 a3 fnM = do
        fn <- fnM
        dbgBLabel subj base label $ fn a1 a2 a3
        return fn
        |]

  else
    [d|
      -- |Output the given line in the current level of indentation.
      dbgBlock :: forall m n . ($md m n, MonadIO m) =>
                    Subject -> Int -> Block -> m ()
      dbgBlock _ _ _ = return ()

      -- | Format and output the given value at the current level of
      -- indentation, with the given leading label.
      dbgBLabel :: forall m n c . ($md m n, MonadIO m, Blockable c) =>
                     Subject -> Int -> String -> c -> m ()
      dbgBLabel _ _ _ _ = return ()

      -- | Format and output the given value as a bullet point at the
      -- current level of indentation, with the given leading label.
      dbgBLabelPt :: ($md m n, MonadIO m, Blockable c) =>
                       Subject -> Int -> String -> c -> m ()
      dbgBLabelPt _ _ _ _ = return ()

      -- | Given a result to be returned from a computation, emit debugging
      -- information about it if debugging mode is on.
      dbgResult :: ($md m n, MonadIO m, Blockable a) =>
                     Subject -> Int -> String -> a -> m a
      {-# INLINE dbgResult #-}
      dbgResult _ _ _ res = return res

      -- |Given a function of one argument, emit debugging information about
      -- it if debugging mode is on.
      dbgBLabelFn1 :: ($md m n, MonadIO m, Blockable r) =>
                        Subject -> Int -> String -> a -> (a -> r) -> m ()
      dbgBLabelFn1 _ _ _ _ _ = return ()

      -- |Given a function of two arguments, emit debugging information
      -- about it if debugging mode is on.
      dbgBLabelFn2 :: ($md m n, MonadIO m, Blockable r) =>
                        Subject -> Int -> String -> a -> b -> (a -> b -> r) ->
                          m ()
      dbgBLabelFn2 _ _ _ _ _ _ = return ()

      -- |Given a function of three arguments, emit debugging information
      -- about it if debugging mode is on.
      dbgBLabelFn3 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) -> m ()
      dbgBLabelFn3 _ _ _ _ _ _ _ = return ()

      -- |Given a computation returning a function of one argument, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn1 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> (a -> r) -> m (a -> r)
      dbgResultFn1 _ _ _ _ fn = return fn

      -- |Given a computation returning a function of two arguments, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn2 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
      dbgResultFn2 _ _ _ _ _ fn = return fn

      -- |Given a computation returning a function of two arguments, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn3 ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> (a -> b -> c -> r) ->
            m (a -> b -> c -> r)
      dbgResultFn3 _ _ _ _ _ _ fn = return fn

      -- |Given a monadic computation whose result will be taken as the
      -- overall result, emit debugging information about that result if
      -- debugging mode is on.
      dbgResultM :: ($md m n, MonadIO m, Blockable a) =>
                      Subject -> Int -> String -> m a -> m a
      {-# INLINE dbgResultM #-}
      dbgResultM _ _ _ resM = resM

      -- |Given a computation returning a function of one argument, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn1M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> m (a -> r) -> m (a -> r)
      dbgResultFn1M _ _ _ _ fnM = fnM

      -- |Given a computation returning a function of two arguments, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn2M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> m (a -> b -> r) ->
            m (a -> b -> r)
      dbgResultFn2M _ _ _ _ _ fnM = fnM

      -- |Given a computation returning a function of two arguments, emit
      -- debugging information about it if debugging mode is on.
      dbgResultFn3M ::
        ($md m n, MonadIO m, Blockable r) =>
          Subject -> Int -> String -> a -> b -> c -> m (a -> b -> c -> r) ->
            m (a -> b -> c -> r)
      dbgResultFn3M _ _ _ _ _ _ fnM = fnM
        |]
  where declDoc :: String -> String -> Q ()
        declDoc name doc = addModFinalizer $ putDoc (DeclDoc $ mkName name) doc

-- | Introduce both the core @Debugln@ definitions, and the additional
-- functions for `Blockable` values, into a module.  The @switch@
-- argument determines whether any output-generating code should
-- actually be produced.
makeDebuglnAndBPPDefs :: Bool -> Q [Dec]
makeDebuglnAndBPPDefs sw = do
  base <- makeDebuglnDefs sw
  bpps <- makeDebuglnBPPDefs sw
  return $ base ++ bpps
