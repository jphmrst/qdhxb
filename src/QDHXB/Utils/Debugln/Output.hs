{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances#-}
{-# LANGUAGE KindSignatures, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.
module QDHXB.Utils.Debugln.Output (
  -- * Debugging output
  dbgLn_impl, dbgPt_impl, indenting_impl, boxed_impl,
  -- * Conditional execution for debugging
  getDebugging_impl,
  whenDebugging_impl, whenAnyDebugging_impl, ifAnyDebugging_impl,
  ifDebugging_impl
  )
where

import Data.Symbol
import Control.Monad.IO.Class
import Control.Monad.Extra
import QDHXB.Utils.Debugln.Class
import qualified Control.Monad.Trans.State.Lazy as SL

-- | Internal: return the debugging state.
debuggingState :: MonadDebugln m n => m DebuglnState
debuggingState = liftDebugln $ Debugln SL.get

-- | Internal: returns the `Bool` master switch setting.
getDebugging_impl :: MonadDebugln m n => m Bool
getDebugging_impl = do
  st <- debuggingState
  return $ debuggingOn st

getVolume :: MonadDebugln m n => Symbol -> m (Maybe Int)
getVolume subj = do
  state <- debuggingState
  let vols = subjects state
  return $ get' vols
  where get' [] = Nothing
        get' ((s,v):_) | s == subj = Just v
        get' (_:ss) = get' ss

-- | Run a subordinated block only if the debugging master switch is
-- on.
whenAnyDebugging_impl :: MonadDebugln m n => m () -> m ()
whenAnyDebugging_impl m = do
  b <- getDebugging_impl
  when b m

-- | Run a subordinated block only if debugging a particular subject.
whenDebugging_impl :: MonadDebugln m n => Symbol -> Int -> m () -> m ()
whenDebugging_impl subj base m = do
  whenAnyDebugging_impl $ do
    vol <- getVolume subj
    case vol of
      Just v | v <= base -> m
      _ -> return ()

-- | Pick from subordinated blocks based on whether the debugging
-- master switch is on.
ifAnyDebugging_impl :: MonadDebugln m n => m a -> m a -> m a
ifAnyDebugging_impl = ifM getDebugging_impl

-- | Pick from subordinated blocks based on whether we are debugging a
-- particular subject.
ifDebugging_impl :: MonadDebugln m n => Symbol -> Int -> m a -> m a -> m a
ifDebugging_impl subj base th el =
  ifAnyDebugging_impl
    (do vol <- getVolume subj
        case vol of
          Just v | v <= base -> th
          _ -> el)
    el

-- | Add a level of indentation to debugging output.
indenting_impl :: MonadDebugln m n => m a -> m a
indenting_impl m = do
  state <- debuggingState
  liftDebugln $ Debugln $ SL.put $
    state { indentation = indentationBase state ++ indentation state }
  result <- m
  liftDebugln $ Debugln $ SL.put state
  return result

putStrIndenting :: (MonadDebugln m n, MonadIO m) => String -> m ()
putStrIndenting str = do
  state <- debuggingState
  liftIO $ putStrLn $ indentation state ++ str

-- | Output the given line in the current level of indentation.
dbgLn_impl :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> String -> m ()
dbgLn_impl subj base str = whenDebugging_impl subj base $ putStrIndenting str

-- | Output the given line as a bulleted item in the current level of
-- indentation.
dbgPt_impl :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> String -> m ()
dbgPt_impl subj base str = dbgLn_impl subj base $ "- " ++ str

-- | Wrap the output of the given code in a (three-sided) box.
boxed_impl :: (MonadDebugln m n, MonadIO m) => m a -> m a
boxed_impl m = do
  putStrIndenting "+----------"
  state <- debuggingState
  liftDebugln $ Debugln $ SL.put $
    state { indentation = indentation state ++ "| " }
  res <- m
  liftDebugln $ Debugln $ SL.put state
  putStrIndenting "+----------"
  return res
