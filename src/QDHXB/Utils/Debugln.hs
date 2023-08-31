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
  Debugln(..), MonadDebugln, liftDebugln, runDebugln,
  -- * Debugging output
  dbgLn, dbgPt, indenting, boxed,
  -- * Conditional execution for debugging
  getDebugging, whenDebugging, ifDebugging, getIndentation,
  -- * File-local definitions
  fileLocalDebuglnSubject, fileLocalDebuglnCall
  )
where

import Language.Haskell.TH (
  Q, Dec, Exp(AppE, VarE, LitE), Lit(StringL, IntegerL),
  mkName, DocLoc(DeclDoc), putDoc)
import Language.Haskell.TH.Syntax (addModFinalizer)
import Data.Kind (Type)
import Data.Symbol
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.ST.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS

-- | Internal state of the `Debugln` monad.
data DebuglnState = DebuglnState {
  debuggingOn :: Bool, -- ^ Master switch for any debugging.
  subjects :: [(Symbol,Int)], -- ^ Topics (by symbol) of current
                              -- interest for debugging.
  indentation :: String, -- ^ Current number of indentation stops.
  indentationBase :: String -- ^ `String` printed per indentation
                            -- stop.
  }

-- | Main type of the `Debugln` monad transformer.
newtype Debugln (m :: Type -> Type) a =
  Debugln { openDebugln :: SL.StateT DebuglnState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Given initial debugging choices, run a `Debugln` monad.
runDebugln :: Monad m => Debugln m a -> Bool -> [(Symbol,Int)] -> String -> m a
runDebugln (Debugln m) switch volumes ind = do
  (result, _) <- SL.runStateT m $ DebuglnState switch volumes "" ind
  return result

-- | Class of monads which support debugging operations.
class (Monad m, Monad n) => MonadDebugln m n | m -> n where
  -- | Lift a `Debugln` computation to the given `MonadDebugln` monad
  -- @m@.
  liftDebugln :: Debugln n a -> m a

instance Monad m => MonadDebugln (Debugln m) m where
  liftDebugln = id

instance MonadDebugln m n => MonadDebugln (MaybeT m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (IdentityT m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (ExceptT e m) n where
  liftDebugln = lift . liftDebugln

instance (MonadDebugln m n, Functor f) => MonadDebugln (FreeT f m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (ReaderT r m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (ResourceT m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (SS.StateT s m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (SL.StateT s m) n where
  liftDebugln = lift . liftDebugln

instance MonadDebugln m n => MonadDebugln (STT s m) n where
  liftDebugln = lift . liftDebugln

instance (MonadDebugln m n, Monoid w) => MonadDebugln (WL.WriterT w m) n where
  liftDebugln = lift . liftDebugln

instance (MonadDebugln m n, Monoid w) => MonadDebugln (WS.WriterT w m) n where
  liftDebugln = lift . liftDebugln

-- | Internal: return the debugging state.
debuggingState :: MonadDebugln m n => m DebuglnState
debuggingState = liftDebugln $ Debugln SL.get

-- | Internal: returns the `Bool` master switch setting.
getDebugging :: MonadDebugln m n => m Bool
getDebugging = do
  st <- debuggingState
  return $ debuggingOn st

-- | Internal: returns the current `String` indentation.
getIndentation :: MonadDebugln m n => m String
getIndentation = do
  st <- debuggingState
  return $ indentation st

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
whenAnyDebugging :: MonadDebugln m n => m () -> m ()
whenAnyDebugging m = do
  b <- getDebugging
  when b m

-- | Run a subordinated block only if debugging a particular subject.
whenDebugging :: MonadDebugln m n => Symbol -> Int -> m () -> m ()
whenDebugging subj base m = do
  whenAnyDebugging $ do
    vol <- getVolume subj
    case vol of
      Just v | v <= base -> m
      _ -> return ()

-- | Pick from subordinated blocks based on whether the debugging
-- master switch is on.
ifAnyDebugging :: Monad m => Debugln m a -> Debugln m a -> Debugln m a
ifAnyDebugging = ifM getDebugging

-- | Pick from subordinated blocks based on whether we are debugging a
-- particular subject.
ifDebugging :: Monad m =>
  Symbol -> Int -> Debugln m a -> Debugln m a -> Debugln m a
ifDebugging subj base th el =
  ifAnyDebugging
    (do vol <- getVolume subj
        case vol of
          Just v | v <= base -> th
          _ -> el)
    el

-- | Add a level of indentation to debugging output.
indenting :: MonadDebugln m n => m a -> m a
indenting m = do
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
dbgLn :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> String -> m ()
dbgLn subj base str = whenDebugging subj base $ putStrIndenting str

-- | Output the given line as a bulleted item in the current level of
-- indentation.
dbgPt :: (MonadDebugln m n, MonadIO m) => Symbol -> Int -> String -> m ()
dbgPt subj base str = dbgLn subj base $ "- " ++ str

-- | Wrap the output of the given code in a (three-sided) box.
boxed :: (MonadDebugln m n, MonadIO m) => m a -> m a
boxed m = do
  putStrIndenting "+----------"
  state <- debuggingState
  liftDebugln $ Debugln $ SL.put $
    state { indentation = indentation state ++ "| " }
  res <- m
  liftDebugln $ Debugln $ SL.put state
  putStrIndenting "+----------"
  return res

-- | Create bindings of `QDHXB.Utils.Debugln` functions fixed to a
-- particular subject.  The `String` argument should be the underlying
-- name of the subject symbol.  The valid entries of the second
-- argument list are: @"dbgLn"@, and @"dbgPt"@.
fileLocalDebuglnSubject :: String -> [String] -> Q [Dec]
fileLocalDebuglnSubject subj = fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgLn" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgLn")
            "Output the given line in the current level of indentation."
          [d| dbgLn :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                        Control.Monad.IO.Class.MonadIO m) => Int -> String -> m ()
              dbgLn = QDHXB.Utils.Debugln.dbgLn $(return subjExp)
            |]
        f' "dbgPt" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgPt")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgPt :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                        Control.Monad.IO.Class.MonadIO m) => Int -> String -> m ()
              dbgPt = QDHXB.Utils.Debugln.dbgPt $(return subjExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)

-- | Create bindings of `QDHXB.Utils.Debugln` functions fixed to a
-- particular subject and base detail volume.  The `String` argument
-- should be the underlying name of the subject symbol.  The valid
-- entries of the second argument list are: @"dbgLn"@ and @"dbgPt"@.
fileLocalDebuglnCall :: String -> Integer -> [String] -> Q [Dec]
fileLocalDebuglnCall subj base =  fmap concat . mapM f'
  where f' :: String -> Q [Dec]
        f' "dbgLn" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgLn")
            "Output the given line in the current level of indentation."
          [d| dbgLn :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                        Control.Monad.IO.Class.MonadIO m) => String -> m ()
              dbgLn = QDHXB.Utils.Debugln.dbgLn
                        $(return subjExp) $(return baseExp)
            |]
        f' "dbgPt" = do
          addModFinalizer $ putDoc (DeclDoc $ mkName "dbgPt")
            "Output the given line as a bulleted item in the current level of indentation."
          [d| dbgPt :: (QDHXB.Utils.Debugln.MonadDebugln m n,
                        Control.Monad.IO.Class.MonadIO m) => String -> m ()
              dbgPt = QDHXB.Utils.Debugln.dbgPt
                        $(return subjExp) $(return baseExp)
            |]
        f' str = error $
          "Name " ++ str ++ " not known to module QDHXB.Utils.Debugln"

        subjExp = AppE (VarE 'Data.Symbol.intern) (LitE $ StringL subj)
        baseExp = LitE $ IntegerL base

        -- subj = intern subjName
