{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

-- | Supporting debugging via @PutStrLn@-style statements, with the
-- option to turn off tracing code at compile-time, replacing print
-- statements with no-ops which can (GHC willing!) be optimized out of
-- compiled code.
--
-- A module which @import@s `QDHXB.Utils.Debugln` and calls
-- `makeDebuglnDefs` must include a number of language extensions.  A
-- standard usage of @Debugln@ would be:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > {-# LANGUAGE FunctionalDependencies #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE UndecidableInstances #-}
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE TypeApplications #-}
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- >
-- > module My.Project.Debugln (module My.Project.Debugln) where
-- >
-- > import QDHXB.Utils.Debugln
-- > makeDebuglnDefs True -- Or `False` to turn off all debugging output.
--
-- A @My.Project.Debugln@ module is also a handy place to make
-- canonical instances of the "subjects" about which debugging
-- messages can be made and filtered:
--
-- > setup, gui, output, messaging :: Subject
-- > setup = subject "setup"
-- > gui = subject "gui"
-- > output = subject "output"
-- > messaging = subject "messaging"
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.

module QDHXB.Utils.Debugln (module QDHXB.Utils.Debugln, Type)
where

import Data.Symbol
import Data.Kind
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax (addModFinalizer)
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

-- | Type of subjects which can be traced by `QDHXB.Utils.Debugln`.
newtype Subject = Subject { subjectSymbol :: Symbol }
  deriving Show

-- | Register a `String` name as a `Subject`
subject :: String -> Subject
subject = Subject . intern

-- | Introduce @Debugln@ tracing functions and other definitions into
-- a module.  The @switch@ argument determines whether any
-- output-generating code should actually be produced.
makeDebuglnDefs :: Bool -> Q [Dec]
makeDebuglnDefs switch = do
  declDoc "DebuglnState" "Internal state of the `Debugln` monad."
  declDoc "Debugln" "Main type of the `Debugln` monad transformer."
  declDoc "MonadDebugln" "Class of monads which support debugging operations."
  declDoc "liftDebugln" "Lift a `Debugln` computation to the given `MonadDebugln` monad @m@."
  declDoc "subjects" "Topics (by symbol) of current interest for debugging."
  declDoc "indentation" "Current number of indentation stops."
  declDoc "indentationBase" "`String` printed per indentation stop."
  declDoc "runDebugln" "Given initial debugging choices, run a `Debugln` monad."
  declDoc "getIndentation" "Internal: returns the current `String` indentation."
  declDoc "debuggingState" "Internal: return the debugging state."
  declDoc "getDebugging" "Internal: the `Bool` master switch setting for this `Debugln` instantiation."
  declDoc "getVolume" "Return the level of detail in a `Debugln` monad for a particular `Subject`."
  declDoc "getDebuggingAny" "Check whether both the debugging master switch is on, and at least one debugging subject has been activated."
  declDoc "whenDebugging" "Run a subordinated block only if debugging a particular subject at the given level of detail."
  declDoc "whenAnyDebugging" "Run a subordinated block only if some subject has been activated."
  declDoc "getDebuggingAny" "Check whether some debugging subject has been activated."
  declDoc "ifAnyDebugging" "Pick from subordinated blocks based on whether the debugging master switch is on."
  declDoc "ifDebugging" "Pick from subordinated blocks based on whether we are debugging a particular subject."
  declDoc "indenting" "Add a level of indentation to debugging output."
  declDoc "dbgLn" "Output the given line in the current level of indentation."
  declDoc "dbgPt" "Output the given line as a bulleted item in the current level of indentation."
  declDoc "boxed" "Wrap the output of the given code in a (three-sided) box."
  declDoc "_putStrIndenting" "Internal printer helper."
  if switch
  then do
    [d|
      data DebuglnState = DebuglnState {
        subjects :: [(Symbol,Int)],
        indentation :: String,
        indentationBase :: String
        }

      newtype Debugln (m :: Type -> Type) a =
        Debugln { openDebugln :: SL.StateT DebuglnState m a }
        deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

      runDebugln :: Monad m =>
        Debugln m a -> Bool -> [(Subject,Int)] -> String -> m a
      runDebugln (Debugln m) _switch volumes ind = do
        (result, _) <- SL.runStateT m $
          DebuglnState (map (\(s,v) -> (subjectSymbol s,v)) volumes) "" ind
        return result

      class (Monad m, Monad n) => MonadDebugln m n | m -> n where
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
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WL.WriterT w m) n where
        liftDebugln = lift . liftDebugln
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WS.WriterT w m) n where
        liftDebugln = lift . liftDebugln

      getIndentation :: forall m n . MonadDebugln m n => m String
      getIndentation = do
        st <- debuggingState
        return $ indentation st

      debuggingState :: forall m n . MonadDebugln m n => m DebuglnState
      debuggingState = liftDebugln $ Debugln SL.get

      getDebugging :: forall m n . MonadDebugln m n => m Bool
      getDebugging = return True

      getVolume :: forall m n . MonadDebugln m n => Subject -> m (Maybe Int)
      getVolume (Subject subj) = do
        state <- debuggingState
        let vols = subjects state
        return $ get' vols
        where get' [] = Nothing
              get' ((s,v):_) | s == subj = Just v
              get' (_:ss) = get' ss

      getDebuggingAny :: forall m n . MonadDebugln m n => m Bool
      getDebuggingAny = do
        state <- debuggingState
        return $ not $ null $ subjects state

      whenAnyDebugging :: forall m n . MonadDebugln m n => m () -> m ()
      whenAnyDebugging = whenM getDebuggingAny

      whenDebugging ::
        forall m n . MonadDebugln m n => Subject -> Int -> m () -> m ()
      whenDebugging subj base m = do
        whenAnyDebugging $ do
          vol <- getVolume subj
          case vol of
            Just v | v <= base -> m
            _ -> return ()

      ifAnyDebugging :: forall m n a . MonadDebugln m n => m a -> m a -> m a
      ifAnyDebugging = ifM getDebuggingAny

      ifDebugging ::
        forall m n a . MonadDebugln m n => Subject -> Int -> m a -> m a -> m a
      ifDebugging subj base th el = do
        vol <- getVolume subj
        case vol of
          Just v | v <= base -> th
          _ -> el

      indenting :: forall m n a . MonadDebugln m n => m a -> m a
      indenting m = do
        state <- debuggingState
        liftDebugln $ Debugln $ SL.put $
          state { indentation = indentationBase state ++ indentation state }
        result <- m
        liftDebugln $ Debugln $ SL.put state
        return result

      _putStrIndenting ::
        forall m n . (MonadDebugln m n, MonadIO m) => String -> m ()
      _putStrIndenting str = do
        state <- debuggingState
        liftIO $ putStrLn $ indentation state ++ str

      dbgLn ::
        forall m n . (MonadDebugln m n, MonadIO m) =>
          Subject -> Int -> String -> m ()
      dbgLn subj base str =
        whenDebugging subj base $ _putStrIndenting str

      dbgPt ::
        forall m n . (MonadDebugln m n, MonadIO m) =>
          Subject -> Int -> String -> m ()
      dbgPt subj base str = dbgLn subj base $ "- " ++ str

      boxed :: forall m n a . (MonadDebugln m n, MonadIO m) => m a -> m a
      boxed m = do
        _putStrIndenting "+----------"
        state <- debuggingState
        liftDebugln $ Debugln $ SL.put $
          state { indentation = indentation state ++ "| " }
        res <- m
        liftDebugln $ Debugln $ SL.put state
        _putStrIndenting "+----------"
        return res

      |]
  else do
    [d|
      data DebuglnState = DebuglnState

      newtype Debugln (m :: Type -> Type) a = Debugln (m a)
        deriving (Functor, Applicative, Monad, MonadIO)
      instance MonadTrans Debugln where
        -- lift :: Monad m => m a -> Debugln (m a)
        lift = Debugln

      runDebugln :: Monad m =>
        Debugln m a -> Bool -> [(Subject,Int)] -> String -> m a
      runDebugln (Debugln m) _switch _volumes _ind = m

      class (Monad m, Monad n) => MonadDebugln m n | m -> n where
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
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WL.WriterT w m) n where
        liftDebugln = lift . liftDebugln
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WS.WriterT w m) n where
        liftDebugln = lift . liftDebugln

      getIndentation :: MonadDebugln m n => m String
      getIndentation = return ""

      debuggingState :: MonadDebugln m n => m DebuglnState
      debuggingState = return DebuglnState

      getDebugging :: MonadDebugln m n => m Bool
      getDebugging = return False

      getVolume :: MonadDebugln m n => Subject -> m (Maybe Int)
      getVolume _ = return Nothing

      getDebuggingAny :: MonadDebugln m n => m Bool
      getDebuggingAny = return False

      whenAnyDebugging :: MonadDebugln m n => m () -> m ()
      whenAnyDebugging _ = return ()

      whenDebugging :: MonadDebugln m n => Subject -> Int -> m () -> m ()
      whenDebugging _ _ _ = return ()

      ifAnyDebugging :: MonadDebugln m n => m a -> m a -> m a
      ifAnyDebugging _ el = el

      ifDebugging :: MonadDebugln m n => Subject -> Int -> m a -> m a -> m a
      ifDebugging _ _ _ el = el

      indenting :: MonadDebugln m n => m a -> m a
      indenting m = m

      _putStrIndenting :: (MonadDebugln m n, MonadIO m) => String -> m ()
      _putStrIndenting _ = return ()

      dbgLn :: (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgLn _ _ _ = return ()

      dbgPt :: (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgPt _ _ _ = return ()

      boxed :: (MonadDebugln m n, MonadIO m) => m a -> m a
      boxed m = m

      |]
  where declDoc :: String -> String -> Q ()
        declDoc name doc = addModFinalizer $ putDoc (DeclDoc $ mkName name) doc
