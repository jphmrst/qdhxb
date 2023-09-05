{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll, KindSignatures #-}


-- | Internal monad for the XSD-to-Haskell rewriting.
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.
module QDHXB.Utils.Debugln.New (
  module QDHXB.Utils.Debugln.New, Type
  )
where

import Data.Symbol
import Data.Kind
import Language.Haskell.TH hiding (Type)
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

makeDebuglnDefs :: Bool -> Q [Dec]
makeDebuglnDefs switch = do
  if switch
  then do
    [d|
      -- | Internal state of the `Debugln` monad.
      data DebuglnState = DebuglnState {
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
      runDebugln :: Monad m =>
        Debugln m a -> Bool -> [(Subject,Int)] -> String -> m a
      runDebugln (Debugln m) _switch volumes ind = do
        (result, _) <- SL.runStateT m $
          DebuglnState (map (\(s,v) -> (subjectSymbol s,v)) volumes) "" ind
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
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WL.WriterT w m) n where
        liftDebugln = lift . liftDebugln
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WS.WriterT w m) n where
        liftDebugln = lift . liftDebugln

      -- | Internal: returns the current `String` indentation.
      getIndentation :: forall m n . MonadDebugln m n => m String
      getIndentation = do
        st <- debuggingState
        return $ indentation st

      -- | Internal: return the debugging state.
      debuggingState :: forall m n . MonadDebugln m n => m DebuglnState
      debuggingState = liftDebugln $ Debugln SL.get

      -- | Internal: returns `True` as the `Bool` master switch setting.
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

      -- | Check whether some debugging subject has been activated.
      getDebuggingAny :: forall m n . MonadDebugln m n => m Bool
      getDebuggingAny = do
        state <- debuggingState
        return $ not $ null $ subjects state

      -- | Run a subordinated block only if some subject has been activated.
      whenAnyDebugging :: forall m n . MonadDebugln m n => m () -> m ()
      whenAnyDebugging = whenM getDebuggingAny

      -- | Run a subordinated block only if debugging a particular subject
      -- at the given level of detail.
      whenDebugging :: forall m n . MonadDebugln m n => Subject -> Int -> m () -> m ()
      whenDebugging subj base m = do
        whenAnyDebugging $ do
          vol <- getVolume subj
          case vol of
            Just v | v <= base -> m
            _ -> return ()

      -- | Pick from subordinated blocks based on whether the debugging
      -- master switch is on.
      ifAnyDebugging :: forall m n a . MonadDebugln m n => m a -> m a -> m a
      ifAnyDebugging = ifM getDebuggingAny

      -- | Pick from subordinated blocks based on whether we are debugging a
      -- particular subject.
      ifDebugging :: forall m n a . MonadDebugln m n => Subject -> Int -> m a -> m a -> m a
      ifDebugging subj base th el = do
        vol <- getVolume subj
        case vol of
          Just v | v <= base -> th
          _ -> el

      -- | Add a level of indentation to debugging output.
      indenting :: forall m n a . MonadDebugln m n => m a -> m a
      indenting m = do
        state <- debuggingState
        liftDebugln $ Debugln $ SL.put $
          state { indentation = indentationBase state ++ indentation state }
        result <- m
        liftDebugln $ Debugln $ SL.put state
        return result

      putStrIndenting :: forall m n . (MonadDebugln m n, MonadIO m) => String -> m ()
      putStrIndenting str = do
        state <- debuggingState
        liftIO $ putStrLn $ indentation state ++ str

      -- | Output the given line in the current level of indentation.
      dbgLn :: forall m n . (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgLn subj base str =
        whenDebugging subj base $ putStrIndenting str

      -- | Output the given line as a bulleted item in the current level of
      -- indentation.
      dbgPt :: forall m n . (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgPt subj base str = dbgLn subj base $ "- " ++ str

      -- | Wrap the output of the given code in a (three-sided) box.
      boxed :: forall m n a . (MonadDebugln m n, MonadIO m) => m a -> m a
      boxed m = do
        putStrIndenting "+----------"
        state <- debuggingState
        liftDebugln $ Debugln $ SL.put $
          state { indentation = indentation state ++ "| " }
        res <- m
        liftDebugln $ Debugln $ SL.put state
        putStrIndenting "+----------"
        return res

      |]
  else do
    [d|
      -- | Internal state of the `Debugln` monad.
      data DebuglnState = DebuglnState

      -- | Main type of the `Debugln` monad transformer.
      newtype Debugln (m :: Type -> Type) a = Debugln (m a)
        deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

      -- | Given initial debugging choices, run a `Debugln` monad.
      runDebugln :: Monad m =>
        Debugln m a -> Bool -> [(Subject,Int)] -> String -> m a
      runDebugln (Debugln m) _switch _volumes _ind = m

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
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WL.WriterT w m) n where
        liftDebugln = lift . liftDebugln
      instance (MonadDebugln m n, Monoid w) =>
          MonadDebugln (WS.WriterT w m) n where
        liftDebugln = lift . liftDebugln

      -- | Internal: returns the current `String` indentation.
      getIndentation :: MonadDebugln m n => m String
      getIndentation = return ""

      -- | Internal: return the debugging state.
      debuggingState :: MonadDebugln m n => m DebuglnState
      debuggingState = return DebuglnState

      -- | Internal: returns `True` as the `Bool` master switch setting.
      getDebugging :: MonadDebugln m n => m Bool
      getDebugging = return False

      getVolume :: MonadDebugln m n => Subject -> m (Maybe Int)
      getVolume _ = return Nothing

      -- | Check whether some debugging subject has been activated.
      getDebuggingAny :: MonadDebugln m n => m Bool
      getDebuggingAny = return False

      -- | Run a subordinated block only if some subject has been activated.
      whenAnyDebugging :: MonadDebugln m n => m () -> m ()
      whenAnyDebugging _ = return ()

      -- | Run a subordinated block only if debugging a particular subject
      -- at the given level of detail.
      whenDebugging :: MonadDebugln m n => Subject -> Int -> m () -> m ()
      whenDebugging _ _ _ = return ()

      -- | Pick from subordinated blocks based on whether the debugging
      -- master switch is on.
      ifAnyDebugging :: MonadDebugln m n => m a -> m a -> m a
      ifAnyDebugging _ _ el = el

      -- | Pick from subordinated blocks based on whether we are debugging a
      -- particular subject.
      ifDebugging :: MonadDebugln m n => Subject -> Int -> m a -> m a -> m a
      ifDebugging _ _ el = el

      -- | Add a level of indentation to debugging output.
      indenting :: MonadDebugln m n => m a -> m a
      indenting m = m

      putStrIndenting :: (MonadDebugln m n, MonadIO m) => String -> m ()
      putStrIndenting _ = return ()

      -- | Output the given line in the current level of indentation.
      dbgLn :: (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgLn _ _ _ = return ()

      -- | Output the given line as a bulleted item in the current level of
      -- indentation.
      dbgPt :: (MonadDebugln m n, MonadIO m) => Subject -> Int -> String -> m ()
      dbgPt _ _ _ = return ()

      -- | Wrap the output of the given code in a (three-sided) box.
      boxed :: (MonadDebugln m n, MonadIO m) => m a -> m a
      boxed m = m

      |]
