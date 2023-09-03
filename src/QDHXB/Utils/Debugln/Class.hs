{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances#-}
{-# LANGUAGE KindSignatures, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
--
-- See also the `QDHXB.Utils.DebuglnBlock` module for functions
-- generating debug messages for `QDHXB.Utils.BPP.Blockable` values.
module QDHXB.Utils.Debugln.Class (
  Debugln(..), DebuglnState(..), MonadDebugln, liftDebugln, runDebugln,
  getIndentation
  )
where

import Data.Kind (Type)
import Data.Symbol
import Control.Monad.IO.Class
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
runDebugln (Debugln m) _switch volumes ind = do
  (result, _) <- SL.runStateT m $ DebuglnState volumes "" ind
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

-- | Internal: returns the current `String` indentation.
getIndentation :: MonadDebugln m n => m String
getIndentation = do
  st <- debuggingState
  return $ indentation st
