{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Template Haskell definitions
module QDHXB.Internal.XSDQ (
  -- * XSD loading monad
  QdxhbState, initialQdxhbState,
  XSDQ, runXSDQ, liftIOtoXSDQ, liftQtoXSDQ, liftStatetoXSDQ,

  -- * Miscellaneous
  NameStore, containForBounds)
where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import QDHXB.Internal.Types

type NameStore a = [(String, a)]

-- | The type of the internal state of an `XSDQ` computation, tracking
-- the names of XSD entities and their definitions.
type QdxhbState = (NameStore ItemDefn, NameStore ItemDefn)

-- | The initial value of `XSDQ` states.
initialQdxhbState :: QdxhbState
initialQdxhbState = ([], [])

-- | Monadic type for loading and interpreting XSD files, making
-- definitions available after they are loaded.
newtype XSDQ a = XSDQ (StateT QdxhbState Q a)
  deriving (Functor, Applicative, Monad, MonadIO)

liftIOtoXSDQ :: IO a -> XSDQ a
liftIOtoXSDQ = XSDQ . lift . liftIO

liftQtoXSDQ :: Q a -> XSDQ a
liftQtoXSDQ = XSDQ . lift

liftStatetoXSDQ :: StateT QdxhbState Q a -> XSDQ a
liftStatetoXSDQ = XSDQ

instance Quote XSDQ where
  newName = liftQtoXSDQ . newName

-- | Run an `XSDQ` monad, exposing the underlying `Q` computation.
runXSDQ :: XSDQ a -> Q a
runXSDQ (XSDQ m) = evalStateT m initialQdxhbState

-- | Transform a Haskell type representation based on the (possibly
-- absent) lower and upper bounds of an XSD type constraint.
containForBounds :: Maybe Int -> Maybe Int -> XSDQ Type -> XSDQ Type
containForBounds (Just 0) (Just 0) _ = [t|()|]
containForBounds (Just 0) (Just 1) t = [t|Maybe $t|]
containForBounds (Just 1) (Just 1) t = t
containForBounds _ _ t = [t|[$t]|]
