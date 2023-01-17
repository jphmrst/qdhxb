{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Template Haskell definitions
module QDHXB.Internal.XSDQ (
  -- * XSD loading monad
  QdxhbState, initialQdxhbState,
  XSDQ, runXSDQ, liftIOtoXSDQ, liftQtoXSDQ, liftStatetoXSDQ,
  fileNewItemDefn,
  addElementDefn, getElementDefn, addAttrDefn, getAttrDefn,

  -- * Miscellaneous
  NameStore, containForBounds)
where

import Language.Haskell.TH
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

fileNewItemDefn :: ItemDefn -> XSDQ ()
fileNewItemDefn defn@(SimpleRep n _) = addElementDefn n defn
fileNewItemDefn defn@(AttributeRep n _ _) = addAttrDefn n defn
fileNewItemDefn defn@(SequenceRep n _) = addElementDefn n defn

addElementDefn :: String -> ItemDefn -> XSDQ ()
addElementDefn name defn = liftStatetoXSDQ $ do
  (elems, attrs) <- get
  put ((name, defn) : elems, attrs)

addAttrDefn :: String -> ItemDefn -> XSDQ ()
addAttrDefn name defn = liftStatetoXSDQ $ do
  (elems, attrs) <- get
  put (elems, (name, defn) : attrs)

getElementDefn :: String -> XSDQ (Maybe ItemDefn)
getElementDefn name = liftStatetoXSDQ $ do
  (elems, _) <- get
  return $ lookupFirst elems name

getAttrDefn :: String -> XSDQ (Maybe ItemDefn)
getAttrDefn name = liftStatetoXSDQ $ do
  (_, attrs) <- get
  return $ lookupFirst attrs name

lookupFirst :: [(String, a)] -> String -> Maybe a
lookupFirst [] _ = Nothing
lookupFirst ((fnd, x):_) targ | fnd == targ = Just x
lookupFirst (_:xs) targ = lookupFirst xs targ

