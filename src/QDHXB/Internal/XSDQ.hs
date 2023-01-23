{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Defines the internal monad for the XSD-to-Haskell rewriting.
module QDHXB.Internal.XSDQ (
  -- * XSD loading monad
  QdxhbState, initialQdxhbState,
  XSDQ, runXSDQ, liftIOtoXSDQ, liftQtoXSDQ, liftStatetoXSDQ,
  fileNewDefinition,
  addElementType, getElementType, getElementTypeOrFail,
  getOptions, getUseNewtype, getDebugging, whenDebugging,

  -- * Miscellaneous
  NameStore, containForBounds)
where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import QDHXB.Internal.Types
import QDHXB.Options

type NameStore a = [(String, a)]

-- | The type of the internal state of an `XSDQ` computation, tracking
-- the names of XSD entities and their definitions.
data QdxhbState = QdxhbState QDHXBOptionSet (NameStore String)

-- | The initial value of `XSDQ` states.
initialQdxhbState :: QDHXBOption -> QdxhbState
initialQdxhbState optsF = QdxhbState (optsF defaultOptionSet) []

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
runXSDQ :: QDHXBOption -> XSDQ a -> Q a
runXSDQ optsF (XSDQ m) = evalStateT m $ initialQdxhbState optsF

-- | Transform a Haskell type representation based on the (possibly
-- absent) lower and upper bounds of an XSD type constraint.
containForBounds :: Maybe Int -> Maybe Int -> XSDQ Type -> XSDQ Type
containForBounds (Just 0) (Just 0) _ = [t|()|]
containForBounds (Just 0) (Just 1) t = [t|Maybe $t|]
containForBounds (Just 1) (Just 1) t = t
containForBounds _ _ t = [t|[$t]|]

fileNewDefinition :: Definition -> XSDQ ()
fileNewDefinition (SimpleTypeDefn _ _) = return ()
fileNewDefinition (AttributeDefn _ _)  = return ()
fileNewDefinition (SequenceDefn _ _)   = return ()
fileNewDefinition (ElementDefn n t)    = do
  whenDebugging $ do
    liftIO $ putStrLn $ "Filing ElementDefn: " ++ n ++ " :: " ++ t
  addElementType n t

addElementType :: String -> String -> XSDQ ()
addElementType name typ = liftStatetoXSDQ $ do
  (QdxhbState opts elemTypes) <- get
  put (QdxhbState opts $ (name, typ) : elemTypes)

getElementType :: String -> XSDQ (Maybe String)
getElementType name = liftStatetoXSDQ $ do
  (QdxhbState _ elemTypes) <- get
  return $ lookupFirst elemTypes name

getElementTypeOrFail :: String -> XSDQ String
getElementTypeOrFail name = do
  typM <- getElementType name
  case typM of
    Just typ -> return typ
    Nothing -> error $ "Undefined element " ++ name

-- |Return the QDHXBOptionSet in effect for this run.
getOptions :: XSDQ (QDHXBOptionSet)
getOptions = liftStatetoXSDQ $ do
  (QdxhbState opts _) <- get
  return opts

-- |Return whether @newtype@ should be used in this run.
getUseNewtype :: XSDQ Bool
getUseNewtype = fmap optUseNewType getOptions

-- |Return whether debugging output should be generated in this run.
getDebugging :: XSDQ Bool
getDebugging = fmap optDebugging getOptions

-- |Guard a block executed only when debugging is activated.
whenDebugging :: XSDQ () -> XSDQ ()
whenDebugging = whenM getDebugging

-- ------------------------------------------------------------

lookupFirst :: [(String, a)] -> String -> Maybe a
lookupFirst [] _ = Nothing
lookupFirst ((fnd, x):_) targ | fnd == targ = Just x
lookupFirst (_:xs) targ = lookupFirst xs targ

