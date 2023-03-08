{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Internal monad for the XSD-to-Haskell rewriting.
module QDHXB.Internal.XSDQ (
  -- * XSD loading monad
  QdxhbState, initialQdxhbState,
  XSDQ, runXSDQ, liftIOtoXSDQ, liftQtoXSDQ, liftStatetoXSDQ,
  fileNewDefinition,
  addElementType, getElementType, getElementTypeOrFail,
  addTypeDefn, getTypeDefn, isKnownType,
  ifKnownType, isSimpleType, isComplexType,
  getOptions, getUseNewtype, getDebugging, whenDebugging, ifDebuggingDoc,
  pushNamespaces, popNamespaces, getNamespaces, getDefaultNamespace,
  decodePrefixedName, getURIprefix, getNextCapName,
  indenting, indentingWith, dbgLn, dbgBLabel, boxed,
  dbgResult,

  -- * Miscellaneous
  NameStore, containForBounds)
where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Text.XML.Light.Types
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.Namespaces
import QDHXB.Internal.Utils.Misc
import QDHXB.Internal.Types
import QDHXB.Options

-- | Synonym for an association list from a `String` to the argument
-- type.
type NameStore a = [(String, a)]

-- | Synonym for an association list from a `QName` to the argument
-- type.
type QNameStore a = [(QName, a)]

-- | The type of the internal state of an `XSDQ` computation, tracking
-- the names of XSD entities and their definitions.
data QdxhbState =
  QdxhbState QDHXBOptionSet (QNameStore QName) (QNameStore Definition)
             [Maybe String] [Namespaces] [String] String

-- | The initial value of `XSDQ` states.
initialQdxhbState :: QDHXBOption -> QdxhbState
initialQdxhbState optsF =
  QdxhbState (optsF defaultOptionSet) [] [] [] [] nameBodies ""

-- | Monadic type for loading and interpreting XSD files, making
-- definitions available after they are loaded.
newtype XSDQ a = XSDQ (StateT QdxhbState Q a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Lift a computation from the `IO` monad to the `XSDQ` wrapper.
liftIOtoXSDQ :: IO a -> XSDQ a
liftIOtoXSDQ = XSDQ . lift . liftIO

-- | Lift a computation from the `Q` monad to the `XSDQ` wrapper.
liftQtoXSDQ :: Q a -> XSDQ a
liftQtoXSDQ = XSDQ . lift

-- | Lift a computation from the `StateT` layer to the `XSDQ` wrapper.
liftStatetoXSDQ :: StateT QdxhbState Q a -> XSDQ a
liftStatetoXSDQ = XSDQ

-- | Provide the `newName` function from the internal `Q` layer to
-- top-level `XSDQ` computations.
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

-- | Register a `Definition` with the tracking tables in the `XSDQ`
-- state.
fileNewDefinition :: Definition -> XSDQ ()
fileNewDefinition (SimpleSynonymDefn _ _ _ _) = return ()
fileNewDefinition (AttributeDefn _ _ _ _)  = return ()
fileNewDefinition (SequenceDefn _ _ _ _)   = return ()
fileNewDefinition (UnionDefn _ _ _ _)   = return ()
fileNewDefinition (ListDefn _ _ _ _) = return ()
fileNewDefinition (ElementDefn n t _ _)    = do
  whenDebugging $ do
    ind <- getIndentation
    liftIO $ putStrLn $ show $
      (labelBlock (ind ++ "Filing ElementDefn: ") $ block n)
       `follow` (labelBlock " :: " $ block t)
  addElementType n t

-- | Register the `Definition` of an XSD type with the tracking tables
-- in the `XSDQ` state.
addTypeDefn :: QName -> Definition -> XSDQ ()
addTypeDefn name defn = liftStatetoXSDQ $ do
  QdxhbState opts elemTypes typeDefns dfts nss names ind <- get
  put (QdxhbState opts elemTypes ((name, defn) : typeDefns) dfts nss names ind)

-- | Return the `Definition` of an XSD type from the tracking tables
-- in the `XSDQ` state.
getTypeDefn :: QName -> XSDQ (Maybe Definition)
getTypeDefn name = liftStatetoXSDQ $ do
  QdxhbState _ _ typeDefns _ _ _ _ <- get
  return $ lookupFirst typeDefns name

-- | Return `True` if the string argument names an XSD type known to
-- the tracking tables in the `XSDQ` state.
isKnownType :: QName -> XSDQ Bool
isKnownType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just _ -> True
    _ -> False

-- | Boolean test based on `isKnownType`.
ifKnownType :: QName -> XSDQ a -> XSDQ a -> XSDQ a
{-# INLINE ifKnownType #-}
ifKnownType str thenM elseM = do
  isKnown <- isKnownType str
  if isKnown then thenM else elseM

-- | Return `True` if the string argument names a simple XSD type
-- known to the tracking tables in the `XSDQ` state.
isSimpleType :: QName -> XSDQ Bool
isSimpleType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just (SimpleSynonymDefn _ _ _ _) -> True
    _ -> False

-- | Return `True` if the string argument names a complex XSD type
-- known to the tracking tables in the `XSDQ` state.
isComplexType :: QName -> XSDQ Bool
isComplexType name = do
  defn <- getTypeDefn name
  return $ case defn of
    Just (SequenceDefn _ _ _ _) -> True
    _ -> False

-- | Register the type name associated with an element tag with the
-- tracking tables in the `XSDQ` state.
addElementType :: QName -> QName -> XSDQ ()
addElementType name typ = liftStatetoXSDQ $ do
  QdxhbState opts elemTypes typeDefns dfts nss names ind <- get
  put (QdxhbState opts ((name, typ) : elemTypes) typeDefns dfts nss names ind)

-- | Get the type name associated with an element tag from the
-- tracking tables in the `XSDQ` state, or `Nothing` if there is no
-- such element name.
getElementType :: QName -> XSDQ (Maybe QName)
getElementType name = liftStatetoXSDQ $ do
  (QdxhbState _ elemTypes _ _ _ _ _) <- get
  return $ lookupFirst elemTypes name

-- | Get the type name associated with an element tag from the
-- tracking tables in the `XSDQ` state, throwing an error if there is
-- no such element name.
getElementTypeOrFail :: QName -> XSDQ QName
getElementTypeOrFail name = do
  typM <- getElementType name
  case typM of
    Just typ -> return typ
    Nothing -> error $ "Undefined element " ++ show name

-- |Return the QDHXBOptionSet in effect for this run.
getOptions :: XSDQ (QDHXBOptionSet)
getOptions = liftStatetoXSDQ $ do
  (QdxhbState opts _ _ _ _ _ _) <- get
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

-- |Execute one of two blocks depending on whether documentation
-- debugging is selected.
ifDebuggingDoc :: XSDQ () -> XSDQ () -> XSDQ ()
ifDebuggingDoc yes no = ifM getDebugging yes no

-- |Create a new namespace scope on the `XSDQ` state corresponding to
-- the attributes, presumably from an XSD element schema.
pushNamespaces :: [Attr] -> XSDQ ()
pushNamespaces attrs = do
  liftStatetoXSDQ $ do
    QdxhbState opts elemTypes typeDefns dfts nss names ind <- get
    let (thisNS, thisDft) = decodeAttrsForNamespaces attrs
    put $ QdxhbState opts elemTypes typeDefns
                     (thisDft : dfts) (thisNS : nss) names ind
  whenDebugging $ do
    nss <- getNamespaces
    dft <- getDefaultNamespace
    liftIO $ putStrLn $ "Default namespace: " ++ show dft
    forM_ nss $ \ns -> do
      liftIO $ putStrLn $ "----------------------------------------"
      forM_ ns $ \(p,u) ->
        liftIO $ putStrLn $ p ++ " --> " ++ u

-- |Remove a namespace scope, corrsponding to finishing the processing
-- of a file.
popNamespaces :: XSDQ ()
popNamespaces = liftStatetoXSDQ $ do
  QdxhbState opts elemTypes typeDefns dfts nss names ind <- get
  case nss of
    [] -> return ()
    (_:nss') -> put $ QdxhbState opts elemTypes typeDefns dfts nss' names ind

-- |Return the stack of `Namespaces` currently known to the `XSDQ`
-- state.
getNamespaces :: XSDQ [Namespaces]
getNamespaces = liftStatetoXSDQ $ do
  QdxhbState _ _ _ _ nss _ _ <- get
  return nss

-- |Return the current target namespace URI.
getDefaultNamespace :: XSDQ (Maybe String)
getDefaultNamespace = liftStatetoXSDQ $ do
  QdxhbState _ _ _ dfts _ _ _ <- get
  getFirstActual dfts
    where getFirstActual [] = return Nothing
          getFirstActual (r@(Just _) : _) = return r
          getFirstActual (Nothing : ds) = getFirstActual ds

-- |Given a string which may be namespace-prefixed, reconstruct the
-- corresponding `QName` according to the current `XSDQ` state.
decodePrefixedName :: String -> XSDQ QName
decodePrefixedName str = do
  nss <- getNamespaces
  dft <- getDefaultNamespace
  let dftPrefix :: Maybe String
      dftPrefix = compressMaybe $
        fmap (\s -> getFirst lookupPrefixForURI s nss) dft
  return $ decodePrefixed dftPrefix dft [] nss str

getFirst ::
  (String -> Namespaces -> Maybe String) -> String -> [Namespaces] ->
    Maybe String
getFirst _ _ [] = Nothing
getFirst fn val (ns:nss) = case fn val ns of
                             Nothing -> getFirst fn val nss
                             res -> res

-- |Given the string of a URI, return the associated prefix in the
-- current namespaces.
getURIprefix :: String -> XSDQ (Maybe String)
getURIprefix uri = do
  nss <- getNamespaces
  return $ getFirst lookupPrefixForURI uri nss

lookupPrefixForURI :: String -> Namespaces -> Maybe String
lookupPrefixForURI _ [] = Nothing
lookupPrefixForURI target ((p,u):_) | target == u = Just p
lookupPrefixForURI target (_:ns) = lookupPrefixForURI target ns

-- ------------------------------------------------------------

lookupFirst :: Eq k => [(k, a)] -> k -> Maybe a
lookupFirst [] _ = Nothing
lookupFirst ((fnd, x):_) targ | fnd == targ = Just x
lookupFirst (_:xs) targ = lookupFirst xs targ

-- | Return the next generated capitalized name from the `XSDQ`
-- monad's list.  TODO --- these names are not yet checked for
-- uniqueness with respect to the names already in use.
getNextCapName :: XSDQ String
getNextCapName = liftStatetoXSDQ $ do
  QdxhbState opts elemTypes typeDefns dfts nss (z:zs) ind <- get
  put (QdxhbState opts elemTypes typeDefns dfts nss zs ind)
  return $ "Z__" ++ z

nameBodies :: [String]
nameBodies = basicNamePool ++ (concat $ map (\n -> map (n ++) basicNamePool) basicNamePool)

basicNamePool :: [String]
basicNamePool = map (\x -> [x]) ['a'..'z']

-- ------------------------------------------------------------

getIndentation :: XSDQ String
getIndentation = liftStatetoXSDQ $ do
  QdxhbState _ _ _ _ _ _ ind <- get
  return ind

setIndentation :: String -> XSDQ ()
setIndentation ind = liftStatetoXSDQ $ do
  QdxhbState opts elemTypes typeDefns dfts nss zs _ <- get
  put (QdxhbState opts elemTypes typeDefns dfts nss zs ind)

indentingWith :: String -> XSDQ a -> XSDQ a
indentingWith s m = do
  ind <- getIndentation
  setIndentation $ ind ++ s
  res <- m
  setIndentation ind
  return res

indenting :: XSDQ a -> XSDQ a
indenting = indentingWith "  "

dbgLn :: String -> XSDQ ()
dbgLn s = do
  ind <- getIndentation
  liftIO $ putStrLn $ ind ++ s

dbgBLabel :: Blockable c => String -> c -> XSDQ ()
dbgBLabel s m = do
  ind <- getIndentation
  liftIO $ bLabelPrintln (ind ++ s) m

boxed :: XSDQ a -> XSDQ a
boxed s = do
  dbgLn "+--------"
  res <- indentingWith "| " s
  dbgLn "+--------"
  return res

dbgResult :: Blockable a => String -> a -> XSDQ a
{-# INLINE dbgResult #-}
dbgResult msg res = do
  whenDebugging $ dbgBLabel ("  " ++ msg ++ " ") res
  return res
