{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}     -- For Hoistable
{-# LANGUAGE ConstraintKinds, ExistentialQuantification, ImpredicativeTypes #-} -- For assembleIfUpdated

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.AST (MaybeUpdated(..), Hoistable, hoistUpdate,
                           Upd(..), assembleIfUpdated,
                           Substitutions, substString, substQName,
                           AST(..)) where

-- import Text.XML.Light.Output
import Data.Kind (Type)
import Text.XML.Light.Types
import QDHXB.Utils.XMLLight (inSameNamspace)
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Types
import QDHXB.Utils.BPP
import QDHXB.Utils.Misc (applySnd)

-- | Finite set of substitutions of one `QName` for another.
type Substitutions = [(String, String)]

-- | Helper datatype noting whether some values was changed by a
-- process, or whether a function result differs from a corresponding
-- argument.
data MaybeUpdated a = Same { resultOnly :: a } | Updated { resultOnly :: a }

instance Blockable a => Blockable (MaybeUpdated a) where
  block (Same v) = stringToBlock "[Unchanged]" `stack2` block v
  block (Updated v) = stringToBlock "[Updated]" `stack2` block v

instance Functor MaybeUpdated where
  fmap f (Same x) = Same $ f x
  fmap f (Updated x) = Updated $ f x

-- | Not exported: class of types to which `hoistUpdate` applies.
class Hoistable (k :: Type -> Type) where
  -- | Combine a list of marked values into a marked list of unmarked
  -- values.
  hoistUpdate :: k (MaybeUpdated a) -> MaybeUpdated (k a)

instance Hoistable [] where
  hoistUpdate zs = let (anyUpdated, ys) = hoistUpdate' True zs
                   in if anyUpdated then Updated ys else Same ys
    where hoistUpdate' b [] = (b, [])
          hoistUpdate' b (Same x : xs) = applySnd (x :) $ hoistUpdate' b xs
          hoistUpdate' _ (Updated x : xs) =
            applySnd (x :) $ hoistUpdate' False xs

instance Hoistable Maybe where
  hoistUpdate (Just (Same x)) = Same $ Just x
  hoistUpdate (Just (Updated x)) = Updated $ Just x
  hoistUpdate Nothing = Updated Nothing

-- | Hack for existential types in `assembleIfUpdated` --- see
-- https://jozefg.bitbucket.io/posts/2014-01-14-existential.html
data Upd = forall a . Upd (MaybeUpdated a)

-- | Given several possible updates as `MaybeUpdated` values, assemble
-- a new value from updated components, or return a previous value if
-- no updates existed.
assembleIfUpdated :: [Upd] -> b -> b -> MaybeUpdated b
assembleIfUpdated elems ifPrev ifNew =
  if hasUpdate elems then Updated ifNew else Same ifPrev
  where hasUpdate :: [Upd] -> Bool
        hasUpdate [] = False
        hasUpdate (Upd (Same _) : elems') = hasUpdate elems'
        hasUpdate (Upd (Updated _) : _) = True

-- | Class of abstract syntax trees @ast@ which
class (Blockable ast, Blockable [ast]) => AST ast where

  -- | Rewrite otherwise-unstructured parsed XML content as a sequence
  -- of ASTs.
  decodeXML :: [Content] -> XSDQ [ast]

  -- | Traverse a list of ASTs to collect the top-level bound names.
  getBoundNameStrings :: [ast] -> [String]
  getBoundNameStrings = concat . map getBoundNameStringsFrom

  -- | Traverse a single AST to collect the top-level bound names.
  getBoundNameStringsFrom :: ast -> [String]

  -- | Rename any nonunique hidden names within the scope of the given
  -- @ast@.  This is a case over the structure of the @ast@ type, and
  -- applying `ensureUniqueNames` to recursively-held lists of ASTs.
  -- Used by `QDHXB.Internal.AST.ensureUniqueNames`.
  ensureUniqueInternalNames :: ast -> XSDQ (MaybeUpdated ast)

  -- | Apply the given substitutions to the given ASTs.
  applySubstitutions :: Substitutions -> [ast] -> MaybeUpdated [ast]
  applySubstitutions substs = hoistUpdate . map (applySubstitutionsTo substs)

  -- | Apply the given substitutions to the given AST.
  applySubstitutionsTo :: Substitutions -> ast -> MaybeUpdated ast

  -- | Helper function for the `ensureUniqueNames` method, noting
  -- whether the result and argument differ.
  ensureUniqueNames' :: [ast] -> XSDQ (MaybeUpdated [ast])
  ensureUniqueNames' dss = do
    whenDebugging $ dbgLn "Starting ensureUniqueNames'"

    -- First rename any nonunique hidden names within the scope of
    -- each `DataScheme` in the input list.
    dssR' <- fmap hoistUpdate $ mapM ensureUniqueInternalNames dss
    let dss' = resultOnly dssR'

    -- Next harvest the top-level bound names in the `DataScheme`
    -- list.
    let bound_names = getBoundNameStrings dss'
    whenDebugging $ dbgBLabel "- bound names: " bound_names

    -- Log the fresh names, and issue a substitution for any which are
    -- already used.
    substs <- make_needed_substitutions bound_names
    whenDebugging $ dbgBLabel "- substs: " substs

    -- Now apply these substitutions.
    let dssR'' = case substs of
          [] -> dssR'
          _ -> applySubstitutions substs dss'

    dbgResult "- result: " $
      assembleIfUpdated [Upd dssR', Upd dssR''] dss $ resultOnly dssR''

  -- | Special case of `ensureUniqueNames'` for a single @ast@.
  ensureUniqueNames1 :: ast -> XSDQ (MaybeUpdated ast)
  ensureUniqueNames1 ds = do
    whenDebugging $ dbgBLabel "- ensureUniqueNames1 on " $ debugSlug ds

    -- First rename any nonunique hidden names within the scope of the
    -- `DataScheme`.
    dsR' <- ensureUniqueInternalNames ds
    let ds' = resultOnly dsR'

    -- Next harvest any top-level bound name(s?) in the `DataScheme`.
    let bound_names = getBoundNameStringsFrom ds'

    -- Log the fresh names, and issue a substitution for any which are
    -- already used.
    substs <- make_needed_substitutions bound_names

    -- Now apply these substitutions.
    let dsR'' = case substs of
          [] -> dsR'
          _ -> applySubstitutionsTo substs ds'

    return $ assembleIfUpdated [Upd dsR', Upd dsR''] ds $ resultOnly dsR''

  -- | Pipeline step renaming multiply-used names in different nested
  -- scopes.  Since Haskell does not have nested scoping of types and
  -- constructors, all of these bindings will be visible as top-level
  -- declarations.  So subsequent bindings of the same name must be
  -- renamed before we lose the structure of nested scopes in the
  -- `QDHXB.Internal.L0.Flatten` step.
  --
  -- This is the top-level call of the various renaming-related
  -- functions in the `AST` class.  You know it's top-level because
  -- the intermediate-copy-avoiding `MaybeUpdated` tag is removed!
  ensureUniqueNames :: [ast] -> XSDQ [ast]
  ensureUniqueNames dss = do
    whenDebugging $ dbgLn "Starting ensureUniqueNames"
    fmap resultOnly $ ensureUniqueNames' dss

  -- | Rewrite the nested AST into short, flat definitions closer to
  -- Haskell declarations.
  flatten :: [ast] -> XSDQ [Definition]

  -- | For debugging
  debugSlug :: ast -> String

-- | Process a list of names with respect to the current `XSDQ` state:
-- log the fresh names as in use, and issue a substitution for any
-- which are already used.
make_needed_substitutions :: [String] -> XSDQ Substitutions
make_needed_substitutions = make_needed_substitutions' []
  where make_needed_substitutions' ::
          Substitutions -> [String] -> XSDQ Substitutions
        make_needed_substitutions' acc [] = return acc
        make_needed_substitutions' acc (x:xs) = do
          inUse <- typeNameIsInUse x
          if inUse
            then do
              n <- getNextDisambig
              suffix <- getDisambigString
              let fresh = x ++ suffix ++ show n
              addUsedTypeName fresh
              make_needed_substitutions' ((x,fresh):acc) xs
            else do
              addUsedTypeName x
              make_needed_substitutions' acc xs

-- | Apply a substitution to a `String`.
substString :: Substitutions -> String -> MaybeUpdated String
substString [] q = Same q
substString ((q1,q2):_) q | q == q1 = Updated q2
substString (_:substs) q = substString substs q

-- | Apply a substitution to a `QName`.
substQName :: Substitutions -> QName -> MaybeUpdated QName
substQName [] q = Same q
substQName ((q1,q2):_) q | qName q == q1 = Updated $ inSameNamspace q2 q
substQName (_:substs) q = substQName substs q
