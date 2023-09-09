{-# LANGUAGE TemplateHaskell #-}     -- For debugging utils
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}     -- For Hoistable
{-# LANGUAGE ConstraintKinds, ExistentialQuantification, ImpredicativeTypes #-} -- For assembleIfUpdated

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.AST (MaybeUpdated(..), Hoistable, hoistUpdate,
                           Upd(..), assembleIfUpdated,
                           Substitutions, substString, substQName,
                           makeNeededSubstitutions,
                           AST(..)) where

-- import Text.XML.Light.Output
import Data.Kind (Type)
import Text.XML.Light.Types
import QDHXB.Utils.XMLLight (inSameNamspace)
import QDHXB.Internal.Debugln
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
  block (Same _) = stringToBlock "[Unchanged]"
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
  hoistUpdate zs = let (anyUpdated, ys) = hoistUpdate' False zs
                   in if anyUpdated then Updated ys else Same ys
    where hoistUpdate' b [] = (b, [])
          hoistUpdate' b (Same x : xs) = applySnd (x :) $ hoistUpdate' b xs
          hoistUpdate' _ (Updated x : xs) =
            applySnd (x :) $ hoistUpdate' True xs

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

-- | Class of abstract syntax trees @ast@ representing possibly-nested
-- XSD declarations.
class (Blockable ast, Blockable [ast]) => AST ast where

  -- =================================================================
  -- The four top-level routines, forming a pipeline from XML input,
  -- to this AST representation, to the common flattened intermediate
  -- representation.
  -- =================================================================

  -- | Rewrite otherwise-unstructured parsed XML content as a sequence
  -- of ASTs.
  decodeXML :: [Content] -> XSDQ [ast]

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
    dbgLn unique 0 "Starting ensureUniqueNames"
    fmap resultOnly $ ensureUniqueNames' dss

  -- | Rewrite the nested AST into short, flat definitions closer to
  -- Haskell declarations.
  flatten :: [ast] -> XSDQ [Definition]

  -- | For debugging
  debugSlug :: ast -> String

  -- | Traverse a list of ASTs to collect the top-level bound names.
  getBoundNameStrings :: [ast] -> [String]
  getBoundNameStrings = concat . map getBoundNameStringsFrom


  -- =================================================================
  -- Function breaking down the implementation of `ensureUniqueNames`.
  -- =================================================================

  -- | Helper function for the `ensureUniqueNames` method, noting
  -- whether the result and argument differ.
  ensureUniqueNames' :: [ast] -> XSDQ (MaybeUpdated [ast])
  ensureUniqueNames' dss = do
    dbgLn unique 1 $
      "Starting ensureUniqueNames' on ["
      ++ foldr (\x -> \y -> debugSlug x ++ ", " ++ y) "" dss ++ "]"
    indenting $ do

      -- First harvest the top-level bound names in the `DataScheme`
      -- list.
      let bound_names = getBoundNameStrings dss
      dbgBLabel unique 2 "- top-level bound names: " bound_names

      -- Log the fresh names, and issue a substitution for any which
      -- are already used.
      substs <- indenting $ makeNeededSubstitutions bound_names
      dbgBLabel unique 2 "- substs: " substs

      -- Now apply these substitutions.
      dssR' <- case substs of
            [] -> do
              indenting $ dbgPt unique 1 "No substs"
              return $ Same dss
            _ -> do
              indenting $
                dbgPt unique 1 "Calling applySubstitutions on substs"
              return $ applySubstitutions substs dss
      dbgBLabel unique 2 "- ensureUniqueNames' dssR' is " dssR'

      -- Finally rename any nonunique hidden names within the scope of
      -- each `DataScheme` in the input list.
      dssR'' <- indenting $ fmap hoistUpdate $
        mapM ensureUniqueInternalNames $ resultOnly dssR'
      dbgBLabel unique 2 "- ensureUniqueNames' dssR'' is " dssR''

      dbgResult unique 1 "- ensureUniqueNames' result: " $
        assembleIfUpdated [Upd dssR', Upd dssR''] dss $ resultOnly dssR''


  -- | Special case of `ensureUniqueNames'` for a single @ast@.
  ensureUniqueNames1 :: ast -> XSDQ (MaybeUpdated ast)
  ensureUniqueNames1 ds = do
    dbgBLabel unique 1 "- ensureUniqueNames1 on " $ debugSlug ds

    -- First harvest any top-level bound name(s?) in the `DataScheme`.
    let bound_names = getBoundNameStringsFrom ds
    dbgBLabel unique 2 "- top-level bound names: " bound_names

    -- Log the fresh names, and issue a substitution for any which are
    -- already used.
    substs <- indenting $ makeNeededSubstitutions bound_names
    dbgBLabel unique 2 "- substs: " substs

    -- Now apply these substitutions.
    let dsR' = case substs of
          [] -> Same ds
          _ -> applySubstitutionsTo substs ds
    dbgBLabel unique 2 "- ensureUniqueNames1 dsR' is " dsR'

    -- Finally rename any nonunique hidden names within the scope of
    -- the `DataScheme`.
    dsR'' <- indenting $ ensureUniqueInternalNames $ resultOnly dsR'
    dbgBLabel unique 2 "- ensureUniqueNames1 dsR'' is " dsR''

    dbgResult unique 1 "- ensureUniqueNames1 result: " $
      assembleIfUpdated [Upd dsR', Upd dsR''] ds $ resultOnly dsR''


  -- | Apply the given substitutions to the given ASTs.
  applySubstitutions :: Substitutions -> [ast] -> MaybeUpdated [ast]
  applySubstitutions substs = hoistUpdate . map (applySubstitutionsTo substs)

  -- | Apply the given substitutions to the given AST.
  applySubstitutionsTo :: Substitutions -> ast -> MaybeUpdated ast

  -- | Traverse a single AST to collect the top-level bound names.
  getBoundNameStringsFrom :: ast -> [String]

  -- | Rename any nonunique hidden names within the scope of the given
  -- @ast@.  This is a case over the structure of the @ast@ type, and
  -- applying `ensureUniqueNames` to recursively-held lists of ASTs.
  -- Used by `QDHXB.Internal.AST.ensureUniqueNames`.
  ensureUniqueInternalNames :: ast -> XSDQ (MaybeUpdated ast)

-- =================================================================
-- Top-level (not part of the `AST` typeclass) helper functions for
-- substitutions.
-- =================================================================

-- | Process a list of names with respect to the current `XSDQ` state:
-- log the fresh names as in use, and issue a substitution for any
-- which are already used.
makeNeededSubstitutions :: [String] -> XSDQ [(String, String)]
makeNeededSubstitutions = makeNeededSubstitutions' []
  where makeNeededSubstitutions' ::
          [(String, String)] -> [String] -> XSDQ [(String, String)]
        makeNeededSubstitutions' acc [] = return acc
        makeNeededSubstitutions' acc (x:xs) = do
          inUse <- typeNameIsInUse x
          if inUse
            then do
              fresh <- freshenStringForBinding Nothing Nothing x
              makeNeededSubstitutions' ((x,fresh):acc) xs
            else do
              addUsedTypeName x
              makeNeededSubstitutions' acc xs

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
