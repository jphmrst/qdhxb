{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.UniqueNames (Renamable(..), ensureUniqueNames) where

import Text.XML.Light.Output
import Text.XML.Light.Types
import QDHXB.Internal.XSDQ
import QDHXB.Utils.BPP

-- | Class of abstract syntax trees @ast@ which
class (Blockable ast, Blockable [ast]) => Renamable ast where

  -- | Traverse a list of ASTs to collect the top-level bound names.
  get_bound_name_strings :: [ast] -> [String]
  get_bound_name_strings = map concat . map get_bound_name_strings_from

  -- | Traverse a single AST to collect the top-level bound names.
  get_bound_name_strings_from :: ast -> [String]

  -- | Rename any nonunique hidden names within the scope of the given
  -- @ast@.  This is a case over the structure of the @ast@ type, and
  -- applying `ensureUniqueNames` to recursively-held lists of ASTs.
  ensure_unique_internal_names :: ast -> XSDQ ast

  -- | Apply the given substitutions to the given ASTs.
  apply_substitutions :: [(String, String)] -> [ast] -> [ast]
  apply_substitutions substs = map (apply_substitutions' substs)

  -- | Apply the given substitutions to the given AST.
  apply_substitutions' :: [(String, String)] -> ast -> ast

-- | Pipeline step renaming multiply-used names in different nested
-- scopes.  Since Haskell does not have nested scoping of types and
-- constructors, all of these bindings will be visible as top-level
-- declarations.  So subsequent bindings of the same name must be
-- renamed before we lose the structure of nested scopes in the
-- `QDHXB.Internal.L0.Flatten` step.
ensureUniqueNames :: Renamable ast => [ast] -> XSDQ [ast]
ensureUniqueNames dss = do
  -- First rename any nonunique hidden names within the scope of each
  -- `DataScheme` in the input list.
  dss' <- mapM ensure_unique_internal_names dss

  -- Next harvest the top-level bound names in the `DataScheme` list.
  let bound_names = get_bound_name_strings dss'

  -- Log the fresh names, and issue a substitution for any which are
  -- already used.
  substs <- make_needed_substitutions bound_names

  -- Now apply these substitutions.
  let dss'' = case substs of
        [] -> dss'
        _ -> apply_substitutions substs dss'

  return dss''

-- | TODO Process a list of names with respect to the current `XSDQ`
-- state: log the fresh names as in use, and issue a substitution for
-- any which are already used.
make_needed_substitutions :: [String] -> XSDQ [(String, String)]
make_needed_substitutions _ = return []

