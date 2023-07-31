{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Translate parsed but otherwise unstructured XSD into the first
-- internal representation, allowing nested type definitions.
module QDHXB.Internal.L0.UniqueNames (ensureUniqueNames) where

import Text.XML.Light.Output
import Text.XML.Light.Types
import QDHXB.Internal.L0.NestedTypes
import QDHXB.Internal.XSDQ

-- | Pipeline step renaming multiply-used names in different nested
-- scopes.  Since Haskell does not have nested scoping of types and
-- constructors, all of these bindings will be visible as top-level
-- declarations.  So subsequent bindings of the same name must be
-- renamed before we lose the structure of nested scopes in the
-- `QDHXB.Internal.L0.Flatten` step.
ensureUniqueNames :: [DataScheme] -> XSDQ [DataScheme]
ensureUniqueNames dss = do
  -- First rename any nonunique hidden names within the scope of each
  -- `DataScheme` in the input list.
  dss' <- mapM ensure_unique_internal_names dss

  -- Next harvest the top-level bound names in the `DataScheme` list.
  bound_names <- get_bound_name_strings dss'

  -- Log the fresh names, and issue a substitution for any which are
  -- already used.
  substs <- make_needed_substitutions bound_names

  -- Now apply these substitutions.
  let dss'' = case substs of
        [] -> dss'
        _ -> apply_substitutions substs dss'

  return dss''

-- | TODO Rename any nonunique hidden names within the scope of each
-- `DataScheme` in the input list.  This is a case over the structure
-- of the `DataScheme`, and applying `ensureUniqueNames` to
-- recursively-held lists of `DataScheme`s.
ensure_unique_internal_names :: DataScheme -> XSDQ DataScheme
ensure_unique_internal_names = return

-- | TODO Traverse the AST to collect the top-level bound names in the
-- argument `DataScheme` list.
get_bound_name_strings :: [DataScheme] -> XSDQ [String]
get_bound_name_strings = fmap concat . mapM get_bound_name_strings'

-- | TODO Traverse the AST to collect the top-level bound names in the
-- argument `DataScheme`.
get_bound_name_strings' :: DataScheme -> XSDQ [String]
get_bound_name_strings' _ = return []

-- | TODO Process a list of names with respect to the current `XSDQ`
-- state: log the fresh names as in use, and issue a substitution for
-- any which are already used.
make_needed_substitutions :: [String] -> XSDQ [(String, String)]
make_needed_substitutions _ = return []

-- | TODO Apply the given substitutions to the given `DataScheme`s.
apply_substitutions :: [(String, String)] -> [DataScheme] -> [DataScheme]
apply_substitutions substs = map (apply_substitutions' substs)

-- | TODO Apply the given substitutions to the given `DataScheme`.
apply_substitutions' :: [(String, String)] -> DataScheme -> DataScheme
apply_substitutions' _ = id
