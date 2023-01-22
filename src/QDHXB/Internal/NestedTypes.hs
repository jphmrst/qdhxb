
-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  TypeSchemeRep(Sequence, Restriction, Extension),
  SchemeRep(
      ElementScheme, AttributeScheme, ComplexTypeScheme, SimpleTypeScheme)
) where

data TypeSchemeRep = Sequence [SchemeRep]
  | Restriction String -- ^ base
  | Extension String -- ^ base
              [SchemeRep] -- ^ additional
  deriving Show

data SchemeRep =
  ElementScheme [SchemeRep] -- ^ contents
                (Maybe String) -- ^ ifName
                (Maybe String) -- ^ ifType
                (Maybe String) -- ^ ifRef
                (Maybe Int) -- ^ ifMin
                (Maybe Int) -- ^ ifMax
  | AttributeScheme (Maybe String) -- ^ ifName
                    (Maybe String) -- ^ ifType
                    (Maybe String) -- ^ ifRef
                    String -- ^ use mode: prohibited, optional
                           -- (default), required
  | ComplexTypeScheme TypeSchemeRep -- ^ typeDetail
                      [SchemeRep] -- ^ addlAttrs
                      (Maybe String) -- ^ ifName
  | SimpleTypeScheme String -- ^ baseSpec
                     String -- ^ name
  deriving Show
