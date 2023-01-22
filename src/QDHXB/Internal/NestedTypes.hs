
-- | Manual translation of an XSD file into the nested-definition
-- internal @ScheleRef@ representation.
module QDHXB.Internal.NestedTypes (
  TypeScheme(Sequence, Restriction, Extension),
  DataScheme(
      ElementScheme, AttributeScheme, ComplexTypeScheme, SimpleTypeScheme)
) where

data TypeScheme = Sequence [DataScheme]
  | Restriction String -- ^ base
  | Extension String -- ^ base
              [DataScheme] -- ^ additional
  deriving Show

data DataScheme =
  ElementScheme [DataScheme] -- ^ contents
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
  | ComplexTypeScheme TypeScheme -- ^ typeDetail
                      [DataScheme] -- ^ addlAttrs
                      (Maybe String) -- ^ ifName
  | SimpleTypeScheme String -- ^ baseSpec
                     String -- ^ name
  deriving Show
