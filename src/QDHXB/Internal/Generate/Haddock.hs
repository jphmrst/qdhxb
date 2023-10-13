
{-| Helpers for Haddock documentation generation. -}

module QDHXB.Internal.Generate.Haddock (pushDeclHaddock) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import QDHXB.Internal.XSDQ

-- | Install Haddock documentation for the top-level declaration
-- associated with the given name.
pushDeclHaddock ::
  Maybe String -- ^ If present, is separated from the leading
               -- documentation with a colon.  If absent, a full
               -- stop/period is appended to the leading
               -- documentation.
  -> Name
  -> String -- ^ The leading documentation for the declaration.
  -> XSDQ ()
pushDeclHaddock ifDoc = do
  pushDeclHaddock' $ maybe "." (": " ++) ifDoc

pushDeclHaddock' :: String -> Name -> String -> XSDQ ()
pushDeclHaddock' suffix name spec = do
  liftQtoXSDQ $ addModFinalizer $ putDoc (DeclDoc name) $ spec ++ suffix
