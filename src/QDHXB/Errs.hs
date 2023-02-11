{-# LANGUAGE TemplateHaskell #-}

-- | Exceptions raised in QDHXB code generation.
module QDHXB.Errs(HXBErr(..), HXBExcept) where
import Control.Monad.Except
import Text.XML.Light.Types (Line)
import QDHXB.Internal.Utils.BPP

-- | Exceptions raised in `QDHXB.Internal.XSDQ` computations.
data HXBErr =
  MiscError -- ^ Situation not covered by another case
      String -- ^ Plain-language description of the exception
      (Maybe Line) -- ^ Location of the element.
  | NoValidContentInUnion -- ^ None of the alternative interpretations
                          -- of the simple-type text contents of a
                          -- union could successfully interpret
                          -- content found in this element.
      String -- ^ Union element tag.
      (Maybe Line) -- ^ Location of the element.
  | AtMostOnceIn -- ^ An element which may occur at most once within a
                 -- container was found more than once
      String -- ^ Contained element tag.
      String -- ^ Container element tag.
      (Maybe Line) -- ^ Location of the element.
  | MustBePresentIn -- ^ An element which must occur within a
                    -- container was not found
      String -- ^ Missing/contained element tag.
      String -- ^ Container element tag.
      (Maybe Line) -- ^ Location of the element.
  deriving Eq

instance Blockable HXBErr where
  block (MiscError s _) = stringToBlock $ "Misc error: " ++ s
  block (NoValidContentInUnion s _) =
    stringToBlock $ "No interpretation of contents for <" ++ s ++ "> union"

instance Show HXBErr where show = bpp

-- | An `Except` computation which may throw an `HXBErr`.
type HXBExcept a = Except HXBErr a
