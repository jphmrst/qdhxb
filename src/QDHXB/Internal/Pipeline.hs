{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

-- | Top-level XSD-to-Haskell rewriting pipeline.
module QDHXB.Internal.Pipeline () where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
-- import Data.Char
import Text.XML.Light.Types (Content(Elem), Element(Element), QName(QName))
import QDHXB.Utils.BPP
import QDHXB.Utils.XMLLight (getCoreContent)
import QDHXB.Internal.XSDQ
import QDHXB.Internal.AST
