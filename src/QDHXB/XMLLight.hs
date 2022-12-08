{-# LANGUAGE TemplateHaskell #-}

module QDHXB.XMLLight where

import Text.XML.Light.Types

pullAttr :: String -> [Attr] -> Maybe String
pullAttr _ [] = Nothing
pullAttr nam ((Attr (QName nam' _ _) val) : ats) =
  if nam == nam' then Just val else pullAttr nam ats

isElem :: Content -> Bool
isElem (Elem _) = True
isElem _ = False
