{-# LANGUAGE TemplateHaskell #-}

module QDHXB.Internal (IntOrUnbound(Bound, Unbounded),
                       decodeIntOrUnbound,
                       ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
                       ItemDefn(SimpleRep, AttributeRep, SequenceRep),

                       hdecls)
where

import Language.Haskell.TH
-- import System.Directory
import Data.Char
import Text.XML.Light.Types
import QDHXB.TH
import QDHXB.XMLLight

data IntOrUnbound = Bound Int | Unbounded
decodeIntOrUnbound :: String -> IntOrUnbound
decodeIntOrUnbound "unbounded" = Unbounded
decodeIntOrUnbound s = Bound $ read s

data ItemRef =
  ElementItem String
  | AttributeItem String
  | ComplexTypeItem String

data ItemDefn =
  SimpleRep String String
  | AttributeRep String String
  | SequenceRep String [ItemRef]

hdecls :: [ItemDefn] -> [Dec]
hdecls = concat . map hdecl

hdecl :: ItemDefn -> [Dec]
hdecl (SimpleRep nam typ) =
  [TySynD (mkName $ firstToUpper nam) [] (decodeTypeAttrVal typ)]
hdecl (AttributeRep nam typ) =
  [TySynD (mkName $ firstToUpper nam ++ "AttrType") [] (decodeTypeAttrVal typ)]
hdecl (SequenceRep nam refs) = [] -- TODO
