{-# LANGUAGE TemplateHaskell #-}

module QDHXB.Internal (IntOrUnbound(Bound, Unbounded),
                       decodeIntOrUnbound,
                       ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
                       ItemDefn(SimpleRep, AttributeRep, SequenceRep),

                       hdecls)
where

import Language.Haskell.TH
-- import System.Directory
import Control.Monad.IO.Class
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
  deriving Show

data ItemDefn =
  SimpleRep String String
  | AttributeRep String String
  | SequenceRep String [ItemRef]
  deriving Show

hdecls :: [ItemDefn] -> Q [Dec]
hdecls defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM hdecl defns

hdecl :: ItemDefn -> Q [Dec]
hdecl (SimpleRep nam typ) =
  return [TySynD (mkName $ firstToUpper nam) [] (decodeTypeAttrVal typ)]
hdecl (AttributeRep nam typ) =
  return [ TySynD (mkName $ firstToUpper nam ++ "AttrType")
                  [] (decodeTypeAttrVal typ)
         ]
hdecl (SequenceRep namStr refs) = do
  let nam = mkName $ firstToUpper namStr
  return [ DataD [] nam [] Nothing [NormalC nam $ map href refs] [] ]

href :: ItemRef -> BangType
href (ElementItem ref) = (useBang, ConT $ mkName $ firstToUpper ref)
href (AttributeItem ref) = (useBang,
                            ConT $ mkName $ firstToUpper $ ref ++ "AttrType")
href (ComplexTypeItem ref) = (useBang, ConT $ mkName $ firstToUpper ref)

useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness
