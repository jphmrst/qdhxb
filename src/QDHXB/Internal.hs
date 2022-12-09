{-# LANGUAGE TemplateHaskell #-}

module QDHXB.Internal (ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
                       ItemDefn(SimpleRep, AttributeRep, SequenceRep),

                       hdecls)
where

import Language.Haskell.TH
-- import System.Directory
-- import Control.Monad.IO.Class
-- import Data.Char
-- import Text.XML.Light.Types
import QDHXB.TH
-- import QDHXB.XMLLight

data ItemRef =
  ElementItem String IntOrUnbound IntOrUnbound
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
  hrefOut <- mapM href refs
  return [ DataD [] nam [] Nothing [NormalC nam $ hrefOut] [] ]

href :: ItemRef -> Q BangType
href (ElementItem ref lower upper) = do
  typ <- containForBounds lower upper $ return $ ConT $ mkName $ firstToUpper ref
  return (useBang, typ)
href (AttributeItem ref) =
  return (useBang,
          ConT $ mkName $ firstToUpper $ ref ++ "AttrType")
href (ComplexTypeItem ref) =
  return (useBang, ConT $ mkName $ firstToUpper ref)

useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness
