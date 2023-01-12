{-# LANGUAGE TemplateHaskell #-}

-- | Our internal representation of XSD elements.
module QDHXB.Internal (
  -- * The representation types
  ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
  ItemDefn(SimpleRep, AttributeRep, SequenceRep),

  -- * Code generation from the internal representation
  hdecls)
where

import Language.Haskell.TH
-- import System.Directory
-- import Control.Monad.IO.Class
-- import Data.Char
import Text.XML.Light.Types
import QDHXB.TH
-- import QDHXB.XMLLight

-- | A reference to an XSD element.
data ItemRef =
  ElementItem String (Maybe Int) (Maybe Int)
  -- ^ A named element type, possibly with numeric instance bounds.
  | AttributeItem String
  -- ^ The name of an attribute.
  | ComplexTypeItem String
  -- ^ The name of a complex type.
  deriving Show

-- | The actual definition of an XSD element.
data ItemDefn =
  SimpleRep String String
  -- ^ Defining one element to have the same structure as another.
  | AttributeRep String String
  -- ^ Defining the type of an attribute to be the same as another.
  | SequenceRep String [ItemRef]
  -- ^ Define an element to contain a sequence of subelements.
  deriving Show

todoStr :: String
todoStr = "TODO"

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
hdecls :: [ItemDefn] -> Q [Dec]
hdecls defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM hdecl defns

-- | Translate one XSD definition to a Template Haskell quotation monad.
hdecl :: ItemDefn -> Q [Dec]
hdecl (SimpleRep nam typ) =
  return [TySynD (mkName $ firstToUpper nam) [] (decodeTypeAttrVal typ)]
hdecl (AttributeRep nam typ) =
  return [ TySynD (mkName $ firstToUpper nam ++ "AttrType")
                  [] (decodeTypeAttrVal typ)
         ]
hdecl (SequenceRep namStr refs) = do
  let nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      decNam = mkName $ "decode" ++ nameRoot
      encNam = mkName $ "encode" ++ nameRoot
      loadNam  = mkName $ "load" ++ nameRoot
      writeNam = mkName $ "write" ++ nameRoot
  hrefOut <- mapM href refs
  encType <- [t| $(return $ VarT typNam) -> Content |]
  decType <- [t| [Content] -> $(return $ VarT typNam) |]
  return $
    DataD [] typNam [] Nothing [NormalC typNam $ hrefOut] [] -- Type decl

       -- TODO Decoder
     : SigD decNam decType
     : FunD decNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                              (VarE $ mkName todoStr)) []]

       -- TODO Encoder
     : SigD encNam encType
     : FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                              (VarE $ mkName todoStr)) []]

       -- TODO Reader
     : (SigD loadNam $ VarT $ mkName "a")
     : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                               (VarE $ mkName todoStr)) []]

       -- TODO Writer
     : (SigD writeNam $ VarT $ mkName "a")
     : FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                (VarE $ mkName todoStr)) []]
     : []

-- | Translate a reference to an XSD element type to a Template Haskell
-- quotation monad returning a type.
href :: ItemRef -> Q BangType
href (ElementItem ref lower upper) = do
  typ <- containForBounds lower upper $ return $ ConT $ mkName $ firstToUpper ref
  return (useBang, typ)
href (AttributeItem ref) =
  return (useBang,
          ConT $ mkName $ firstToUpper $ ref ++ "AttrType")
href (ComplexTypeItem ref) =
  return (useBang, ConT $ mkName $ firstToUpper ref)

-- | Handy abbreviation of some TH boilerplate.
useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness
