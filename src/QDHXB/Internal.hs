{-# LANGUAGE TemplateHaskell #-}

-- | Our internal representation of XSD elements.
module QDHXB.Internal (
  -- * The representation types
  ItemRef(ElementItem, AttributeItem,  ComplexTypeItem),
  ItemDefn(SimpleRep, AttributeRep, SequenceRep),

  -- * Code generation from the internal representation
  xsdDeclsToHaskell)
where

import Language.Haskell.TH
-- import System.Directory
-- import Control.Monad.IO.Class
-- import Data.Char
import Text.XML.Light.Types
import QDHXB.TH
import QDHXB.XMLLight

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

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
xsdDeclsToHaskell :: [ItemDefn] -> Q [Dec]
xsdDeclsToHaskell defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM xsdDeclToHaskell defns

-- | Translate one XSD definition to a Template Haskell quotation monad.
xsdDeclToHaskell :: ItemDefn -> Q [Dec]
xsdDeclToHaskell (SimpleRep nam typ) =
  let baseName = firstToUpper nam
      decNam = mkName $ "decode" ++ baseName
      -- encNam = mkName $ "encode" ++ baseName
      -- loadNam  = mkName $ "load" ++ baseName
      -- writeNam = mkName $ "write" ++ baseName
  in do
    decoder <- [| pullCRefContent $(return $ LitE $ StringL nam) ctxt |]
    return [
      TySynD (mkName baseName) [] (decodeTypeAttrVal typ),

      -- TODO Decoder
      {- SigD decNam decType, -}
      FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]

      {-
      -- TODO Encoder
      SigD encNam encType
      FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                             (LitE $ StringL "TODO")) []]

      -- TODO Reader
      (SigD loadNam $ VarT $ mkName "a")
      FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                              (LitE $ StringL "TODO")) []]

      -- TODO Writer
      (SigD writeNam $ VarT $ mkName "a")
      FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                               (LitE $ StringL "TODO")) []]

  -}
      ]
xsdDeclToHaskell (AttributeRep nam typ) =
  let rootName = firstToUpper nam
      decNam = mkName $ "decode" ++ rootName
      -- encNam = mkName $ "encode" ++ rootName
      -- loadNam  = mkName $ "load" ++ rootName
      -- writeNam = mkName $ "write" ++ rootName
  in do
    decoder <- [| pullAttrFrom $(return $ LitE $ StringL nam) ctxt |]
    return [
      TySynD (mkName $ rootName ++ "AttrType") [] (decodeTypeAttrVal typ),

      -- TODO Decoder
      {- SigD decNam decType, -}
      FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]

      {-
      -- TODO Encoder
      SigD encNam encType
      FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                             (LitE $ StringL "TODO")) []]

      -- TODO Reader
      (SigD loadNam $ VarT $ mkName "a")
      FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                              (LitE $ StringL "TODO")) []]

      -- TODO Writer
      (SigD writeNam $ VarT $ mkName "a")
      FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                               (LitE $ StringL "TODO")) []]
      -}
      ]
xsdDeclToHaskell (SequenceRep namStr refs) =
  let nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      decNam = mkName $ "decode" ++ nameRoot
      -- encNam = mkName $ "encode" ++ nameRoot
      -- loadNam  = mkName $ "load" ++ nameRoot
      -- writeNam = mkName $ "write" ++ nameRoot
  in do
    hrefOut <- mapM xsdRefToBangTypeQ refs
    -- encType <- [t| $(return $ VarT typNam) -> Content |]
    -- decType <- [t| Content -> [Content] -> $(return $ VarT typNam) |]
    -- decoder <- [| pullAttrFrom $(return $ LitE $ StringL nam) ctxt |]
    let subNames = map (mkName . ("s" ++) . show) [1..length refs]

        binders = map (\(n, r) -> ValD (VarP n)
                                       (NormalB $ xsdRefToHaskellExpr (mkName "ctxt") r) []) $
                       zip subNames refs

        decoder = LetE binders $
                    foldl (\x y -> AppE x y) (VarE decNam) (map VarE subNames)
    return $
      DataD [] typNam [] Nothing [NormalC typNam $ hrefOut] [] -- Type decl

         -- TODO Decoder
       {- : SigD decNam decType -}
       : FunD decNam [Clause [VarP $ mkName "ctxt"]
                             (NormalB decoder) []]

       {-
         -- TODO Encoder
       : SigD encNam encType
       : FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                (LitE $ StringL "TODO")) []]

         -- TODO Reader
       : (SigD loadNam $ VarT $ mkName "a")
       : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                 (LitE $ StringL "TODO")) []]

         -- TODO Writer
       : (SigD writeNam $ VarT $ mkName "a")
       : FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                  (LitE $ StringL "TODO")) []]

  -}
       : []

-- | Translate a reference to an XSD element type to a Haskell
-- `Exp`ression representation describing the extraction of the given
-- value.
xsdRefToHaskellExpr :: Name -> ItemRef -> Exp
xsdRefToHaskellExpr _ (ElementItem _ (Just 0) (Just 0)) = TupE []
xsdRefToHaskellExpr _ (ElementItem _ (Just 0) (Just 1)) = TupE []
xsdRefToHaskellExpr param (ElementItem ref (Just 1) (Just 1)) =
  xsdRefToHaskellExpr' param ref
xsdRefToHaskellExpr param (ElementItem ref _ _) =
  AppE (AppE (VarE $ mkName "map") (decoderExpFor ref)) (VarE param)
xsdRefToHaskellExpr param (AttributeItem ref) = xsdRefToHaskellExpr' param ref
xsdRefToHaskellExpr param (ComplexTypeItem ref) = xsdRefToHaskellExpr' param ref

-- | Helper for `xsdRefToHaskellExpr`.
xsdRefToHaskellExpr' :: Name -> String -> Exp
xsdRefToHaskellExpr' param ref = AppE (decoderExpFor ref) (VarE param)

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
decoderExpFor :: String -> Exp
decoderExpFor ref = VarE $ mkName $ "decode" ++ firstToUpper ref

-- | Translate a reference to an XSD element type to a Template Haskell
-- quotation monad returning a type.
xsdRefToBangTypeQ :: ItemRef -> Q BangType
xsdRefToBangTypeQ (ElementItem ref lower upper) = do
  typ <-
    containForBounds lower upper $ return $ ConT $ mkName $ firstToUpper ref
  return (useBang, typ)
xsdRefToBangTypeQ (AttributeItem ref) =
  return (useBang, ConT $ mkName $ firstToUpper $ ref ++ "AttrType")
xsdRefToBangTypeQ (ComplexTypeItem ref) =
  return (useBang, ConT $ mkName $ firstToUpper ref)

-- | Handy abbreviation of some TH boilerplate.
useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness
