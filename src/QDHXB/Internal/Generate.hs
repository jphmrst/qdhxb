{-# LANGUAGE TemplateHaskell #-}

-- | Our internal representation of XSD elements.
module QDHXB.Internal.Generate (
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
-- import Text.XML.Light.Types
import QDHXB.TH
import QDHXB.XMLLight
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
xsdDeclsToHaskell :: [ItemDefn] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM xsdDeclToHaskell defns

-- | Translate one XSD definition to a Template Haskell quotation monad.
xsdDeclToHaskell :: ItemDefn -> XSDQ [Dec]
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
    let binderMapper :: (Name, ItemRef) -> XSDQ Dec
        binderMapper (n, r) = do
          body <- xsdRefToHaskellExpr (mkName "ctxt") r
          return $ ValD (VarP n) (NormalB body) []
    let subNames = map (mkName . ("s" ++) . show) [1..length refs]
    binders <- mapM binderMapper $ zip subNames refs
    let decoder = LetE binders $
                    foldl (\x y -> AppE x y) (ConE typNam) (map VarE subNames)
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
xsdRefToHaskellExpr :: Name -> ItemRef -> XSDQ Exp
xsdRefToHaskellExpr param (ElementItem ref min max) =
  let casePrefix = CaseE $ subcontentZom ref param
  in case (min, max) of
    (_, Just 0) -> return $ TupE []
    (Just 0, Just 1) -> do
      matches <- zomMatches
        (ConE $ mkName "Nothing")
        (\paramName -> AppE (ConE $ mkName "Just")
                            (AppE (decoderExpFor ref) (VarE paramName)))
        (\_ -> throwsError "QDHXB: should not return multiple results")
      return $ casePrefix matches
    (_, Just 1) -> do
      matches <- zomMatches
        (throwsError "QDHXB: should not return zero results")
        (\paramName -> AppE (decoderExpFor ref) (VarE paramName))
        (\_ -> throwsError "QDHXB: should not return multiple results")
      return $ casePrefix matches
    _ -> return $ AppE (AppE (VarE $ mkName "map") (decoderExpFor ref))
                       (subcontentZom ref param)
xsdRefToHaskellExpr param (AttributeItem ref) = xsdRefToHaskellExpr' param ref
xsdRefToHaskellExpr param (ComplexTypeItem ref) = xsdRefToHaskellExpr' param ref

-- | Helper for `xsdRefToHaskellExpr`.
xsdRefToHaskellExpr' :: Name -> String -> XSDQ Exp
xsdRefToHaskellExpr' param ref = return $ AppE (decoderExpFor ref) (VarE param)

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
decoderExpFor :: String -> Exp
decoderExpFor ref = VarE $ mkName $ "decode" ++ firstToUpper ref

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
subcontentZom :: String -> Name -> Exp
subcontentZom ref param =
  AppE (AppE (VarE $ mkName "pullContent") (LitE (StringL ref))) (VarE param)

zomMatches :: Exp -> (Name -> Exp) -> (Name -> Exp) -> XSDQ [Match]
zomMatches zeroCase oneCaseF manyCaseF = do
  newX <- newName "x"
  newXS <- newName "xs"
  return $ [
    Match (ConP zeroName [] []) (NormalB zeroCase) [],
    Match (ConP oneName [] [VarP newX]) (NormalB $ oneCaseF newX) [],
    Match (ConP manyName [] [VarP newXS]) (NormalB $ manyCaseF newXS) []
    ]

-- | Translate a reference to an XSD element type to a Template Haskell
-- quotation monad returning a type.
xsdRefToBangTypeQ :: ItemRef -> XSDQ BangType
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
