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
import QDHXB.Internal.FlatTypes
import QDHXB.Internal.XSDQ

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
xsdDeclsToHaskell :: [ItemDefn] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM xsdDeclToHaskell defns

-- | Translate one XSD definition to a Template Haskell quotation
-- monad, usually updating the internal state to store the new
-- `ItemDefn`.
xsdDeclToHaskell :: ItemDefn -> XSDQ [Dec]
xsdDeclToHaskell decl@(SimpleRep nam typ) =
  let baseName = firstToUpper nam
      decNam = mkName $ "decode" ++ baseName
      -- encNam = mkName $ "encode" ++ baseName
      loadNam  = mkName $ "load" ++ baseName
      -- writeNam = mkName $ "write" ++ baseName
  in do
    fileNewItemDefn decl
    let (haskellType, basicDecoder) = xsdTypeNameTranslation typ
    decoder <- fmap basicDecoder
                 [| case pullCRefContent $(return $ LitE $ StringL nam) ctxt of
                      Nothing -> error $ "QDHXB: CRef must be present within " ++ nam
                      Just v -> v |]
    return $
      TySynD (mkName baseName) [] haskellType

      -- Decoder
      : SigD decNam (fn1Type (ConT $ mkName "Content")
                             (ConT $ mkName baseName))
      : FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]

      {-
      -- TODO Encoder
      SigD encNam encType
      FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                             (LitE $ StringL "TODO")) []]
      -}

      -- Reader
      : SigD loadNam (fn1Type (ConT $ mkName "String")
                              (AppT (ConT $ mkName "IO")
                                    (ConT $ mkName baseName)))
      : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "loadElement")
                                                (VarE decNam)) []]

      {-
      -- TODO Writer
      (SigD writeNam $ VarT $ mkName "a")
      FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                               (LitE $ StringL "TODO")) []]
      -}

      : []
xsdDeclToHaskell decl@(AttributeRep nam typ usage) =
  let rootName = firstToUpper nam
      rootTypeName = mkName $ rootName ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      -- encNam = mkName $ "encode" ++ rootName
      -- loadNam  = mkName $ "load" ++ rootName
      -- writeNam = mkName $ "write" ++ rootName
  in do
    fileNewItemDefn decl
    coreDecoder <- [| pullAttrFrom $(return $ LitE $ StringL nam) ctxt |]
    decoder <- unpackAttrDecoderForUsage usage coreDecoder
    let (haskellTyp, _) = xsdTypeNameTranslation typ
    return $
      TySynD rootTypeName [] (attrTypeForUsage usage haskellTyp)

      -- Decoder
      : SigD decNam (fn1Type (ConT $ mkName "Content")
                             (ConT rootTypeName))
      : FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]

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
      : []
xsdDeclToHaskell decl@(SequenceRep namStr refs) =
  let nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      decNam = mkName $ "decode" ++ nameRoot
      -- encNam = mkName $ "encode" ++ nameRoot
      loadNam  = mkName $ "load" ++ nameRoot
      -- writeNam = mkName $ "write" ++ nameRoot
  in do
    fileNewItemDefn decl
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
      -- Type declaration
      DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
            [DerivClause Nothing [ConT $ mkName "Eq", ConT $ mkName "Show"]]

      -- Decoder
       : SigD decNam
              (fn1Type (ConT $ mkName "Content") (ConT $ mkName nameRoot))
       : FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]

       {-
      -- TODO Encoder
       : SigD encNam encType
       : FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                (LitE $ StringL "TODO")) []]
       -}

      -- Reader
       : SigD loadNam (fn1Type (ConT $ mkName "String")
                               (AppT (ConT $ mkName "IO")
                                     (ConT $ mkName nameRoot)))
       : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "loadElement")
                                                 (VarE decNam)) []]
       {-
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
xsdRefToHaskellExpr param (ElementItem ref occursMin occursMax) =
  let casePrefix = CaseE $ subcontentZom ref param
  in case (occursMin, occursMax) of
    (_, Just 0) -> return $ TupE []
    (Just 0, Just 1) -> do
      matches <- zomMatch1
        (ConE $ mkName "Nothing")
        (\paramName -> AppE (ConE $ mkName "Just")
                            (AppE (decoderExpFor ref) (VarE paramName)))
        (throwsError "QDHXB: should not return multiple results")
      return $ casePrefix matches
    (_, Just 1) -> do
      matches <- zomMatch1
        (throwsError "QDHXB: should not return zero results")
        (\paramName -> AppE (decoderExpFor ref) (VarE paramName))
        (throwsError "QDHXB: should not return multiple results")
      return $ casePrefix matches
    _ -> return $ AppE (AppE (VarE $ mkName "map") (decoderExpFor ref))
                       (AppE (VarE $ mkName "zomToList")
                             (subcontentZom ref param))
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
  AppE (AppE (VarE $ mkName "pullContentFrom") (LitE (StringL ref))) (VarE param)

zomMatches :: Exp -> (Name -> Exp) -> (Name -> Exp) -> XSDQ [Match]
zomMatches zeroCase oneCaseF manyCaseF = do
  newX <- newName "x"
  newXS <- newName "xs"
  assembleZomMatches zeroCase
                     (VarP newX) (oneCaseF newX)
                     (VarP newXS) (manyCaseF newXS)

zomMatch1 :: Exp -> (Name -> Exp) -> Exp -> XSDQ [Match]
zomMatch1 zeroCase oneCaseF manyCaseExp = do
  newX <- newName "x"
  assembleZomMatches zeroCase
                     (VarP newX) (oneCaseF newX)
                     WildP (manyCaseExp)

assembleZomMatches :: Exp -> Pat -> Exp -> Pat -> Exp -> XSDQ [Match]
assembleZomMatches zeroCase onePattern oneCase manyPattern manyCase =
  return $ [
    Match (ConP zeroName [] []) (NormalB zeroCase) [],
    Match (ConP oneName [] [onePattern]) (NormalB oneCase) [],
    Match (ConP manyName [] [manyPattern]) (NormalB manyCase) []
    ]

maybeMatches :: Exp -> (Name -> Exp) -> XSDQ [Match]
maybeMatches zeroCase oneCaseF = do
  newX <- newName "x"
  return $ [
    Match (ConP nothingName [] []) (NormalB zeroCase) [],
    Match (ConP justName [] [VarP newX]) (NormalB $ oneCaseF newX) []
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

attrTypeForUsage :: AttributeUsage -> Type -> Type
attrTypeForUsage Forbidden _ = TupleT 0
attrTypeForUsage Optional typ = AppT (ConT $ mkName "Maybe") typ
attrTypeForUsage Required typ = typ

unpackAttrDecoderForUsage :: AttributeUsage -> Exp -> XSDQ Exp
unpackAttrDecoderForUsage Forbidden _ = return $ TupE []
unpackAttrDecoderForUsage Optional expr = return expr
unpackAttrDecoderForUsage Required expr = fmap (CaseE expr) $
  maybeMatches (throwsError "QDHXB: should not return Nothing")
               (\paramName -> VarE paramName)

-- | Handy abbreviation of some TH boilerplate.
useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness
