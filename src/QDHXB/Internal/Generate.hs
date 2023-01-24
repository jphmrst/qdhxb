{-# LANGUAGE TemplateHaskell #-}

-- | Generate Haskell code from the flattened internal representation.
module QDHXB.Internal.Generate (
  -- * The representation types
  Reference(ElementRef, AttributeRef  {-, ComplexTypeRef -} ),
  Definition(SimpleTypeDefn, AttributeDefn, SequenceDefn, ElementDefn),

  -- * Code generation from the internal representation
  xsdDeclsToHaskell,

  -- * Functions appearing in the generated code
  __decodeForSimpleType
  )
where

import Control.Monad.IO.Class
import Data.List (intercalate)
import Language.Haskell.TH
import Text.XML.Light.Types (Content)
import QDHXB.Internal.Utils.TH
import QDHXB.Internal.Utils.XMLLight
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
xsdDeclsToHaskell :: [Definition] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  -- liftIO $ putStrLn $ show defns
  fmap concat $ mapM xsdDeclToHaskell defns

-- | Translate one XSD definition to a Template Haskell quotation
-- monad, usually updating the internal state to store the new
-- `Definition`.
xsdDeclToHaskell :: Definition -> XSDQ [Dec]
xsdDeclToHaskell decl@(SimpleTypeDefn nam typ) =
  let baseName = firstToUpper nam
      -- decNam = mkName $ "decode" ++ baseName
      decAsNam = mkName $ "decodeAs" ++ baseName
      -- encNam = mkName $ "encode" ++ baseName
      -- loadNam  = mkName $ "load" ++ baseName
      -- writeNam = mkName $ "write" ++ baseName
  in do
    fileNewDefinition decl
    let (haskellType, basicDecoder) = xsdTypeNameTranslation typ
    decodeAs <- fmap basicDecoder
                  [| __decodeForSimpleType e
                         ctxt
                         $(return $ LitE $ StringL $
                            "QDHXB: CRef must be present within " ++ nam) |]
    let res = (
          TySynD (mkName baseName) [] haskellType

          {-
          -- Decoder
          : SigD decNam (fn1Type (ConT $ mkName "Content")
                                 (ConT $ mkName baseName))
          : FunD decNam [Clause [VarP $ mkName "ctxt"] (NormalB decoder) []]
          -}

          -- Decoder
          : SigD decAsNam (fn2Type (ConT $ mkName "String")
                                   (ConT $ mkName "Content")
                                   (ConT $ mkName baseName))
          : FunD decAsNam [Clause [VarP $ mkName "e", VarP $ mkName "ctxt"]
                                  (NormalB decodeAs) []]

          {-
          -- TODO Encoder
          SigD encNam encType
          FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                 (LitE $ StringL "TODO")) []]

          -- Reader
          : SigD loadNam (fn1Type (ConT $ mkName "String")
                                  (AppT (ConT $ mkName "IO")
                                        (ConT $ mkName baseName)))
          : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "loadElement")
                                                    (VarE decNam)) []]

          -- TODO Writer
          (SigD writeNam $ VarT $ mkName "a")
          FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                   (LitE $ StringL "TODO")) []]
          -}

          : [])
    whenDebugging $ do
      liftIO $ putStrLn $
        "> Generating from " ++ show decl
      liftIO $ putStrLn $ "  to " ++ indCode "     " res
    return res
xsdDeclToHaskell decl@(ElementDefn nam typ) = do
  let baseName = firstToUpper nam
      typBaseName = firstToUpper typ
      decNam = mkName $ "decode" ++ baseName
      -- encNam = mkName $ "encode" ++ baseName
      loadNam = mkName $ "load" ++ baseName
      -- writeNam = mkName $ "write" ++ baseName
  fileNewDefinition decl
  -- let (haskellType, basicDecoder) = xsdTypeNameTranslation typ
  decoder <- [| $(return $ VarE $ mkName $ "decodeAs" ++ typBaseName)
                   $(return $ LitE $ StringL nam)
                     ctxt |]
  let res = (
        -- TySynD (mkName baseName) [] haskellType

        -- Decoder
        SigD decNam (fn1Type (ConT $ mkName "Content")
                               (ConT $ mkName typBaseName))
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
                                      (ConT $ mkName typBaseName)))
        : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "loadElement")
                                                  (VarE decNam)) []]

        {-
        -- TODO Writer
        (SigD writeNam $ VarT $ mkName "a")
        FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                 (LitE $ StringL "TODO")) []]
        -}

        : [])
  whenDebugging $ do
    liftIO $ putStrLn $
      "> Generating from " ++ show decl
    liftIO $ putStrLn $ "  to " ++ indCode "     " res
  return res
xsdDeclToHaskell decl@(AttributeDefn nam typ) =
  let rootName = firstToUpper nam
      rootTypeName = mkName $ rootName ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      -- encNam = mkName $ "encode" ++ rootName
      -- loadNam  = mkName $ "load" ++ rootName
      -- writeNam = mkName $ "write" ++ rootName
  in do
    fileNewDefinition decl
    decoder <- [| pullAttrFrom $(return $ LitE $ StringL nam) ctxt |]
    -- decoder <- unpackAttrDecoderForUsage usage coreDecoder
    let (haskellTyp, _) = xsdTypeNameTranslation typ
    let res = (
          TySynD rootTypeName [] haskellTyp

          -- Decoder
          : SigD decNam (fn1Type (ConT $ mkName "Content")
                                 (AppT (ConT $ mkName "Maybe") (ConT rootTypeName)))
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
          : [])
    whenDebugging $ do
      liftIO $ putStrLn $
        "> Generating from " ++ show decl
      liftIO $ putStrLn $ "  to " ++ indCode "     " res
    return res
xsdDeclToHaskell decl@(SequenceDefn namStr refs) =
  let nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      decNam = mkName $ "decodeAs" ++ nameRoot
      -- encNam = mkName $ "encode" ++ nameRoot
      -- loadNam  = mkName $ "load" ++ nameRoot
      -- writeNam = mkName $ "write" ++ nameRoot
  in do
    fileNewDefinition decl
    hrefOut <- mapM xsdRefToBangTypeQ refs
    -- encType <- [t| $(return $ VarT typNam) -> Content |]
    -- decType <- [t| Content -> [Content] -> $(return $ VarT typNam) |]
    -- decoder <- [| pullAttrFrom $(return $ LitE $ StringL nam) ctxt |]
    let binderMapper :: (Name, Reference) -> XSDQ Dec
        binderMapper (n, r) = do
          body <- xsdRefToHaskellExpr (mkName "ctxt") r
          return $ ValD (VarP n) (NormalB body) []
    let subNames = map (mkName . ("s" ++) . show) [1..length refs]
    binders <- mapM binderMapper $ zip subNames refs
    let decoder = LetE binders $
                    foldl (\x y -> AppE x y) (ConE typNam) (map VarE subNames)
    let res = (
          -- Type declaration
          DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
                [DerivClause Nothing [ConT $ mkName "Eq", ConT $ mkName "Show"]]

          -- Decoder
           : SigD decNam
                  (fn2Type (ConT $ mkName "String") (ConT $ mkName "Content")
                           (ConT $ mkName nameRoot))
           : FunD decNam [Clause [WildP, VarP $ mkName "ctxt"]
                                 (NormalB decoder) []]

           {-
          -- TODO Encoder
           : SigD encNam encType
           : FunD encNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                    (LitE $ StringL "TODO")) []]

          -- Reader
           : SigD loadNam (fn1Type (ConT $ mkName "String")
                                   (AppT (ConT $ mkName "IO")
                                         (ConT $ mkName nameRoot)))
           : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ mkName "loadElement")
                                                     (VarE decNam)) []]
             -- TODO Writer
           : (SigD writeNam $ VarT $ mkName "a")
           : FunD writeNam [Clause [] (NormalB $ AppE (VarE $ mkName "error")
                                                      (LitE $ StringL "TODO")) []]
           -}

           : [])
    whenDebugging $ do
      liftIO $ putStrLn $
        "> Generating from " ++ show decl
      liftIO $ putStrLn $ "  to " ++ indCode "     " res
    return res

-- | Translate a reference to an XSD element type to a Haskell
-- `Exp`ression representation describing the extraction of the given
-- value.
xsdRefToHaskellExpr :: Name -> Reference -> XSDQ Exp
xsdRefToHaskellExpr param (ElementRef ref occursMin occursMax) =
  let casePrefix = CaseE $ subcontentZom ref param
  in do
    typeName <- getElementTypeOrFail ref
    whenDebugging $ liftIO $ putStrLn $
      "Retrieving type " ++ typeName ++ " for " ++ ref
    case (occursMin, occursMax) of
      (_, Just 0) -> return $ TupE []
      (Just 0, Just 1) -> do
        matches <- zomMatch1
          (ConE $ mkName "Nothing")
          (\paramName -> AppE (ConE $ mkName "Just")
                              (AppE (AppE (decoderAsExpFor typeName)
                                          (LitE $ StringL ref))
                                    (VarE paramName)))
          (throwsError "QDHXB: should not return multiple results")
        return $ casePrefix matches
      (_, Just 1) -> do
        matches <- zomMatch1
          (throwsError "QDHXB: should not return zero results")
          (\paramName -> AppE (AppE (decoderAsExpFor typeName)
                                          (LitE $ StringL ref))
                                    (VarE paramName))
          (throwsError "QDHXB: should not return multiple results")
        return $ casePrefix matches
      _ -> return $ AppE (AppE (VarE $ mkName "map")
                               (AppE (decoderAsExpFor typeName)
                                     (LitE $ StringL ref)))
                         (AppE (VarE $ mkName "zomToList")
                               (subcontentZom ref param))
xsdRefToHaskellExpr param (AttributeRef ref usage) = do
  core <- xsdRefToHaskellExpr' param ref
  unpackAttrDecoderForUsage usage core
  {-
xsdRefToHaskellExpr _ (AttributeRef _ Forbidden) =
  return $ TupE []
xsdRefToHaskellExpr param (AttributeRef ref Optional) =
  xsdRefToHaskellExpr' param ref
xsdRefToHaskellExpr param (AttributeRef ref Required) = do
  core <- xsdRefToHaskellExpr' param ref
  fmap (CaseE core) $ maybeMatches
    (throwsError $ "QDHXB: required attribute " ++ ref ++ " must be present")
    VarE
-}
{-
xsdRefToHaskellExpr param (ComplexTypeRef ref) = xsdRefToHaskellExpr' param ref
-}

-- | Called from generated code.
__decodeForSimpleType :: String -> Content -> String -> String
{-# INLINE __decodeForSimpleType #-}
__decodeForSimpleType elementName ctxt msgIfNothing =
  case pullCRefContent elementName ctxt of
    Nothing -> error msgIfNothing
    Just v -> v

-- | Helper for `xsdRefToHaskellExpr`.
xsdRefToHaskellExpr' :: Name -> String -> XSDQ Exp
xsdRefToHaskellExpr' param ref = return $ AppE (decoderExpFor ref) (VarE param)

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
decoderExpFor :: String -> Exp
decoderExpFor ref = VarE $ mkName $ "decode" ++ firstToUpper ref

-- | From a type name, construct the associated Haskell element
-- decoder function `Exp`ression.
decoderAsExpFor :: String -> Exp
decoderAsExpFor typ = VarE $ mkName $ "decodeAs" ++ firstToUpper typ

-- elementDefnToTypeDecoderExp :: Reference -> Exp
-- elementDefnToTypeDecoderExp (ElementDefn _ t) =

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
subcontentZom :: String -> Name -> Exp
subcontentZom ref param =
  AppE (AppE (VarE $ mkName "pullContentFrom") (LitE (StringL ref)))
       (VarE param)

{- Squash a warning --- for now, this function is not used.
zomMatches :: Exp -> (Name -> Exp) -> (Name -> Exp) -> XSDQ [Match]
zomMatches zeroCase oneCaseF manyCaseF = do
  newX <- newName "x"
  newXS <- newName "xs"
  assembleZomMatches zeroCase
                     (VarP newX) (oneCaseF newX)
                     (VarP newXS) (manyCaseF newXS)
-}

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
xsdRefToBangTypeQ :: Reference -> XSDQ BangType
xsdRefToBangTypeQ (ElementRef ref lower upper) = do
  typeName <- getElementTypeOrFail ref
  typ <- containForBounds lower upper $ return $ ConT $ mkName $
         firstToUpper typeName
  return (useBang, typ)
xsdRefToBangTypeQ (AttributeRef ref usage) =
  return (useBang,
          attrTypeForUsage usage $
            ConT $ mkName $ firstToUpper $ ref ++ "AttrType")
{-
xsdRefToBangTypeQ (ComplexTypeRef ref) =
  return (useBang, ConT $ mkName $ firstToUpper ref)
-}

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

indCode :: String -> [Dec] -> String
indCode ind = intercalate ("\n" ++ ind) . lines . pprint
