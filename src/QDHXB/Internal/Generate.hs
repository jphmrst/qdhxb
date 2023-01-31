{-# LANGUAGE TemplateHaskell #-}

-- | Generate Haskell code from the flattened internal representation.
module QDHXB.Internal.Generate (
  -- * The representation types
  Reference(ElementRef, AttributeRef  {-, ComplexTypeRef -} ),
  Definition(SimpleSynonymDefn, AttributeDefn, SequenceDefn, ElementDefn),

  -- * Code generation from the internal representation
  xsdDeclsToHaskell,

  -- * Functions appearing in the generated code
  __decodeForSimpleType
  )
where

import Control.Monad.IO.Class
import Data.List (intercalate)
import Language.Haskell.TH
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, Content, qName)
import QDHXB.Internal.Utils.TH
import QDHXB.Internal.Utils.Misc
import QDHXB.Internal.Utils.XMLLight
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

-- | Translate a list of XSD definitions to a Template Haskell quotation
-- monad returning top-level declarations.
xsdDeclsToHaskell :: [Definition] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  fmap ([
        ] ++) $ fmap concat $ mapM xsdDeclToHaskell defns

-- | Translate one XSD definition to a Template Haskell quotation
-- monad, usually updating the internal state to store the new
-- `Definition`.
xsdDeclToHaskell :: Definition -> XSDQ [Dec]
xsdDeclToHaskell decl@(SimpleSynonymDefn nam typ) =
  let baseName = firstToUpper $ qName nam
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseName
      decAsNam = mkName $ "decodeAs" ++ baseName
  in do
    let (haskellType, basicDecoder) = xsdTypeNameTranslation $ qName typ
    decodeAs <- fmap basicDecoder
                  [| __decodeForSimpleType e
                         ctxt
                         $(return $ quoteStr $
                            "QDHXB: CRef must be present within " ++ show nam) |]
    let res = (
          TySynD (mkName baseName) [] haskellType

          -- Safe decoder
          : SigD safeDecAsNam
                 (fn2Type stringConT contentConT
                          (applyExceptCon stringConT
                                          (ConT $ mkName baseName)))
          : FunD safeDecAsNam [Clause [VarP eName, VarP ctxtName]
                                  (NormalB $ applyReturn decodeAs) []]

          -- Decoder
          : SigD decAsNam (fn2Type stringConT
                                   contentConT
                                   (ConT $ mkName baseName))
          : FunD decAsNam [Clause [VarP xName, VarP ctxtName]
                                  (NormalB $ resultOrThrow $
                                    app2Exp (VarE safeDecAsNam)
                                            (VarE xName)
                                            (VarE ctxtName)) []]

          {-
          -- TODO Encoder
          SigD encNam encType
          FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                 (quoteStr "TODO")) []]
          -}

          : [])
    whenDebugging $ do
      liftIO $ putStrLn $
        "> Generating from " ++ show decl
      liftIO $ putStrLn $ "  to " ++ indCode "     " res
    return res
xsdDeclToHaskell decl@(ElementDefn nam typ) = do
  let baseName = qFirstToUpper nam
      typBaseName = firstToUpper $ qName typ
      tryDecNam = mkName $ "tryDecode" ++ qName baseName
      decNam = mkName $ "decode" ++ qName baseName
      loadNam = mkName $ "load" ++ qName baseName
  tryDecoder <- [| $(return $ VarE $ mkName $ "tryDecodeAs" ++ typBaseName)
                      $(return $ quoteStr $ qName nam)
                        ctxt |]
  let res = (

        -- Safe decoder
        SigD tryDecNam
                (fn1Type contentConT
                         (applyExceptCon stringConT
                                         (ConT $ mkName typBaseName)))
        : FunD tryDecNam [Clause [VarP ctxtName]
                                 (NormalB tryDecoder) []]

        -- Decoder
        : SigD decNam (fn1Type contentConT
                               (ConT $ mkName typBaseName))
        : FunD decNam [Clause [VarP ctxtName]
                              (NormalB $ resultOrThrow $
                                AppE (VarE tryDecNam)
                                     (VarE ctxtName)) []]

        {-
        -- TODO Encoder
        SigD encNam encType
        FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                               (quoteStr "TODO")) []]
        -}

        -- Reader
        : SigD loadNam (fn1Type stringConT
                                (AppT ioConT
                                      (ConT $ mkName typBaseName)))
        : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ loadElementName)
                                                  (VarE decNam)) []]

        {-
        -- TODO Writer
        (SigD writeNam $ VarT aName)
        FunD writeNam [Clause [] (NormalB $ AppE errorVarE
                                                 (quoteStr "TODO")) []]
        -}

        : [])
  whenDebugging $ do
    liftIO $ putStrLn $
      "> Generating from " ++ show decl
    liftIO $ putStrLn $ "  to " ++ indCode "     " res
  return res
xsdDeclToHaskell decl@(AttributeDefn nam typ) =
  let rootName = firstToUpper $ qName nam
      rootTypeName = mkName $ rootName ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ "tryDecode" ++ rootName
  in do
    decoder <- [| pullAttrFrom $(return $ quoteStr $ qName nam) ctxt |]
    let (haskellTyp, _) = xsdTypeNameTranslation $ qName typ
    let res = (
          TySynD rootTypeName [] haskellTyp

          -- Safe decoder
          : SigD safeDecNam
                 (fn1Type contentConT
                          (applyExceptCon stringConT
                                          (AppT maybeConT
                                                (ConT rootTypeName))))
          : FunD safeDecNam [Clause [VarP ctxtName]
                                    (NormalB $ applyReturn decoder)
                                    []]

          -- Decoder
          : SigD decNam (fn1Type contentConT
                                 (AppT maybeConT (ConT rootTypeName)))
          : FunD decNam [Clause [VarP ctxtName]
                                (NormalB $ resultOrThrow $
                                  AppE (VarE safeDecNam)
                                       (VarE ctxtName)) []]

          {-
          -- TODO Encoder
          SigD encNam encType
          FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                 (quoteStr "TODO")) []]
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
      tryDecNam = mkName $ "tryDecodeAs" ++ nameRoot
  in do
    hrefOut <- mapM xsdRefToBangTypeQ refs
    let subNames = map (mkName . ("s" ++) . show) [1..length refs]
    safeDecoder <- fmap (DoE Nothing) $
      assembleTryStatements refs subNames ctxtName (ConE typNam) []
      -- assembleTryDecoder refs subNames ctxtName (ConE typNam) []
    let res = (
          -- Type declaration
          DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
                [DerivClause Nothing [eqConT, showConT]]

          -- Safe decoder
           : SigD tryDecNam
                  (fn2Type stringConT contentConT
                           (applyExceptCon stringConT
                                           (ConT $ mkName nameRoot)))
           : FunD tryDecNam [Clause [WildP, VarP ctxtName]
                                    (NormalB safeDecoder) []]

          -- Decoder
           : SigD decNam
                  (fn2Type stringConT contentConT (ConT $ mkName nameRoot))
           : FunD decNam [Clause [VarP xName, VarP ctxtName]
                                 (NormalB $ resultOrThrow $
                                   app2Exp (VarE tryDecNam)
                                           (VarE xName)
                                           (VarE ctxtName)) []]

           {-
          -- TODO Encoder
           : SigD encNam encType
           : FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                    (quoteStr "TODO")) []]
           -}

           : [])
    whenDebugging $ do
      liftIO $ putStrLn $
        "> Generating from " ++ show decl
      liftIO $ putStrLn $ "  to " ++ indCode "     " res
    return res

assembleTryStatements ::
  [Reference] -> [Name] -> Name -> Exp -> [Exp] -> XSDQ [Stmt]
assembleTryStatements [] _ _ constructor appNamesR =
  return $ [
    NoBindS $
      applyReturn $
        foldl (\x y -> AppE x y) constructor (reverse appNamesR)
    ]
assembleTryStatements (r:refs) (s:subnames) ctxt constructor namesR = do
  sub <- xsdRefToSafeHaskellExpr ctxt r $ mkName "ELEMENT_NAME"
  further <-
    assembleTryStatements refs subnames ctxt constructor (VarE s:namesR)
  return $ BindS (VarP s) sub : further
assembleTryStatements _ [] _ _ _ =
  error "This should be impossible when passing an arbitrarily long list for subnames"

-- | Translate a reference to an XSD element type to a Haskell
-- `Exp`ression representation describing the extraction of the given
-- value within an `Either` monad.
xsdRefToSafeHaskellExpr :: Name -> Reference -> Name -> XSDQ Exp
xsdRefToSafeHaskellExpr param (ElementRef ref occursMin occursMax) ctxt =
  let casePrefix = CaseE $ subcontentZom ref param
  in do
    typeName <- getElementTypeOrFail ref
    whenDebugging $ liftIO $ putStrLn $
      "Retrieving type " ++ qName typeName ++ " for " ++ showQName ref
    case (occursMin, occursMax) of
      (_, Just 0) -> return $ TupE []
      (Just 0, Just 1) -> do
        matches <- zomMatch1
          (applyReturn nothingConE)
          (\paramName -> applyReturn $
                           AppE justConE
                                (AppE (AppE (decoderAsExpFor $ qName typeName)
                                            (quoteStr $ qName ref))
                                      (VarE paramName)))
          (applyThrowStrExp $
           "QDHXB: " ++ showQName ref ++ " should not occur more than once in "
           ++ show ctxt ++ " element")
        return $ casePrefix matches
      (_, Just 1) -> do
        matches <- zomMatch1
          (applyThrowStrExp $
           "QDHXB: element " ++ showQName ref ++ " must be present in "
           ++ show ctxt ++ " element")
          (\paramName -> applyReturn $
                           AppE (AppE (decoderAsExpFor $ qName typeName)
                                      (quoteStr $ qName ref))
                                (VarE paramName))
          (applyThrowStrExp $
           "QDHXB: " ++ showQName ref ++ " should not occur more than once in "
           ++ show ctxt ++ " element")
        return $ casePrefix matches
      _ -> return $ applyReturn $
             AppE (AppE mapVarE
                        (AppE (decoderAsExpFor $ qName typeName)
                              (quoteStr $ qName ref)))
                  (AppE zomToListVarE (subcontentZom ref param))
xsdRefToSafeHaskellExpr param (AttributeRef ref usage) _ = do
  core <- xsdRefToSafeHaskellExpr' param $ qName ref
  fmap applyReturn $ unpackAttrDecoderForUsage usage core
  where xsdRefToSafeHaskellExpr' :: Name -> String -> XSDQ Exp
        xsdRefToSafeHaskellExpr' p r =
          return $ AppE (decoderExpFor r) (VarE p)

-- | Called from generated code.
__decodeForSimpleType :: String -> Content -> String -> String
{-# INLINE __decodeForSimpleType #-}
__decodeForSimpleType elementName ctxt msgIfNothing =
  case pullCRefContent elementName ctxt of
    Nothing -> error msgIfNothing
    Just v -> v

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
subcontentZom :: QName -> Name -> Exp
subcontentZom ref param =
  AppE (AppE (VarE $ mkName "pullContentFrom") (LitE (StringL $ qName ref)))
       (VarE param)

zomMatch1 :: Exp -> (Name -> Exp) -> Exp -> XSDQ [Match]
zomMatch1 zeroCase oneCaseF manyCaseExp = do
  let newX = mkName "x"
  assembleZomMatches zeroCase
                     (VarP newX) (oneCaseF newX)
                     WildP (manyCaseExp)

assembleZomMatches :: Exp -> Pat -> Exp -> Pat -> Exp -> XSDQ [Match]
assembleZomMatches zeroCase onePattern oneCase manyPattern manyCase =
  return $ [
    Match zeroPat (NormalB zeroCase) [],
    Match (onePat onePattern) (NormalB oneCase) [],
    Match (manyPat manyPattern) (NormalB manyCase) []
    ]

maybeMatches :: Exp -> (Name -> Exp) -> XSDQ [Match]
maybeMatches zeroCase oneCaseF = do
  newX <- newName "x"
  return $ [
    Match nothingPat (NormalB zeroCase) [],
    Match (justPat newX) (NormalB $ oneCaseF newX) []
    ]

-- | Translate a reference to an XSD element type to a Template Haskell
-- quotation monad returning a type.
xsdRefToBangTypeQ :: Reference -> XSDQ BangType
xsdRefToBangTypeQ (ElementRef ref lower upper) = do
  typeName <- getElementTypeOrFail ref
  typ <- containForBounds lower upper $ return $ ConT $ mkName $
         firstToUpper $ qName typeName
  return (useBang, typ)
xsdRefToBangTypeQ (AttributeRef ref usage) =
  return (useBang,
          attrTypeForUsage usage $
            ConT $ mkName $ firstToUpper $ qName ref ++ "AttrType")

attrTypeForUsage :: AttributeUsage -> Type -> Type
attrTypeForUsage Forbidden _ = TupleT 0
attrTypeForUsage Optional typ = AppT maybeConT typ
attrTypeForUsage Required typ = typ

unpackAttrDecoderForUsage :: AttributeUsage -> Exp -> XSDQ Exp
unpackAttrDecoderForUsage Forbidden _ = return $ TupE []
unpackAttrDecoderForUsage Optional expr = return expr
unpackAttrDecoderForUsage Required expr = fmap (CaseE expr) $
  maybeMatches (throwsError "QDHXB: should not return Nothing") VarE

-- | Handy abbreviation of some TH boilerplate.
useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness

indCode :: String -> [Dec] -> String
indCode ind = intercalate ("\n" ++ ind) . lines . pprint
