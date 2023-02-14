{-# LANGUAGE TemplateHaskell #-}

-- | Generate Haskell code from the flattened internal representation.
module QDHXB.Internal.Generate (
  -- * The representation types
  Reference(ElementRef, AttributeRef  {-, ComplexTypeRef -} ),
  Definition(SimpleSynonymDefn, AttributeDefn, SequenceDefn, ElementDefn),

  -- * Code generation from the internal representation
  xsdDeclsToHaskell,

  -- * Functions appearing in the generated code
  simpleTypeDecoder
  )
where

import Control.Monad.Except
import Control.Monad.Extra (whenJust)
-- import Control.Monad.IO.Class
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, Content, qName, Line)
import QDHXB.Errs
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Utils.TH
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
xsdDeclToHaskell decl@(SimpleSynonymDefn nam typ ln ifDoc) = do
  let (haskellType, basicDecoder) = xsdTypeNameTranslation $ qName typ
  let baseName = firstToUpper $ qName nam
  (typeName, decs) <- getSimpleTypeElements baseName (VarP eName)
                                            (basicDecoder $ VarE eName) ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @" ++ qName nam ++ "@ simple type"
  packAndDebug decl $ TySynD typeName [] haskellType : decs
xsdDeclToHaskell decl@(UnionDefn name pairs ln ifDoc) = do
  let baseName = qName name

      makeConstr :: (QName, QName) -> Con
      makeConstr (constructorName, tn) =
        NormalC (mkName $ qName constructorName)
                [(useBang, ConT $ mkName $ qName tn)]

      safeDecoder :: Exp
      safeDecoder = foldr (\(c, t) e -> applyCatchErrorExp
                              (app2Exp
                                 fmapVarE
                                 (ConE $ mkName $ qName c)
                                 (AppE (VarE $ mkName $
                                            "tryStringDecodeFor" ++ qName t)
                                       (VarE xName)))
                              (LamE [WildP] e))
                          (qthNoValidContentInUnion baseName ln)
                          pairs
  (typeName, decs) <- getSimpleTypeElements baseName (VarP xName)
                                            safeDecoder ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @\\<" ++ baseName ++ ">@ union"
  packAndDebug decl $
    DataD [] typeName [] Nothing (map makeConstr pairs)
          [DerivClause Nothing [eqConT, showConT]]
     : decs
xsdDeclToHaskell decl@(ListDefn name elemTypeRef ln ifDoc) = do
  let xmlName = qName name
      baseStr = firstToUpper xmlName
      elemStr = firstToUpper $ qName elemTypeRef
      elemName = mkName elemStr
      elementDecodeName = mkName $ "tryStringDecodeFor" ++ elemStr
      decodeAs = applyMapM (VarE elementDecodeName) (spaceSepApp (VarE eName))
  (typeName, decs) <-
    getSimpleTypeElements baseStr (VarP eName) decodeAs ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Type associated with the @\\<" ++ xmlName ++ ">@ list type"
  packAndDebug decl $ TySynD typeName [] (AppT ListT $ ConT elemName) : decs

xsdDeclToHaskell decl@(ElementDefn nam typ _ln ifDoc) = do
  let origName = qName nam
      baseName = firstToUpper $ origName
      typBaseName = firstToUpper $ qName typ
      tryDecNam = mkName $ "tryDecode" ++ baseName
      decNam = mkName $ "decode" ++ baseName
      loadNam = mkName $ "load" ++ baseName
  tryDecoder <- [| $(return $ VarE $ mkName $ "tryDecodeAs" ++ typBaseName)
                      $(return $ quoteStr $ qName nam)
                        ctxt |]
  let res = (

        -- Safe decoder
        SigD tryDecNam
                (fn1Type contentConT (qHXBExcT (ConT $ mkName typBaseName)))
        : FunD tryDecNam [Clause [ctxtVarP]
                                 (NormalB tryDecoder) []]

        -- Decoder
        : SigD decNam (fn1Type contentConT
                               (ConT $ mkName typBaseName))
        : FunD decNam [Clause [ctxtVarP]
                              (NormalB $ resultOrThrow $
                                AppE (VarE tryDecNam)
                                     ctxtVarE) []]

        -- Reader
        : SigD loadNam (fn1Type stringConT
                                (AppT ioConT
                                      (ConT $ mkName typBaseName)))
        : FunD loadNam [Clause [] (NormalB $ AppE (VarE $ loadElementName)
                                                  (VarE decNam)) []]

        {-
        -- Encoder
        SigD encNam encType
        FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                               (quoteStr "TODO")) []]
        -- Writer
        (SigD writeNam $ VarT aName)
        FunD writeNam [Clause [] (NormalB $ AppE errorVarE
                                                 (quoteStr "TODO")) []]
        -}

        : [])

  pushDeclHaddock ifDoc decNam
    ("Load a @\\<" ++ origName ++ ">@ element from the given file")
  pushDeclHaddock ifDoc tryDecNam
    ("Attempt to decode a @\\<" ++ origName ++ ">@ element as a `"
      ++ typBaseName
      ++ "` value from parsed XML content, throwing a `QDHXB.Errs.HXBErr` in "
      ++ "the `Control.Monad.Except` monad if loading or parsing fails")
  pushDeclHaddock ifDoc loadNam
    ("Load a @\\<" ++ origName ++ ">@ element from the given file")

  whenDebugging $ liftIO $ do
    bLabelPrintln "> Generating from " decl
    bLabelPrintln "  to " res

  return res

xsdDeclToHaskell d@(AttributeDefn nam (AttributeGroupDefn ads) _ln ifDoc) = do
  let xmlName = qName nam
      rootName = firstToUpper xmlName
      baseStr = rootName ++ "AttrType"
      rootTypeName = mkName baseStr
      bangTypes = [ (useBang, AppT maybeConT $ ConT $ mkName $ firstToUpper $
                                qName q ++ "AttrType")
                  | q <- ads ]
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ "tryDecode" ++ rootName
      localNames = take (length ads) $
        map (\z -> mkName $ "s" ++ show z) ([1..] :: [Int])
      pairEnc (q, s) =
        let dec = mkName ("tryDecode" ++ firstToUpper (qName q))
        in BindS (VarP s) (AppE (VarE dec) ctxtVarE)
      bpairs = map pairEnc $ zip ads localNames
      decoder = DoE Nothing $ bpairs ++ [NoBindS $ applyReturn $ applyJust $
                                         foldl (\e n -> AppE e $ VarE n)
                                          (ConE rootTypeName) localNames]
  let res = [
        DataD [] rootTypeName [] Nothing [NormalC rootTypeName bangTypes]
          [DerivClause Nothing [eqConT, showConT]],

        -- Functions
        SigD safeDecNam (fn1Type contentConT
                          (qHXBExcT (AppT maybeConT (ConT rootTypeName)))),
        FunD safeDecNam [Clause [ctxtVarP]
                                    (NormalB decoder)
                                    []],

        -- Decoder
        SigD decNam (fn1Type contentConT
                    (AppT maybeConT (ConT rootTypeName))),
        FunD decNam [Clause [ctxtVarP]
                       (NormalB $ resultOrThrow $
                          AppE (VarE safeDecNam) ctxtVarE) []]
        ]

  pushDeclHaddock ifDoc safeDecNam $
    "Attempt to decode the attributes defined in the @\\<" ++ xmlName
    ++ ">@ attribute group as a `" ++ baseStr
    ++ "`, throwing a `QDHXB.Errs.HXBErr` in the `Control.Monad.Except`"
    ++ " monad if extraction fails"
  pushDeclHaddock ifDoc decNam $
    "Decode the attributes defined in the @\\<" ++ xmlName
    ++ ">@ attribute group as a `" ++ baseStr ++ "`"
  pushDeclHaddock ifDoc rootTypeName $
    "Representation of the attributes defined in the @\\<" ++ xmlName
    ++ ">@ attribute group"

  whenDebugging $ liftIO $ do
    putStrLn "> xsdDeclToHaskell AttributeGroupDefn"
    bLabelPrintln "  > Generating from " d
    bLabelPrintln "    +--> " res
  return res

xsdDeclToHaskell decl@(AttributeDefn nam (SingleAttributeDefn typ _) _l ifDoc) =
  let xmlName = qName nam
      rootName = firstToUpper xmlName
      rootTypeName = mkName $ rootName ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ "tryDecode" ++ rootName
  in do
    puller <- [| pullAttrFrom $(return $ quoteStr $ qName nam) ctxt |]
    let (haskellTyp, basicDecoder) = xsdTypeNameTranslation $ qName typ
    let decoder = DoE Nothing [
          LetS [ValD (VarP yName) (NormalB puller) []],
          NoBindS $
            caseNothingJust' (VarE yName)
              (applyReturn nothingConE)
              xName (DoE Nothing [
                        BindS (VarP resName) $ basicDecoder $ VarE xName,
                        NoBindS $ applyReturn $ applyJust (VarE resName)
                        ])
          ]
    let res = (
          TySynD rootTypeName [] haskellTyp

          -- Safe decoder
          : SigD safeDecNam
                 (fn1Type contentConT
                          (qHXBExcT (AppT maybeConT (ConT rootTypeName))))
          : FunD safeDecNam [Clause [ctxtVarP] (NormalB decoder) []]

          -- Decoder
          : SigD decNam (fn1Type contentConT
                                 (AppT maybeConT (ConT rootTypeName)))
          : FunD decNam [Clause [ctxtVarP]
                                (NormalB $ resultOrThrow $
                                  AppE (VarE safeDecNam)
                                       ctxtVarE) []]

          {-
          -- TODO Encoder
          SigD encNam encType
          FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                 (quoteStr "TODO")) []]
          -}
          : [])

    pushDeclHaddock ifDoc safeDecNam $
      "Attempt to decode the @\\<" ++ xmlName
      ++ ">@ attribute as a `" ++ show rootTypeName
      ++ "`, throwing a `QDHXB.Errs.HXBErr` in the `Control.Monad.Except`"
      ++ " monad if extraction fails"
    pushDeclHaddock ifDoc decNam $
      "Decode the @\\<" ++ xmlName ++ ">@ attribute as a `"
      ++ show rootTypeName ++ "`"
    pushDeclHaddock ifDoc rootTypeName $
      "Representation of the @\\<" ++ xmlName ++ ">@ attribute"

    whenDebugging $ liftIO $ do
      bLabelPrintln "> Generating from " decl
      bLabelPrintln "  to " res
    return res

xsdDeclToHaskell decl@(SequenceDefn namStr refs _ln ifDoc) =
  let nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      decNam = mkName $ "decodeAs" ++ nameRoot
      tryDecNam = mkName $ "tryDecodeAs" ++ nameRoot
  in do
    hrefOut <- mapM xsdRefToBangTypeQ refs
    let subNames = map (mkName . ("s" ++) . show) [1..length refs]
    safeDecoder <- fmap (DoE Nothing) $
      assembleTryStatements refs subNames ctxtName (ConE typNam) []

    let res = (
          -- Type declaration
          DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
                [DerivClause Nothing [eqConT, showConT]]

          -- Safe decoder
           : SigD tryDecNam
                  (fn2Type stringConT contentConT
                           (qHXBExcT (ConT $ mkName nameRoot)))
           : FunD tryDecNam [Clause [WildP, ctxtVarP]
                                    (NormalB safeDecoder) []]

          -- Decoder
           : SigD decNam
                  (fn2Type stringConT contentConT (ConT $ mkName nameRoot))
           : FunD decNam [Clause [VarP xName, ctxtVarP]
                                 (NormalB $ resultOrThrow $
                                   app2Exp (VarE tryDecNam)
                                           (VarE xName)
                                           ctxtVarE) []]

           {-
          -- TODO Encoder
           : SigD encNam encType
           : FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                    (quoteStr "TODO")) []]
           -}

           : [])

    pushDeclHaddock ifDoc tryDecNam $
      "Attempt to decode the @\\<" ++ namStr
         ++ ">@ attribute as a `" ++ nameRoot
         ++ "`, throwing a `QDHXB.Errs.HXBErr` in the `Control.Monad.Except` monad if extraction fails"
    pushDeclHaddock ifDoc decNam $
      "Decode the @\\<" ++ namStr ++ ">@ attribute as a `" ++ nameRoot ++ "`"
    pushDeclHaddock ifDoc typNam $
      "Representation of the @\\<" ++ namStr ++ ">@ attribute"

    whenDebugging $ liftIO $ do
      bLabelPrintln "> Generating from " decl
      bLabelPrintln "  to " res

    return res

packAndDebug :: Blockable a => Definition -> a -> XSDQ a
packAndDebug d r = do
  whenDebugging $ liftIO $ do
    bLabelPrintln "> Generating from " d
    bLabelPrintln "  to " r
  return r

getSimpleTypeElements ::
  String -> Pat -> Exp -> Maybe Line -> Maybe String
  -> XSDQ (Name, {- Name, Name, Type, Type, -} [Dec])
getSimpleTypeElements baseNameStr pat1 body l ifDoc = do
  let typeName = mkName baseNameStr
      typeType = ConT typeName
      decStrName = mkName $ "tryStringDecodeFor" ++ baseNameStr
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      decStrType = fn1Type stringConT (qHXBExcT typeType)
      tryDecType = fn2Type stringConT contentConT  (qHXBExcT typeType)
      decType = fn2Type stringConT contentConT typeType

  pushDeclHaddock ifDoc decStrName $
    "Attempt to decode a string representing a value of the XSD simple type as a `"
    ++ baseNameStr ++ "` value, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc safeDecAsNam $
    "Attempt to decode an element of simple type represented as `"
    ++ baseNameStr ++ "`, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc decAsNam $
    "Decode an element of simple type represented as `" ++ baseNameStr
    ++ "`, or fail with a top-level `error`"

  return (typeName,
          [SigD decStrName decStrType,
           FunD decStrName [Clause [pat1] (NormalB body) []],

           SigD safeDecAsNam tryDecType,
           FunD safeDecAsNam [Clause [VarP xName, VarP ctxtName]
                               (NormalB $
                                app4Exp simpleTypeDecoderVarE
                                  (VarE xName) (VarE ctxtName)
                                  (qCrefMustBePresentIn baseNameStr l)
                                  (VarE decStrName)) []],

           SigD decAsNam decType,
           FunD decAsNam [Clause [VarP xName, ctxtVarP]
                           (NormalB $ resultOrThrow $
                             app2Exp (VarE safeDecAsNam) (VarE xName)
                                     ctxtVarE)
                           []]
           {-
           -- TODO Encoder
           SigD encNam encType
           FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                  (quoteStr "TODO")) []]
           -}
          ])

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
xsdRefToSafeHaskellExpr param (ElementRef ref occursMin occursMax ln) _ctxt =
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
          (qthAtMostOnceIn (showQName ref) ln)
        return $ casePrefix matches
      (_, Just 1) -> do
        matches <- zomMatch1
          (qthMustBePresentIn (showQName ref) ln)
          (\paramName -> applyReturn $
                           AppE (AppE (decoderAsExpFor $ qName typeName)
                                      (quoteStr $ qName ref))
                                (VarE paramName))
          (qthAtMostOnceIn (showQName ref) ln)
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
simpleTypeDecoder ::
  String -> Content -> HXBErr -> (String -> HXBExcept a) -> HXBExcept a
{-# INLINE simpleTypeDecoder #-}
simpleTypeDecoder elementName contentNode miscFailMsg stringDecoder = do
  case pullCRefContent elementName contentNode of
    Nothing -> throwError miscFailMsg
    Just v -> stringDecoder v

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
decoderExpFor :: String -> Exp
decoderExpFor ref = VarE $ mkName $ "decode" ++ firstToUpper ref

-- | From a type name, construct the associated Haskell element
-- decoder function `Exp`ression.
decoderAsExpFor :: String -> Exp
decoderAsExpFor typ = VarE $ mkName $ "decodeAs" ++ firstToUpper typ

-- elementDefnToTypeDecoderExp :: Reference -> Exp
-- elementDefnToTypeDecoderExp (ElementDefn _ t _ln _ifDoc) =

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.
subcontentZom :: QName -> Name -> Exp
subcontentZom ref param =
  AppE (AppE (VarE $ mkName "QDHXB.Expansions.pullContentFrom") (LitE (StringL $ qName ref)))
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
xsdRefToBangTypeQ (ElementRef ref lower upper _ln) = do
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

pushDeclHaddock :: Maybe String -> Name -> String -> XSDQ ()
pushDeclHaddock ifDoc = do
  pushDeclHaddock' $ maybe "." (": " ++) ifDoc

pushDeclHaddock' :: String -> Name -> String -> XSDQ ()
pushDeclHaddock' suffix name spec = do
  liftQtoXSDQ $ addModFinalizer $ putDoc (DeclDoc name) $ spec ++ suffix

