{-# LANGUAGE TemplateHaskell #-}

{-| Generate Haskell code from the flattened internal representation.
Each XSD type, attribute, and element definition is translated into a
Haskell type declaration plus several functions.

A __complex type__ named /Typ/ is accompanied with these functions:

 - @tryDecodeAs@/Typ/, which takes a `String` and a piece of XML
   `QDHXB.Expansions.Content`, and returns a computation in
   `QDHXB.Expansions.HXBExcept` returning a value of the underlying
   implementation type.
   This function makes calls to `` and `` to find and extract
   subcomponent values from the given parsed XML, and assembles
   them into a value of the implementation type.

 - @decodeAs@/Typ/, which takes a `String` and a piece of XML
   `QDHXB.Expansions.Content`, and returns a value of the
   underlying implementation type, throwing an exception if the
   conversion is not possible.  This function just uses
   `Control.Monad.runExcept` to access the result (or absence of a
   result) of a call to @tryDecodeAs@/Typ/ with the same argument.

A __simple type__ named /Typ/ is accompanied with the same two functions
as a complex type, plus:

 - @tryStringDecodeFor@/Typ/, which takes a `String` and returns
   a computation in `QDHXB.Expansions.HXBExcept` returning a value
   of the underlying implementation type.

For simple types, the @tryDecodeAs@/Typ/ then just attempts to decode
the text between element tags using @tryStringDecodeFor@/Typ/, adding
attribute values when needed.

An __attribute__ named /Attr/ is accompanied with these functions:

 - @tryDecodeAs@/Attr/, which takes a piece of XML
   `QDHXB.Expansions.Content`, and returns a computation in
   `QDHXB.Expansions.HXBExcept` returning a `Maybe`-wrapped value
   of the underlying implementation type.

 - @decodeAs@/Attr/, which takes a piece of XML
   `QDHXB.Expansions.Content`, and returns a `Maybe`-wrapped value
   of the underlying implementation type.

Each __element__ with delimiters @<elem>@ is accompanied with these
functions, where /Elem/ is the same as @elem@ except capitalizing the
first letter if it is not already capitalized:

 - @tryDecodeAs@/Elem/ and @decodeAs@/Elem/, as for the functions
   associated with XML types, and implemented in terms of these
   functions on the type associated with the element.

 - @load@/Elem/, for retrieving XML-encoded values stored in a file.
   The result is not now wrapped in an `Except`, but should be.

-}

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
-- import Control.Monad.Extra (whenJust)
-- import Control.Monad.IO.Class
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, Content, qName, Line)
import QDHXB.Errs
import QDHXB.Internal.Utils.TH
import QDHXB.Internal.Utils.XMLLight
import QDHXB.Internal.Utils.BPP (bpp)
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ


-- | Translate a list of XSD definitions to top-level Haskell
-- declarations in the Template Haskell quotation monad.
xsdDeclsToHaskell :: [Definition] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  whenDebugging $ dbgLn "Generating Haskell declarations from definitions"
  dbgResultM "Declarations:" $
    fmap concat $ indenting $ mapM xsdDeclToHaskell defns


-- | Translate one XSD definition to a list of top-level Haskell
-- declarations in the Template Haskell quotation monad, and record
-- the associated Haddock documentation.
xsdDeclToHaskell :: Definition -> XSDQ [Dec]

xsdDeclToHaskell decl@(SimpleSynonymDefn nam typ ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (a) " decl
  -- let (haskellType, basicDecoder) = xsdNameToTypeTranslation $ qName typ
  -- let decoder = basicDecoder $ VarE eName
  haskellType <- getTypeHaskellType typ
  decoder <- getSafeDecoder typ
  {-
  let baseName = firstToUpper $ qName nam
  (typeName, decs) <- indenting $
    getSimpleTypeElements baseName (VarP eName) (decoder $ VarE eName) ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @" ++ qName nam ++ "@ simple type"
  dbgResult "Generated" $ TySynD typeName [] haskellType : decs
  -}
  dbgResultM "Generated" $
    newAssemble nam (\tn -> TySynD tn [] haskellType) decoder ifDoc

xsdDeclToHaskell decl@(ComplexSynonymDefn nam typ ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (b) " decl
  -- let (haskellType, basicDecoder) = xsdNameToTypeTranslation $ qName typ
  -- let decoder = basicDecoder $ VarE eName
  haskellType <- getTypeHaskellType typ
  decoder <- getSafeStringDecoder typ
  let baseName = firstToUpper $ qName nam
  (typeName, decs) <- indenting $
    getComplexTypeElements baseName (decoder $ VarE eName) ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @" ++ qName nam ++ "@ simple type"
  dbgResult "Generated" $ TySynD typeName [] haskellType : decs

xsdDeclToHaskell decl@(UnionDefn name pairs ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (c) UnionDefn" decl
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
  (typeName, decs) <- indenting $
    getSimpleTypeElements baseName (VarP xName) safeDecoder ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @\\<" ++ baseName ++ ">@ union"
  dbgResult "Generated" $
      DataD [] typeName [] Nothing (map makeConstr pairs)
            [DerivClause Nothing [eqConT, showConT]]
      : decs


xsdDeclToHaskell decl@(ListDefn name elemTypeRef ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (d) " decl
  let xmlName = qName name
      baseStr = firstToUpper xmlName
      elemStr = firstToUpper $ qName elemTypeRef
      elemName = mkName elemStr
      elementDecodeName = mkName $ "tryStringDecodeFor" ++ elemStr
      decodeAs = applyMapM (VarE elementDecodeName) (spaceSepApp (VarE eName))
  (typeName, decs) <- indenting $
    getSimpleTypeElements baseStr (VarP eName) decodeAs ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Type associated with the @\\<" ++ xmlName ++ ">@ list type"
  dbgResult "Generated" $
    TySynD typeName [] (AppT ListT $ ConT elemName) : decs


xsdDeclToHaskell decl@(ElementDefn nam typ _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (e) " decl
  -- let (haskellType, _) = xsdNameToNameTranslation $ qName typ
  let origName = qName nam
      baseName = firstToUpper $ origName
      typBaseName = firstToUpper $ qName typ
      tryDecNam = mkName $ "tryDecode" ++ baseName
      decNam = mkName $ "decode" ++ baseName
      loadNam = mkName $ "load" ++ baseName
  decodedType <- getTypeHaskellType typ
  tryDecoder <- getSafeDecoder typ
  whenDebugging $ dbgBLabel "- tryDecoder " $ tryDecoder ctxtVarE
  {-
  tryDecoder <- [| $(return $ VarE $ mkName $ "tryDecodeAs" ++ typBaseName)
                      $(return $ quoteStr $ qName nam)
                        ctxt |]
    -}

  let res = (

        -- Safe decoder
        SigD tryDecNam
                (fn1Type contentConT (qHXBExcT decodedType))
        : FunD tryDecNam [Clause [ctxtVarP]
                                 (NormalB $ tryDecoder ctxtVarE) []]

        -- Decoder
        : SigD decNam (fn1Type contentConT decodedType)
        : FunD decNam [Clause [ctxtVarP]
                              (NormalB $ resultOrThrow $
                                AppE (VarE tryDecNam)
                                     ctxtVarE) []]

        -- Reader
        : SigD loadNam (fn1Type stringConT
                                (AppT ioConT decodedType))
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

  dbgResult "Generated" res


xsdDeclToHaskell d@(AttributeDefn nam (AttributeGroupDefn ads) _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (f) " d
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
      pairEnc (q, s) = do
        dec <- getSafeStringDecoder q
        return $ BindS (VarP s) $ dec ctxtVarE
        {-
        let dec = mkName ("tryDecode" ++ firstToUpper (qName q))
        in BindS (VarP s) (AppE (VarE dec) ctxtVarE)
        -}
  bpairs <- mapM pairEnc $ zip ads localNames
  let decoder = DoE Nothing $ bpairs ++ [NoBindS $ applyReturn $ applyJust $
                                         foldl (\e n -> AppE e $ VarE n)
                                          (ConE rootTypeName) localNames]

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

  dbgResult "Generated" $ [
    DataD [] rootTypeName [] Nothing [NormalC rootTypeName bangTypes]
      [DerivClause Nothing [eqConT, showConT]],

    -- Functions
    SigD safeDecNam (fn1Type contentConT
                      (qHXBExcT (AppT maybeConT (ConT rootTypeName)))),
    FunD safeDecNam [Clause [ctxtVarP]
                                (NormalB decoder)
                                []],

    -- Decoder
    SigD decNam (fn1Type contentConT (AppT maybeConT (ConT rootTypeName))),
    FunD decNam [Clause [ctxtVarP]
                   (NormalB $ resultOrThrow $
                      AppE (VarE safeDecNam) ctxtVarE) []]
    ]


xsdDeclToHaskell d@(AttributeDefn nam (SingleAttributeDefn typ _) _l ifd) = do
  whenDebugging $ dbgBLabel "Generating from (g) " d
  let xmlName = qName nam
      rootName = firstToUpper xmlName
      rootTypeName = mkName $ rootName ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ "tryDecode" ++ rootName

  puller <- [| pullAttrFrom $(return $ quoteStr $ qName nam) ctxt |]
  -- let (haskellTyp, basicDecoder) = xsdNameToTypeTranslation $ qName typ
  -- let coreDecoder = basicDecoder $ VarE xName
  haskellTyp <- getTypeHaskellType typ
  coreDecoder <- getSafeStringDecoder typ
  let decoder = DoE Nothing [
        LetS [ValD (VarP yName) (NormalB puller) []],
        NoBindS $
          caseNothingJust' (VarE yName)
            (applyReturn nothingConE)
            xName (DoE Nothing [
                      BindS (VarP resName) (coreDecoder $ VarE xName),
                      NoBindS $ applyReturn $ applyJust (VarE resName)
                      ])
        ]

  pushDeclHaddock ifd safeDecNam $
    "Attempt to decode the @\\<" ++ xmlName
    ++ ">@ attribute as a `" ++ show rootTypeName
    ++ "`, throwing a `QDHXB.Errs.HXBErr` in the `Control.Monad.Except`"
    ++ " monad if extraction fails"
  pushDeclHaddock ifd decNam $
    "Decode the @\\<" ++ xmlName ++ ">@ attribute as a `"
    ++ show rootTypeName ++ "`"
  pushDeclHaddock ifd rootTypeName $
    "Representation of the @\\<" ++ xmlName ++ ">@ attribute"

  dbgResult "Generated" $ (
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


xsdDeclToHaskell decl@(SequenceDefn nam refs ln ifDoc) =
  let namStr = qName nam
      nameRoot = firstToUpper namStr
      typNam = mkName nameRoot
      -- decNam = mkName $ "decodeAs" ++ nameRoot
      -- tryDecNam = mkName $ "tryDecodeAs" ++ nameRoot
  in do
    whenDebugging $ dbgBLabel "Generating from (h) " decl
    hrefOut <- mapM xsdRefToBangTypeQ refs
    whenDebugging $ indenting $ do
      dbgLn $ "REFS " ++ (foldr (++) " " $ map show refs)
      dbgLn $ "HREFOUT " ++ (foldr (++) " " $ map show hrefOut)
    let subNames = map (mkName . ("s" ++) . (show :: Int -> String)) [1..]
    safeDecoder <- fmap (DoE Nothing) $ indenting $
      assembleTryStatements refs subNames ctxtName (ConE typNam) []

    (typeName, decs) <-
      getComplexTypeElements (firstToUpper namStr) safeDecoder ln ifDoc

    pushDeclHaddock ifDoc typeName $
      "Representation of the @\\<" ++ namStr ++ ">@ attribute"

    -- Type declaration
    let typeDef = DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
                    [DerivClause Nothing [eqConT, showConT]]

    dbgResult "Generated" $ typeDef : decs


xsdDeclToHaskell decl@(ExtensionDefn qn base refs _ _) = do
  whenDebugging $ do
    dbgBLabel "Generating from (i) " decl
    dbgBLabel "DECL " decl
  let nameRoot = firstToUpper $ qName qn
      typNam = mkName nameRoot
      decNam = mkName $ "decodeAs" ++ nameRoot
      tryDecNam = mkName $ "tryDecodeAs" ++ nameRoot
      subNames = map (mkName . ("s" ++) . (show :: Int -> String)) [1..]
  safeDecoder <- fmap (DoE Nothing) $ indenting $
    assembleTryStatements (base:refs) subNames ctxtName (ConE typNam) []
  hrefOut <- mapM xsdRefToBangTypeQ $ base : refs

  dbgResult "Generated" $ [
    -- Type declaration
    DataD [] typNam [] Nothing [NormalC typNam $ hrefOut]
          [DerivClause Nothing [eqConT, showConT]],

    -- Safe decoder
    SigD tryDecNam (fn1Type contentConT
                            (qHXBExcT (ConT $ mkName nameRoot))),
    FunD tryDecNam [Clause [ctxtVarP]
                              (NormalB safeDecoder) []],

    -- Decoder
    SigD decNam (fn1Type contentConT (ConT $ mkName nameRoot)),
    FunD decNam [Clause [ctxtVarP]
                           (NormalB $ resultOrThrow $
                             AppE (VarE tryDecNam) ctxtVarE) []]

     {-
    -- TODO Encoder
     : SigD encNam encType
     : FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                              (quoteStr "TODO")) []]
     -}

     ]


xsdDeclToHaskell decl@(GroupDefn qn (TypeRef tqn _ _ _ _) _ifLn _ifDoc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (j) " decl
  let groupRoot = firstToUpper $ qName qn
      groupName = mkName groupRoot
      tryDecodeGroup = mkName $ "tryDecodeAs" ++ groupRoot
      decodeGroup = mkName $ "decodeAs" ++ groupRoot
  let typeRoot = firstToUpper $ qName tqn
      typeName = mkName typeRoot
      tryDecodeType = mkName $ "tryDecodeAs" ++ typeRoot
      decodeType = mkName $ "decodeAs" ++ typeRoot

  decoderBody <- getSafeStringDecoder tqn

  dbgResult "Generated" [
    -- Type declaration
    TySynD groupName [] (ConT typeName),

    -- Safe decoder
    SigD tryDecodeGroup (fn1Type contentConT
                                 (qHXBExcT (ConT groupName))),
    FunD tryDecodeGroup
      [Clause [ctxtVarP] (NormalB $ decoderBody ctxtVarE) []],

    -- Decoder
    SigD decodeGroup (fn1Type contentConT (ConT groupName)),
    FunD decodeGroup [
        Clause [ctxtVarP] (NormalB $ resultOrThrow $
                            AppE (VarE tryDecodeGroup) ctxtVarE) []
        ]

     {-
    -- TODO Encoder
     : SigD encNam encType
     : FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                              (quoteStr "TODO")) []]
     -}

    ]
  -- error "xsdDeclToHaskell > GroupDefn case"


xsdDeclToHaskell decl@(ChoiceDefn name fields ifLine _) = do
  whenDebugging $ do
    dbgBLabel "Generating from (k) " decl
  let choiceRoot = firstToUpper $ qName name
      choiceName = mkName choiceRoot
      tryDecodeChoice = mkName $ "tryDecodeAs" ++ choiceRoot
      decodeChoice = mkName $ "decodeAs" ++ choiceRoot
  (constrDefs, constrs, tryFns) <-
    fmap unzip3 $ mapM (makeChoiceConstructor name) fields
  let typeDef =
        DataD [] (mkName $ firstToUpper $ qName name) [] Nothing constrDefs []
      tryBody = makeTryDecoder (VarE xName) ctxtVarE constrs tryFns
  dbgResult "Generated" [
    typeDef,
    SigD tryDecodeChoice (fn1Type contentConT
                                  (qHXBExcT (ConT choiceName))),
    FunD tryDecodeChoice [Clause [ctxtVarP] (NormalB tryBody) []],

    -- Decoder
    SigD decodeChoice (fn1Type contentConT (ConT choiceName)),
    FunD decodeChoice [Clause [ctxtVarP]
                           (NormalB $ resultOrThrow $
                             AppE (VarE tryDecodeChoice)
                                     ctxtVarE) []]
    {- FunD decodeChoice [Clause [] (NormalB $ AppE errorVarE
                                              (quoteStr "TODO")) []] -}
    ]

xsdDeclToHaskell decl = do
  boxed $ do
    dbgLn "Uncaught case in xsdDeclToHaskell"
    dbgBLabel "DECL " decl
  error "Uncaught case in xsdDeclToHaskell"

getSafeStringDecoder :: QName -> XSDQ (Exp -> Exp)
getSafeStringDecoder qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> case defn of
      BuiltinDefn _ _ _ efn -> return $ efn
      ElementDefn _ ty _ _ -> getSafeStringDecoder ty
      AttributeDefn _ (SingleAttributeDefn ty _) _ _ ->
        getSafeStringDecoder ty
      SimpleSynonymDefn _ ty _ _ -> getSafeStringDecoder ty
      ComplexSynonymDefn _ _ _ _ ->
        return $ \_ -> throwsError "No string decoder for complex synonym type"
      SequenceDefn _ _ _ _ ->
        return $ \_ -> throwsError "No string decoder for complex sequence"
      UnionDefn _ _ _ _ ->
        return $ \_ -> throwsError "No string decoder for union"
      ChoiceDefn _ _ _ _ ->
        return $ \_ -> throwsError "No string decoder for complex choice"
      GroupDefn _ _ _ _ ->
        return $ \_ -> throwsError "No string decoder for complex group"
      ListDefn _ _ _ _ ->
        return $ \_ -> throwsError
          "TODO Write the generated string decoder for list of simple types"
      _ -> return $ \_ -> throwsError "TODO"

getSafeDecoder :: QName -> XSDQ (Exp -> Exp)
getSafeDecoder qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> case defn of
      BuiltinDefn ty _ _ _ -> forSimpleType ty
      ElementDefn _ ty _ _ -> getSafeDecoder ty
      AttributeDefn _ (SingleAttributeDefn ty _) _ _ -> do
        error "TODO"
      SimpleSynonymDefn _ ty _ _ -> forSimpleType ty
      ComplexSynonymDefn _ _ _ _ -> do
        error "TODO"
      SequenceDefn _ _ _ _ -> do
        error "TODO"
      UnionDefn _ _ _ _ -> do
        error "TODO"
      ChoiceDefn _ _ _ _ -> do
        error "TODO"
      GroupDefn _ _ _ _ -> do
        error "TODO"
      ListDefn _ _ _ _ -> do
        error "TODO"
      _ -> do
        error "TODO"

  where forSimpleType ty = do
          strDec <- getSafeStringDecoder ty
          return $ forSimpleTypeWith strDec

        forSimpleTypeWith f =
          \ctnt -> app3Exp simpleTypeDecoderVarE
                           ctnt
                           (qCrefMustBePresentIn (qName qn) Nothing)
                           (qLambdaCtntArg f)


makeTryDecoder :: Exp -> Exp -> [Exp] -> [Exp] -> Exp
makeTryDecoder x ctxt (c:constr) (t:tryFns) =
  makeTryDecoder' x ctxt (app2Exp fmapVarE c (app2Exp t x ctxt)) constr tryFns
  where makeTryDecoder' _ _ exp [] _ = exp
        makeTryDecoder' x ctxt exp (c:constr) (t:tryFns) =
          makeTryDecoder' x ctxt
            (exp `replaceOnError` (app2Exp fmapVarE c (app2Exp t x ctxt)))
            constr tryFns

makeChoiceConstructor :: QName -> (QName, Reference) -> XSDQ (Con, Exp, Exp)
makeChoiceConstructor name (typeName, ref) = do
  let typeRoot = firstToUpper $ qName name
  case ref of

    ElementRef elName ifMin ifMax _ -> do
      let constrBase = typeRoot ++ (firstToUpper $ qName elName)
          constrName = mkName constrBase
          -- (decName, _) = xsdNameToNameTranslation $ qName typeName
      decName <- getTypeHaskellName typeName
      useType <- containForBounds ifMin ifMax $ return $ ConT $ mkName decName
      return (NormalC constrName [(useBang, useType)],
              ConE constrName,
              VarE (mkName $ "tryDecodeAs" ++ decName))


    TypeRef tyName ifMin ifMax _ _ -> do
      boxed $ do
        dbgLn "TODO makeChoiceConstructor"
        dbgBLabel "NAME " name
        dbgBLabel "TYPENAME " typeName
        dbgBLabel "REF " ref
      error "TODO makeChoiceConstructor (b)"

    AttributeRef _ _ ->
      error "Not expected: makeChoiceConstructor for AttributeRef"


newAssemble :: QName -> (Name -> Dec) -> (Exp -> Exp) -> Maybe String -> XSDQ [Dec]
newAssemble base tyDec safeDec ifDoc = do
  let baseNameStr = firstToUpper $ qName base
      typeName = mkName baseNameStr
      typeType = ConT typeName
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      tryDecType = fn1Type contentConT (qHXBExcT typeType)
      decType = fn1Type contentConT typeType

  pushDeclHaddock ifDoc typeName $
    "Representation of the @" ++ baseNameStr ++ "@ type"
  pushDeclHaddock ifDoc safeDecAsNam $
    "Attempt to decode an element represented as `"
    ++ baseNameStr ++ "`, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc decAsNam $
    "Decode an element of simple type represented as `" ++ baseNameStr
    ++ "`, or fail with a top-level `error`"

  return [tyDec typeName,
          SigD safeDecAsNam tryDecType,
          FunD safeDecAsNam [Clause [VarP ctxtName]
                               (NormalB $ safeDec ctxtVarE) []],
          SigD decAsNam decType,
          FunD decAsNam [Clause [ctxtVarP]
                           (NormalB $ resultOrThrow $
                              AppE (VarE safeDecAsNam) ctxtVarE)
                         []]
         ]


-- | Generates the common boilerplate for Haskell code generated from
-- XSD simple types.
getSimpleTypeElements ::
  String -> Pat -> Exp -> Maybe Line -> Maybe String
  -> XSDQ (Name, [Dec])
getSimpleTypeElements baseNameStr pat1 body l ifDoc = do
  let typeName = mkName baseNameStr
      typeType = ConT typeName
      decStrName = mkName $ "tryStringDecodeFor" ++ baseNameStr
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      decStrType = fn1Type stringConT (qHXBExcT typeType)
      tryDecType = fn1Type contentConT (qHXBExcT typeType)
      decType = fn1Type contentConT typeType

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
           FunD safeDecAsNam [Clause [VarP ctxtName]
                               (NormalB $
                                app3Exp simpleTypeDecoderVarE
                                  (VarE ctxtName)
                                  (qCrefMustBePresentIn baseNameStr l)
                                  (VarE decStrName)) []],

           SigD decAsNam decType,
           FunD decAsNam [Clause [ctxtVarP]
                           (NormalB $ resultOrThrow $
                             AppE (VarE safeDecAsNam) ctxtVarE)
                           []]
           {-
           -- TODO Encoder
           SigD encNam encType
           FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                                  (quoteStr "TODO")) []]
           -}
          ])


getComplexTypeElements ::
  String -> Exp -> Maybe Line -> Maybe String
  -> XSDQ (Name, [Dec])
getComplexTypeElements baseNameStr safeDecoder _ ifDoc = do
  let typeName = mkName baseNameStr
      typeType = ConT typeName
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      tryDecType = fn1Type contentConT  (qHXBExcT typeType)
      decType = fn1Type contentConT typeType

  pushDeclHaddock ifDoc safeDecAsNam $
    "Attempt to decode an element of complex type represented as `"
    ++ baseNameStr ++ "`, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc decAsNam $
    "Decode an element of complex type represented as `" ++ baseNameStr
    ++ "`, or fail with a top-level `error`"

  return (typeName,
          [SigD safeDecAsNam tryDecType,
           FunD safeDecAsNam [Clause [VarP ctxtName]
                               (NormalB safeDecoder) []],

           SigD decAsNam decType,
           FunD decAsNam [Clause [ctxtVarP]
                           (NormalB $ resultOrThrow $
                             AppE (VarE safeDecAsNam) ctxtVarE)
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
assembleTryStatements [] _ _ constructor appNamesR = do
  whenDebugging $ dbgLn "assembleTryStatements base case"
  dbgResult "Returning" [
    NoBindS $
      applyReturn $
        foldl (\x y -> AppE x y) constructor (reverse appNamesR)
    ]
assembleTryStatements (r:refs) (s:subnames) ctxt constructor namesR = do
  whenDebugging $ do
    dbgLn "assembleTryStatements inductive "
    dbgBLabel "ref " r
    dbgPt $ "subname " ++ show s
  sub <-  indenting $
    xsdRefToSafeHaskellExpr ctxt r $ LitE $ StringL $ qName $ referenceBase r
  further <- indenting $
    assembleTryStatements refs subnames ctxt constructor (VarE s:namesR)
  return $ BindS (VarP s) sub : further
assembleTryStatements refs [] ctxt constructor namesR = do
  boxed $ do
    dbgLn "Found end of infinite list"
    dbgBLabel "REFS " refs
    dbgLn $ "CTXT " ++ show ctxt
    dbgBLabel "CONSTRUCTOR " constructor
    dbgBLabel "NAMESR" namesR
  error "This should be impossible when passing an arbitrarily long list for subnames"


-- | Translate a reference to an XSD element type to a Haskell
-- `Exp`ression representation describing the extraction of the given
-- value within an `Either` monad.
xsdRefToSafeHaskellExpr :: Name -> Reference -> Exp -> XSDQ Exp

xsdRefToSafeHaskellExpr param r@(ElementRef ref occursMin occursMax ln) _ctxt =
  let casePrefix = CaseE $ subcontentZom ref param
  in do
    whenDebugging $ dbgBLabel "Expr for (R1) " r
    typeName <- getElementTypeOrFail ref
    whenDebugging $ dbgLn $
      "Retrieved type " ++ qName typeName ++ " for " ++ showQName ref
    case (occursMin, occursMax) of
      (_, Just 0) -> dbgResult "Result (0)" $ TupE []
      (Just 0, Just 1) -> do
        decoderFn <- getSafeStringDecoder typeName
        matches <- zomMatch1
          (applyReturn nothingConE)
          (\paramName -> AppE justConE $ decoderFn $ VarE paramName)
          (qthAtMostOnceIn (showQName ref) ln)
        dbgResult "Result (1)" $ casePrefix matches
      (_, Just 1) -> do
        decoderFn <- getSafeStringDecoder typeName
        matches <- zomMatch1
          (qthMustBePresentIn (showQName ref) ln)
          (\paramName -> decoderFn $ VarE paramName)
          (qthAtMostOnceIn (showQName ref) ln)
        dbgResult "Result (2)" $ casePrefix matches
      _ -> do
        -- decoderFn <- getSafeStringDecoder typeName
        dbgResult "Result (3)" $
             AppE (AppE mapVarE
                        (AppE (decoderAsExpFor $ qName typeName)
                              (quoteStr $ qName ref)))
                  (AppE zomToListVarE (subcontentZom ref param))

xsdRefToSafeHaskellExpr param r@(AttributeRef ref usage) _ = do
  whenDebugging $ dbgBLabel "Expr for" r
  core <- xsdRefToSafeHaskellExpr' param $ qName ref
  dbgResultM "Result (A)" $
    fmap applyReturn $ unpackAttrDecoderForUsage usage core
  where unpackAttrDecoderForUsage :: AttributeUsage -> Exp -> XSDQ Exp
        unpackAttrDecoderForUsage Forbidden _ = return $ TupE []
        unpackAttrDecoderForUsage Optional expr = return expr
        unpackAttrDecoderForUsage Required expr = fmap (CaseE expr) $
          maybeMatches (throwsError "QDHXB: should not return Nothing") VarE

        xsdRefToSafeHaskellExpr' :: Name -> String -> XSDQ Exp
        xsdRefToSafeHaskellExpr' p rf =
          return $ AppE (decoderExpFor rf) (VarE p)


xsdRefToSafeHaskellExpr param ref@(TypeRef qn _lower _upper _l _d) ctxt = do
  whenDebugging $ dbgBLabel "Expr for (R3) " ref
  {-
  let xmlName = qName qn
      rootName = firstToUpper xmlName
      tryDecoderName = mkName $ "tryDecodeAs" ++ rootName
      res = AppE (AppE (VarE tryDecoderName) ctxt) (VarE param)
            -- TODO The parameter names are backwards here
  -}
  res <- getSafeStringDecoder qn
  dbgResult "Result (B)" (res $ VarE param)

{- Nothing unmatched right now
xsdRefToSafeHaskellExpr param ref ctxt = do
  boxed $ do
    dbgLn "xsdRefToSafeHaskellExpr unmatched"
    dbgLn $ "PARAM " ++ show param
    dbgBLabel "REF " ref
    dbgLn $ "CTXT " ++ show ctxt
  error "xsdRefToSafeHaskellExpr unmatched"
-}


-- | Called from generated code.
simpleTypeDecoder ::
  Content -> HXBErr -> (String -> HXBExcept a) -> HXBExcept a
{-# INLINE simpleTypeDecoder #-}
simpleTypeDecoder contentNode miscFailMsg stringDecoder =
  maybe (throwError miscFailMsg) stringDecoder $
    pullCRefContent contentNode

qLambdaCtntArg :: (Exp -> Exp) -> Exp
qLambdaCtntArg f = LamE [ctxtVarP] $ f ctxtVarE

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
  AppE (AppE (VarE $ mkName "QDHXB.Expansions.pullContentFrom")
             (LitE (StringL $ qName ref)))
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
  coreType <- getTypeHaskellType typeName
  typ <- containForBounds lower upper $ return coreType
  return (useBang, typ)

xsdRefToBangTypeQ (AttributeRef ref usage) =
  return (useBang,
          attrTypeForUsage usage $
            ConT $ mkName $ firstToUpper $ qName ref ++ "AttrType")

xsdRefToBangTypeQ (TypeRef typeName lower upper _ _) = do
  coreType <- getTypeHaskellType typeName
  typ <- containForBounds lower upper $ return coreType
  return (useBang, typ)

attrTypeForUsage :: AttributeUsage -> Type -> Type
attrTypeForUsage Forbidden _ = TupleT 0
attrTypeForUsage Optional typ = AppT maybeConT typ
attrTypeForUsage Required typ = typ

-- | Handy abbreviation of some TH boilerplate.
useBang :: Bang
useBang = Bang NoSourceUnpackedness NoSourceStrictness

pushDeclHaddock :: Maybe String -> Name -> String -> XSDQ ()
pushDeclHaddock ifDoc = do
  pushDeclHaddock' $ maybe "." (": " ++) ifDoc

pushDeclHaddock' :: String -> Name -> String -> XSDQ ()
pushDeclHaddock' suffix name spec = do
  liftQtoXSDQ $ addModFinalizer $ putDoc (DeclDoc name) $ spec ++ suffix
