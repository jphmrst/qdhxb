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
import QDHXB.Internal.Utils.BPP
import QDHXB.Internal.Types
import QDHXB.Internal.Block
import QDHXB.Internal.XSDQ

-- | Translate a list of XSD definitions to top-level Haskell
-- declarations in the Template Haskell quotation monad.
xsdDeclsToHaskell :: [Definition] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  whenDebugging $ dbgLn "Generating Haskell declarations from definitions"
  dbgResultM "Declarations:" $
    -- Translate each declaration individually, and then concatenate
    -- the results together.
    fmap concat $ indenting $ mapM xsdDeclToHaskell defns


-- | Translate one XSD definition to a list of top-level Haskell
-- declarations in the Template Haskell quotation monad, and record
-- the associated Haddock documentation.
xsdDeclToHaskell :: Definition -> XSDQ [Dec]

xsdDeclToHaskell decl@(ElementDefn nam typ _ln ifDoc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (e) " decl
    dbgBLabel "- typ " typ
  let origName = qName nam
      baseName = firstToUpper $ origName
      extractElemNam = mkName $ "extractElement" ++ baseName
      -- tryLoadNam = mkName $ "tryLoad" ++ baseName
  decodedType <- getTypeHaskellType typ
  decoderFn <- getTypeDecoderFn typ

  extractor <- do
    cparamName <- newName "content"
    extRes <- newName "extRes"
    pushDeclHaddock ifDoc extractElemNam
      ("Decode the given piece of @Content@ as a @\\<"
       ++ origName ++ ">@ element")
    return [
      SigD extractElemNam (fn1Type contentConT (qHXBExcT decodedType)),
      FunD extractElemNam [Clause [VarP cparamName]
                            (NormalB $ DoE Nothing $
                              decoderFn cparamName extRes ++ [
                                NoBindS $ applyReturn $ VarE extRes
                                ]) []]
      ]

  subextractor <- do
    let extractSubElemsNam = mkName $ "extractSubElements" ++ baseName
    cparamName <- newName "content"
    pushDeclHaddock ifDoc extractElemNam
      ("Decode @\\<" ++ origName
       ++ ">@ subelements of a given piece of @Content@.")
    return [
      SigD extractSubElemsNam (fn1Type contentConT
                               (qHXBExcT $ AppT zomConT decodedType)),
      FunD extractSubElemsNam [
          Clause [VarP cparamName]
            (NormalB $
              (applyZommapM (VarE extractElemNam)
                (applyPullContentFrom origName $ VarE cparamName))) []]
      ]

  -- TODO Now add a version of EXTRACTOR for pulling a list (or a
  -- ZeroOneMany?) of them out of the subelements of a piece of
  -- Content.  This can be then used in other places like Sequence
  -- which a check of obeying occurs constraints.

  loader <- do
    let loadNam = mkName $ "load" ++ baseName
    exceptProc <- newName "exc"
    loadBodySrc <- resultOrThrow $ VarE exceptProc
    tmp1 <- newName "t"
    paramName <- newName "file"
    pushDeclHaddock ifDoc loadNam
      ("Load a @\\<" ++ origName ++ ">@ element from the given file")
    return [
        SigD loadNam (fn1Type stringConT (AppT ioConT decodedType)),
        FunD loadNam [Clause [VarP paramName]
                      (NormalB $ DoE Nothing $
                         [BindS (VarP tmp1) (applyLoadContent $ VarE paramName),
                          LetS [ValD (VarP exceptProc)
                                     (NormalB $
                                     AppE (VarE extractElemNam) (VarE tmp1)) []],
                          NoBindS $ applyReturn loadBodySrc]
                      ) []]
        ]

  dbgResult "Generated" $ extractor ++ subextractor ++ loader

xsdDeclToHaskell d@(AttributeDefn nam (AttributeGroupDefn ads) _ln doc) = do
  whenDebugging $ dbgBLabel "Generating from (f) " d
  decoder <- getSafeDecoderBody nam
  whenDebugging $ do
    dbgResultSrcDest "- decoder " decoder
    dbgLn "- getAttributeOrGroupTypeForUsage on each AttributeGroupDefn item:"
  hrefOut <- indenting $ mapM getAttributeOrGroupTypeForUsage ads
  dbgResultM "Generated" $
    newAssemble nam (Just $ \tn ->
                        DataD [] tn [] Nothing [
                          NormalC tn $ map (\x -> (useBang, x)) hrefOut
                          ]
                          [DerivClause Nothing [eqConT, showConT]])
                    decoder doc


xsdDeclToHaskell d@(AttributeDefn nam (SingleAttributeDefn typ _) _l ifd) = do
  whenDebugging $ dbgBLabel "Generating from (g) " d
  let xmlName = qName nam
      rootName = firstToUpper xmlName
      rootTypeName = mkName $ rootName -- ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ "tryDecodeAs" ++ rootName

  -- TODO Much of this is in getSafeDecoderBody --- prune out  duplication

  paramName <- newName "content"
  attrName <- newName "attr"
  puller <- [| pullAttrFrom $(return $ quoteStr $ qName nam)
                            $(return $ VarE paramName) |]
  -- let (haskellTyp, basicDecoder) = xsdNameToTypeTranslation $ qName typ
  -- let coreDecoder = basicDecoder $ VarE xName
  haskellTyp <- getTypeHaskellType typ
  coreDecoder <- getSafeStringDecoder typ
  let decoder = DoE Nothing [
        LetS [SigD attrName (AppT maybeConT stringConT),
              ValD (VarP attrName) (NormalB puller) []],
        NoBindS $
          caseNothingJust' (VarE attrName)
            (applyReturn nothingConE)
            xName (DoE Nothing $ coreDecoder xName resName ++ [
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

  decBody <- resultOrThrow $ AppE (VarE safeDecNam) (VarE paramName)
  let typeDef = TySynD rootTypeName [] haskellTyp
  dbgResult "Generated" $ (
        typeDef

        -- Safe decoder
        : SigD safeDecNam
               (fn1Type contentConT
                        (qHXBExcT (AppT maybeConT (ConT rootTypeName))))
        : FunD safeDecNam [Clause [VarP paramName] (NormalB decoder) []]
         -- Decoder
        : SigD decNam (fn1Type contentConT
                               (AppT maybeConT (ConT rootTypeName)))
        : FunD decNam [Clause [VarP paramName] (NormalB decBody) []]

        {-
        -- TODO Encoder
        SigD encNam encType
        FunD encNam [Clause [] (NormalB $ AppE errorVarE
                                               (quoteStr "TODO")) []]
        -}
        : [])


xsdDeclToHaskell decl@(SimpleSynonymDefn nam typ _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (a) " decl
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType typ
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderCall typ
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    newAssemble nam (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc

xsdDeclToHaskell decl@(ComplexSynonymDefn _nam _typ _ln _ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (b) " decl
  error "REDO"
  {-
  haskellType <- getTypeHaskellType typ
  decoder <- getSafeStringDecoder typ
  let baseName = firstToUpper $ qName nam
  (typeName, decs) <- indenting $
    getComplexTypeElements baseName (decoder $ VarE eName) ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @" ++ qName nam ++ "@ simple type"
  dbgResult "Generated" $ TySynD typeName [] haskellType : decs
  -}

xsdDeclToHaskell decl@(UnionDefn name pairs ln ifDoc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (c) UnionDefn " decl

  let baseName = qName name

  let makeConstr :: (QName, QName) -> Con
      makeConstr (constructorName, tn) =
        NormalC (mkName $ qName constructorName)
                [(useBang, ConT $ mkName $ qName tn)]

  let safeDecoder :: Exp
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

  whenDebugging $ dbgBLabel "- safeDecoder " safeDecoder


  error "REDO"
  {-
  (typeName, decs) <- indenting $
    getSimpleTypeElements baseName (VarP xName) safeDecoder ln ifDoc
  pushDeclHaddock ifDoc typeName $
    "Representation of the @\\<" ++ baseName ++ ">@ union"
  dbgResult "Generated" $
      DataD [] typeName [] Nothing (map makeConstr pairs)
            [DerivClause Nothing [eqConT, showConT]]
      : decs
  -}


xsdDeclToHaskell decl@(ListDefn _name _elemTypeRef _ln _ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (d) " decl
  error "REDO"
  {-

  {-
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
  -}

  elemHaskellType <- getTypeHaskellType elemTypeRef
  let haskellType = AppT ListT elemHaskellType
  decoder <- getSafeDecoderBody name
  dbgResultM "Generated" $
    newAssemble name (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc
  -}


xsdDeclToHaskell decl@(SequenceDefn nam refs _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (h) " decl
  decoder <- getSafeDecoderBody nam
  hrefOut <- mapM xsdRefToBangTypeQ refs
  dbgResultM "Generated" $
    newAssemble nam (Just $ \tn ->
                        DataD [] tn [] Nothing [NormalC tn $ hrefOut]
                              [DerivClause Nothing [eqConT, showConT]])
                    decoder ifDoc


xsdDeclToHaskell decl@(ExtensionDefn _qn _base _refs _ _) = do
  whenDebugging $ do
    dbgBLabel "Generating from (i) " decl
    dbgBLabel "DECL " decl
  error "REDO"
  {-
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
  -}


xsdDeclToHaskell decl@(GroupDefn _qn (TypeRef _tqn _ _ _ _) _ifLn _ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (j) " decl
  throwError
    "Should not encounter GroupDefn in flattened code, for XSDQ state only"
  {-
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType tqn
  whenDebugging $ dbgBLabel "- haskellType " haskellType
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderBody qn
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    newAssemble qn (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc
  -}


xsdDeclToHaskell decl@(ChoiceDefn name fields _ifLine ifDoc) = do
  whenDebugging $ do dbgBLabel "Generating from (k) " decl
  (constrDefs, _, _) <- indenting $
    fmap unzip3 $ mapM (makeChoiceConstructor name) fields
  let dataDef tn = DataD [] tn [] Nothing constrDefs
                     [DerivClause Nothing [eqConT, showConT]]
  decoder <- getSafeDecoderBody name
  whenDebugging $ do
    dbgBLabel "- constrDefs " constrDefs
    dbgBLabel "- dataDef " (dataDef $ mkName "NAME")
    dbgBLabelSrcDest "- decoder " decoder
  dbgResultM "Generated" $ newAssemble name (Just dataDef) decoder ifDoc

xsdDeclToHaskell decl = do
  boxed $ do
    dbgLn "Uncaught case in xsdDeclToHaskell"
    dbgBLabel "DECL " decl
  error "Uncaught case in xsdDeclToHaskell"

getSafeStringDecoder :: QName -> XSDQ BlockMaker
getSafeStringDecoder qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> case defn of
      BuiltinDefn _ _ _ efn -> return $ \src dest ->
        [BindS (VarP dest) $ efn $ VarE src]
      ElementDefn _ ty _ _ -> getSafeStringDecoder ty
      AttributeDefn _ (SingleAttributeDefn ty _) _ _ ->
        getSafeStringDecoder ty
      SimpleSynonymDefn _ ty _ _ -> getSafeStringDecoder ty
      ComplexSynonymDefn _ _ _ _ ->
        throwError "No string decoder for complex synonym type"
      SequenceDefn _ _ _ _ ->
        throwError "No string decoder for complex sequence"
      UnionDefn _ _ _ _ ->
        throwError "No string decoder for union"
      ChoiceDefn _ _ _ _ ->
        throwError "No string decoder for complex choice"
      GroupDefn _ _ _ _ ->
        throwError "No string decoder for complex group"
      ListDefn _ _ty _ _ -> do
        {-
        elemDecoder <- getSafeStringDecoder ty
        return $ \wholeExpr ->
          applyMapM (qLambdaCtntArg elemDecoder) (spaceSepApp wholeExpr)
        -}
        throwError "REDO"
      _ -> throwError "TODO"

getSafeDecoderBody :: QName -> XSDQ BlockMaker
getSafeDecoderBody qn = indenting $ do
  whenDebugging $ dbgBLabel "getSafeDecoderBody for " qn
  ifDefn <- getTypeDefn qn
  case ifDefn of

    Just defn -> do
      whenDebugging $ dbgBLabel "- Found type " defn
      indenting $ case defn of
        BuiltinDefn ty _ _ _ -> decoderForSimpleType qn ty

        SequenceDefn nam refs _ln _doc -> do
          whenDebugging $ dbgLn "  so Sequence case"
          (bindingsF, boundNames) <- indenting $ makeSubexprLabeling refs
          let result :: BlockMaker
              result src dest =
                bindingsF src
                ++ [LetS [ValD (VarP dest)
                               (NormalB $ foldl (AppE)
                                      (ConE $ mkName $ firstToUpper $ qName nam)
                                      (map VarE boundNames)) []]]
          dbgResultSrcDest "- result " result

        ChoiceDefn name fields _ _ -> do
          (_, constrs, stmtsMaker) <-
            indenting $ fmap unzip3 $ mapM (makeChoiceConstructor name) fields
          whenDebugging $ do
            dbgBLabel "- constrs " constrs
            dbgBLabel "- stmtsMaker " $
              map (\x -> x (mkName "SRC") (mkName "DEST")) stmtsMaker
          localDest <- newName "dest"
          let finishStmts :: Name -> (Exp, BlockMaker) -> Exp
              finishStmts src (constr, stmtMaker) =
                DoE Nothing $
                  stmtMaker src localDest
                  ++ [NoBindS $ applyReturn $ AppE constr $ VarE localDest]
              decoder = \src dest ->
                let toDests :: [Exp]
                    toDests = map (finishStmts src) $ zip constrs stmtsMaker
                in [BindS (VarP dest) $ foldl1 replaceOnError toDests]
          return decoder

        GroupDefn _name (TypeRef nam _lo _up _ln _doc) _ifLn _ifDoc -> do
          whenDebugging $
            dbgBLabel "- Using getSafeDecoderUsageCall with Required for " nam
          getSafeDecoderUsageCall (nam, Required)
        GroupDefn _name (ElementRef _ _ _ _) _ifLn _ifDoc -> do
          error "ElementRef not allowed in GroupDefn"
        GroupDefn _name (AttributeRef _ _) _ifLn _ifDoc -> do
          error "AttributeRef not allowed in GroupDefn"

        ElementDefn _ _ty _ _ -> do
          error "REDO"
          -- getSafeDecoderBody ty
        AttributeDefn _ (SingleAttributeDefn _ty _) _ _ -> do
          error "TODO"
        SimpleSynonymDefn _ _ty _ _ -> do
          error "REDO"
          -- forSimpleType ty
        ComplexSynonymDefn _ _ _ _ -> do
          error "TODO"
        UnionDefn _ _ _ _ -> do
          error "TODO"
        ListDefn _listType _ _ _ -> do
          error "REDO"
          -- forSimpleType listType
        _ -> error "TODO"

    -- No type definition found, so look for an attribute group.
    Nothing -> do
      ifGroupDefn <- getAttributeGroup qn
      case ifGroupDefn of
        Just (AttributeGroupDefn subqns) -> do
          whenDebugging $ dbgLn "- AttributeGroupDefn found"
          typeAndConstrName <- fmap mkName $ buildAttrOrGroupHaskellName qn
          haskellType <- buildAttrOrGroupHaskellType qn
          whenDebugging $ do
            dbgBLabel "- typeAndConstrName " typeAndConstrName
            dbgBLabel "- haskellType " haskellType
          whenDebugging $ dbgPt "Mapping getSafeDecoderUsageCall onto group items"
          subdecoders <- indenting $ mapM getSafeDecoderUsageCall subqns
          whenDebugging $ do
            dbgLn "- subdecoders"
            indenting $ forM_ subdecoders $ \sd ->
              dbgBLabel "- " $ sd (mkName "SRC") (mkName "DEST")
          (subBinder, subNames) <- labelBlockMakers subdecoders
          whenDebugging $ do
            dbgBLabelFn1 "- subBinder " (mkName "SRC") $ subBinder
            dbgBLabel "- subNames " $ show subNames
          let res :: BlockMaker
              res src dest = subBinder src ++ [
                LetS [
                    SigD dest (appMaybeType $ ConT typeAndConstrName),
                    ValD (VarP dest)
                      (NormalB $ applyJust $
                        -- Note above applyJust: right now the Maybe
                        -- constructor is built-in.  We should capture
                        -- the usage attribute in the
                        -- AttributeGroupDefn, and proceed based on
                        -- what it says.
                        foldl AppE (ConE typeAndConstrName) $
                          map VarE subNames)
                      []
                    ]
                ]
          whenDebugging $ dbgBLabelSrcDest "- result " res
          return res

        Just (SingleAttributeDefn typ _usage) -> do
          throwError $ "Found SingleAttributeDefn in attribute group table for "
            ++ qName typ

        -- And no attribute group definition found, so look for a
        -- single attrubute definition.
        Nothing -> do
          ifSingleDefn <- getAttributeDefn qn
          case ifSingleDefn of

            Just (SingleAttributeDefn typ _usage) -> do

              -- TODO Move decoder generation to here from
              -- xsdDeclToHaskell clause (g)?  Then generate for
              -- AttributeGroups from the same framework?
              attrName <- newName "attr"
              -- paramName <- newName "content"
              -- haskellTyp <- getTypeHaskellType typ
              coreDecoder <- getSafeStringDecoder typ
              let decoder :: BlockMaker
                  decoder src dest = [
                    LetS [SigD attrName (AppT maybeConT stringConT),
                          ValD (VarP attrName)
                          (NormalB $
                           applyPullAttrFrom (qName qn) (VarE src))
                          []],
                    BindS (VarP dest) $
                      caseNothingJust' (VarE attrName)
                      (applyReturn nothingConE)
                      xName (DoE Nothing $ coreDecoder xName resName ++ [
                                NoBindS $ applyReturn $ applyJust (VarE resName)
                                ])
                    ]
              return decoder

            Just (AttributeGroupDefn _) -> do
              throwError $
                "Found AttributeGroupDefn in single attribute table for "
                ++ qName qn

            Nothing -> liftExcepttoXSDQ $ throwError $
              "No type or attribute/group " ++ bpp qn ++ " found"

decoderForSimpleType :: QName -> QName -> XSDQ BlockMaker
decoderForSimpleType qn ty = do
  strDec <- getSafeStringDecoder ty
  retrievingCRefFor qn strDec


-- | Return an invocation of a safe decoder expected to be defined
-- elsewhere.
getSafeDecoderCall :: QName -> XSDQ BlockMaker
getSafeDecoderCall qn = do
  whenDebugging $ dbgPt $ "getSafeDecoderCall for " ++ showQName qn
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Just defn -> do
      whenDebugging $ dbgBLabel "- Found type " defn
      indenting $ case defn of
        BuiltinDefn ty _ _ _ -> decoderForSimpleType qn ty
        _ -> baseByName
    _ -> baseByName

  where baseByName :: XSDQ BlockMaker
        baseByName =
          do let base :: BlockMaker
                 base = \src dest -> [
                   BindS (VarP dest) $
                     AppE (buildSafeDecoderExpFor $ qName qn) (VarE src)
                   ]
             return base

-- | Return an invocation of a safe decoder expected to be defined
-- elsewhere.
getSafeDecoderUsageCall :: (QName, AttributeUsage) -> XSDQ BlockMaker
getSafeDecoderUsageCall (qn, usage) = do
  whenDebugging $
    dbgPt $ "getSafeDecoderUsageCall on " ++ showQName qn ++ " used " ++ show usage
  base <- indenting $ getSafeDecoderCall qn

  result <- case usage of
    Optional -> return base
    Required ->  do
      tmp <- newName "ifVal"
      core <- justOrThrow (VarE tmp) (LitE $ StringL "Value required")
      return $ \src dest ->
        base src tmp ++ [LetS [ValD (VarP dest) (NormalB core) []]]
    Forbidden -> return $ \_ dest ->
      [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]

  dbgResultSrcDest "  gives" result


-- | Calculate the Haskell type corresponding to a bound QName.
-- Checks the usage in the declaration/assumes Optional for groups for
-- now.
decodersReturnType :: QName -> XSDQ (Maybe Type)
decodersReturnType qn = indenting $ do
  whenDebugging $ dbgBLabel "decodersReturnType for " qn
  ifDefn <- getTypeDefn qn
  case ifDefn of

    Just defn -> do
      whenDebugging $ dbgBLabel "- Found type " defn
      fmap Just $ getTypeHaskellType qn

    Nothing -> do
      ifGroupDefn <- getAttributeGroup qn
      case ifGroupDefn of
        Just defn -> do
          whenDebugging $ dbgBLabel "- Found attribute group " defn
          case defn of
            SingleAttributeDefn _ _ -> throwError $
              "Expected AttributeGroupDefn but found SingleAttributeDefn"
            AttributeGroupDefn _ -> do
              -- Groups are always Optional for now, so wrap the base
              -- type in `Maybe`.
              fmap Just $ fmap appMaybeType $ buildAttrOrGroupHaskellType qn

        Nothing -> do
          ifSingleDefn <- getAttributeDefn qn
          case ifSingleDefn of
            Just defn -> do
              whenDebugging $ dbgBLabel "- Found single attribute " defn
              case defn of
                AttributeGroupDefn _ -> throwError $
                  "Expected SingleAttributeDefn but found AttributeGroupDefn"
                SingleAttributeDefn _ usage -> do
                  -- Groups are always Optional for now, so wrap the
                  -- base type in `Just`.
                  fmap Just $ fmap (attrTypeForUsage usage) $
                    buildAttrOrGroupHaskellType qn

            Nothing -> liftExcepttoXSDQ $ throwError $
              "No type or attribute/group " ++ bpp qn ++ " found"

-- | Given a list of `Reference`s to subelements, produce the
-- pair of (1) a map from the `Name` of a source value to a
-- list of TH `Stmt` statements producing these bound values,
-- and (2) the `Name`s of the bound values.
makeSubexprLabeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
makeSubexprLabeling refs = do
  blockMakers <- mapM referenceToBlockMaker refs
  labelBlockMakers blockMakers

-- | Utility function for @makeSubexprLabeling@ and other decoder
-- generators for terms with subexpressions.  Given a list of
-- `Reference`s to subelements, produces a pair of two values: (1) a
-- function from the `Name` of a source value to a list of TH `Stmt`
-- statements bindings names to the subexpressions, and (2) the
-- `Name`s of the bound values.  The base case joins the list of
-- functions by assembling a new map from the concatenation of the
-- results of the individual functions, and reversing the list of
-- names.
labelBlockMakers :: [BlockMaker] -> XSDQ (Name -> [Stmt], [Name])
labelBlockMakers blockMakers = do
  whenDebugging $ dbgPt "Calling labelBlockMakers"
  indenting $
    labelBlockMakers' blockMakers (map (\z -> "s" ++ show z) ([1..] :: [Int]))
                      [] []

-- | Utility function for @makeSubexprLabeling@ and other decoder
-- generators for terms with subexpressions.  Given a list of
-- `Reference`s to subelements, produces two lists: (1) functions from
-- the `Name` of a source value to a list of TH `Stmt` statements
-- producing these bound values, and (2) the `Name`s of the bound
-- values.  The base case joins the list of functions by assembling a
-- new map from the concatenation of the results of the individual
-- functions, and reversing the list of names.
labelBlockMakers' ::
  [BlockMaker] -> [String] -> [Name] -> [Name -> [Stmt]]
  -> XSDQ (Name -> [Stmt], [Name])
labelBlockMakers' [] _ accNames accFns = do
  whenDebugging $ dbgLn "- End of labelBlockMakers'"
  return
    ((\src -> concat $ reverse $ map (\x -> x src) accFns),
     reverse accNames)

labelBlockMakers' (bmk:bmks) (n:ns) accN accF = do
  fresh <- newName n
  whenDebugging $ do
    dbgLn $ "- labelBlockMakers': " ++ (show $ bmk (mkName "SRC") fresh)
  labelBlockMakers' bmks ns (fresh : accN) ((\src -> bmk src fresh) : accF)

labelBlockMakers' _ [] _ _ =
  error "Internal error --- end of infinite list"

{-
-- For an `ElementRef` of type @qn@, first calculate the
-- single-element safe-decoder, and then pass it to
-- `makeSubelementBinder` to adjust for the given lower/upper
-- occurrence bounds.
labelBlockMakers' (r@(ElementRef eqn lo hi _):rs) (n:ns) accN accF = do
  whenDebugging $ dbgBLabel "- labelBlockMakers' for " r
  singleDecoder <- indenting $ getRefSafeDecoder r
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  getRefSafeDecoder gives " $
        stringToBlock $
          pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  f' <- indenting $ makeSubelementBinder eqn singleDecoder lo hi
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  makeSubelementBinder gives " $
        stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  labelBlockMakers' rs ns (n : accN) ((\src -> f' src n) : accF)
 -- For an `AttributeRef` to values of type @qn@, first build a
-- single-value decoder, and then adjust for the attrubte
-- usage.
labelBlockMakers' (r@(AttributeRef _ usage):rs) (n:ns) accNs accFns = do
  whenDebugging $ dbgBLabel "- labelBlockMakers' for " r
  safeDec <- indenting $ getRefSafeDecoder r
  f' <- indenting $ makeUsageBinder safeDec usage
  let res src = f' src n
  whenDebugging $ do
    dbgBLabel "  - safeDec " $ safeDec (mkName "SRC") (mkName "DEST")
    dbgBLabel "  - f' " $ f' (mkName "SRC") (mkName "DEST")
    dbgBLabel "  - res " $ res (mkName "SRC")
  labelBlockMakers' rs ns (n : accNs) (res : accFns)

-- `TypeRef`s can occur e.g. when ChoiceDefn are lifted out to
-- flatten declarations.
labelBlockMakers' (r@(TypeRef tqn lo hi _ _):rs) (n:ns) accNs accFns = do
  whenDebugging $ dbgBLabel "- labelBlockMakers' for " r
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- getRefSafeDecoder r
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  getRefSafeDecoder gives " $
        stringToBlock $
          pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  f' <- indenting $ makeSubelementBinder tqn singleDecoder lo hi
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  makeSubelementBinder gives " $
        stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  labelBlockMakers' rs ns (n : accNs)
                   ((\src -> f' src n) : accFns)
-}


-- | Construct the body of a @tryDecode@ function for the given
-- `Reference`.
getRefSafeDecoder :: Reference -> XSDQ BlockMaker

getRefSafeDecoder (ElementRef nam _lower _upper _) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder ElementRef case"
  ifTypeOf <- getElementType nam
  case ifTypeOf of
    Nothing -> error $ "No type stored for element \"" ++ showQName nam ++ "\""
    Just typeOf -> getSafeDecoderCall typeOf

getRefSafeDecoder (AttributeRef ref usage) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder AttributeRef case"
  coreFn  <- indenting $ getAttrRefSafeDecoder $ qName ref
  usageFn <- indenting $ unpackAttrDecoderForUsage usage
  tmp <- newName "attr"
  whenDebugging $ do
    dbgBLabelSrcDest "  - coreFn " coreFn
    dbgBLabelSrcDest "  - usageFn " usageFn
  return $ \src dest -> coreFn src tmp ++ usageFn tmp dest

getRefSafeDecoder (TypeRef nam lower upper _ _) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder TypeRef case"
  singleBlockMaker <- getSafeDecoderCall nam
  scaleBlockMakerToBounds singleBlockMaker lower upper

getRefSafeDecoder (GroupRef nam lower upper _ _) = do
  whenDebugging $ dbgLn $
    "- getRefSafeDecoder GroupRef case for " ++ showQName nam
  defn <- getGroupDefn nam
  case defn of
    Just (GroupDefn _ (TypeRef typ _ _ _ _) _ _) -> do
      singleBlockMaker <- getSafeDecoderCall typ
      scaleBlockMakerToBounds singleBlockMaker lower upper
    _ -> do throwError $
              "QDHXB: group reference " ++ showQName nam
              ++ " to non-group definition"

getAttrRefSafeDecoder :: String -> XSDQ (BlockMaker)
getAttrRefSafeDecoder rf = do
  whenDebugging $ dbgLn "getAttrRefSafeDecoder only case"
  let safeDec = buildSafeDecoderExpFor rf
  whenDebugging $ dbgBLabel "  - safeDec " safeDec
  return $ \src dest -> [ BindS (VarP dest) $ AppE safeDec (VarE src) ]

unpackAttrDecoderForUsage :: AttributeUsage -> XSDQ (BlockMaker)
unpackAttrDecoderForUsage Forbidden = do
  whenDebugging $ dbgLn "unpackAttrDecoderForUsage Forbidden case"
  return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
unpackAttrDecoderForUsage Optional = do
  whenDebugging $ dbgLn "unpackAttrDecoderForUsage Optional case"
  return $ \src dest -> [ LetS [ ValD (VarP dest) (NormalB $ VarE src) [] ] ]
unpackAttrDecoderForUsage Required = do
  whenDebugging $ dbgLn "unpackAttrDecoderForUsage Required case"
  matches <- maybeMatches (throwsError "QDHXB: should not return Nothing") VarE
  return $ \src dest ->
    [LetS [ValD (VarP dest) (NormalB $ CaseE (VarE src) matches) []]]

-- | Convert a `Reference` to a `BlockMaker` calculating its
-- value.
referenceToBlockMaker :: Reference -> XSDQ BlockMaker

referenceToBlockMaker r@(ElementRef eqn lo hi _) = do
  whenDebugging $ dbgBLabel "referenceToBlockMaker for" r
  singleDecoder <- indenting $ getRefSafeDecoder r
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  getRefSafeDecoder gives " $
        stringToBlock $
          pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  ifTqn <- getElementType eqn
  let tqn = maybe (error $ "No element type for " ++ bpp eqn) id ifTqn
  hsType <- getTypeHaskellType tqn
  f' <- refBlockMakerForBounds eqn hsType singleDecoder lo hi
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  refBlockMakerForBounds gives " $
        stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

referenceToBlockMaker r@(AttributeRef _ usage) = do
  whenDebugging $ dbgBLabel "referenceToBlockMaker for" r
  safeDec <- indenting $ getRefSafeDecoder r
  f' <- indenting $ makeUsageBinder safeDec usage
  whenDebugging $ do
    dbgBLabelSrcDest "  - safeDec " safeDec
    dbgBLabelSrcDest "  - f' " f'
  return f'

referenceToBlockMaker r@(TypeRef tqn lo hi _ _) = do
  whenDebugging $ dbgBLabel "referenceToBlockMaker for" r
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- getRefSafeDecoder r
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  getRefSafeDecoder gives " $
        stringToBlock $
          pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  hsType <- getTypeHaskellType tqn
  f' <- indenting $ refBlockMakerForBounds tqn hsType singleDecoder lo hi
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  refBlockMakerForBounds gives " $
        stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

referenceToBlockMaker r@(GroupRef rqn lo hi _ _) = do
  whenDebugging $ dbgBLabel "referenceToBlockMaker for" r
  defn <- getGroupDefn rqn
  case defn of
    Just (GroupDefn _ (TypeRef gtyp _ _ _ _) _ _) -> do
      singleBlockMaker <- getSafeDecoderCall gtyp
      scaleBlockMakerToBounds singleBlockMaker lo hi
    _ -> throwError $
           "QDHXB: group reference " ++ showQName rqn
           ++ " to non-group definition"

  {-
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- getRefSafeDecoder r
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  getRefSafeDecoder gives " $
        stringToBlock $
          pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  hsType <- getTypeHaskellType tqn
  f' <- indenting $ refBlockMakerForBounds tqn hsType singleDecoder lo hi
  whenDebugging $ do
    dbgLn $ outBlock $
      labelBlock "  refBlockMakerForBounds gives " $
        stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'
  -}

refBlockMakerForBounds ::
  QName -> Type -> BlockMaker -> Maybe Int -> Maybe Int -> XSDQ BlockMaker
refBlockMakerForBounds sqn hsTyp indivF lo hi = indenting $
  refBlockMakerForBounds' sqn hsTyp (applyPullContentFrom $ qName sqn)
                          indivF lo hi

refBlockMakerForBounds' ::
  QName -> Type -> (Exp -> Exp) -> (BlockMaker) -> Maybe Int -> Maybe Int
  -> XSDQ (BlockMaker)
refBlockMakerForBounds' _ _ _ _ _ (Just 0) = do                  -- Unit
  whenDebugging $ dbgLn "refBlockMakerForBounds' unit case"
  return $ \ _ dest -> [ BindS (VarP dest) $ TupE [] ]

refBlockMakerForBounds' tqn _ puller indivF (Just 1) (Just 1) = do -- Single
  whenDebugging $ dbgLn "refBlockMakerForBounds' single case"
  pull <- newName "pullForSingle"
  single <- newName "single"
  return $ \src dest ->
    (LetS [SigD pull (AppT zomConT contentConT),
           ValD (VarP pull) (NormalB $ puller $ VarE src) []])
    : listToSingle ("Single instance of "
                    ++ showQName tqn
                    ++ " subelement required, none/multiple found")
                   pull single
    ++ indivF single dest

refBlockMakerForBounds' _ _ puller indivF _ (Just 1) = do        -- Maybe
  whenDebugging $ do
    dbgLn "refBlockMakerForBounds' maybe case"
    dbgBLabel "- given puller " $ puller $ VarE $ mkName "SRC"
    dbgBLabelSrcDest "- given indivF " indivF
  pull <- newName "pullForMaybe"
  pullMaybe <- newName "maybeOne"
  tmp <- newName "subres"
  a <- newName "eachPulled"
  finalCase <- caseNothingJust (VarE pullMaybe)
    (applyReturn nothingConE)
    (\nam -> DoE Nothing $
      indivF nam tmp ++ [
        NoBindS $ applyReturn $ applyJust $ VarE tmp])
  let result = \src dest ->
        LetS [SigD pull (AppT zomConT contentConT),
              ValD (VarP pull) (NormalB $ puller $ VarE src) []]
        : listToMaybe pull pullMaybe
        ++ [BindS (VarP dest) finalCase]
  whenDebugging $ dbgBLabelSrcDest "- result " result
  return result

refBlockMakerForBounds' _tqn hsType puller indivF _ _ = do             -- List
  whenDebugging $ dbgLn "refBlockMakerForBounds' list case"
  pull <- newName "pullForList"
  asList <- newName "asList"
  tmp <- newName "postpull"
  a <- newName "a"
  whenDebugging $ dbgBLabel "- hsType" hsType
  return $ \src dest -> [
    LetS [SigD pull (AppT zomConT contentConT),
          ValD (VarP pull) (NormalB $ puller $ VarE src) []],
    BindS (VarP asList) $ applyZomToList $ VarE pull,
    BindS (VarP dest) $
      applyMapM (LamE [VarP a] $ DoE Nothing $
                   indivF a tmp ++ [
                     NoBindS $ applyReturn $ SigE (VarE tmp) hsType]) $
        SigE (VarE asList) (AppT ListT contentConT)
    ]

makeUsageBinder :: BlockMaker -> AttributeUsage -> XSDQ BlockMaker
makeUsageBinder _ Forbidden = do
  whenDebugging $ dbgLn "makeUsageBinder Forbidden case"
  return $ \_ dest -> [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]
makeUsageBinder singleTrans Optional = do
  whenDebugging $ dbgLn "makeUsageBinder Optional case"
  return $ \src dest -> singleTrans src dest
makeUsageBinder singleTrans Required = do
  whenDebugging $ dbgLn "makeUsageBinder Required case"
  return $ \src dest -> singleTrans src dest

{-
getSafeDecoderUsageCall :: QName -> XSDQ (BlockMaker)
getSafeDecoderUsageCall qn = indenting $ do
  whenDebugging $ dbgBLabel "getSafeDecoderUsageCall for " qn
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> do
      whenDebugging $ dbgLn "- Nothing found"
      liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> do
      whenDebugging $ dbgBLabel "- Found " defn
      case defn of
        BuiltinDefn ty _ _ _ -> forSimpleType ty
        ElementDefn _ ty _ _ -> getSafeDecoderCall ty
        AttributeDefn _ (SingleAttributeDefn ty _) _ _ -> do
          error "TODO"
        SimpleSynonymDefn _ ty _ _ -> forSimpleType ty
        ComplexSynonymDefn _ _ _ _ -> do
          error "TODO"
        SequenceDefn nam refs _ln _doc -> do
          refDecoders <- mapM getRefSafeDecoder refs
          let (bindingsF, boundNames) = makeBindings refDecoders
          return $ \ctnt ->
            DoE Nothing $
              bindingsF ctnt
              ++ [NoBindS $ applyReturn $
                    foldl (AppE) (ConE $ mkName $ firstToUpper $ qName nam)
                          (map VarE boundNames)]
        UnionDefn _ _ _ _ -> do
          error "TODO"
        ChoiceDefn _ _ _ _ -> do
          error "TODO"
        GroupDefn _ _ _ _ -> do
          error "TODO"
        ListDefn listType _ _ _ -> forSimpleType listType
        _ -> error "TODO"

  where forSimpleType ty = do
          strDec <- getSafeStringDecoder ty
          return $ forSimpleTypeWith strDec

        forSimpleTypeWith f =
          \ctnt -> app3Exp simpleTypeDecoderVarE
                           ctnt
                           (qCrefMustBePresentIn (qName qn) Nothing)
                           (qLambdaCtntArg f)

makeTryDecoder :: BlockMaker -> [Exp] -> [Exp] -> Exp
makeTryDecoder x ctxt (c:constr) (t:tryFns) =
  makeTryDecoder' x ctxt (app2Exp fmapVarE c (app2Exp t x ctxt)) constr tryFns
  where makeTryDecoder' _ _ exp [] _ = exp
        makeTryDecoder' x ctxt exp (c:constr) (t:tryFns) =
          makeTryDecoder' x ctxt
            (exp `replaceOnError` (app2Exp fmapVarE c (app2Exp t x ctxt)))
            constr tryFns
-}


-- | First in result triple: the `Con` spec for a `DataD` declaration.
-- Second is the constructor as a TH `Exp`.  Third is the `BlockMaker`
-- for the decoder function.
makeChoiceConstructor ::
  QName -> (QName, Reference) -> XSDQ (Con, Exp, BlockMaker)
makeChoiceConstructor name (typeName, ref) = do
  let typeRoot = firstToUpper $ qName name
  case ref of

    ElementRef elName ifMin ifMax _ -> do
      let constrBase = typeRoot ++ (firstToUpper $ qName elName)
          constrName = mkName constrBase
          -- (decName, _) = xsdNameToNameTranslation $ qName typeName
      decName <- getTypeHaskellName typeName
      useType <- containForBounds ifMin ifMax $ return $ ConT $ mkName decName
      decoderFn <- getTypeDecoderFn typeName
      return (NormalC constrName [(useBang, useType)],
              ConE constrName,
              decoderFn -- VarE (mkName $ "tryDecodeAs" ++ decName)
             )
              -- TODO --- must check for basic types here.  Isn't
              -- there a function to return a LamE or this VarE here?
              -- ==> Check out `getTypeDecoderFn`

    GroupRef _grName _ifMin _ifMax _ _ -> do
      boxed $ do
        dbgLn "TODO makeChoiceConstructor - GroupRef case"
        dbgBLabel "NAME " name
        dbgBLabel "TYPENAME " typeName
        dbgBLabel "REF " ref
      error "TODO makeChoiceConstructor (b)"

    TypeRef _tyName _ifMin _ifMax _ _ -> do
      boxed $ do
        dbgLn "TODO makeChoiceConstructor - TypeRef case"
        dbgBLabel "NAME " name
        dbgBLabel "TYPENAME " typeName
        dbgBLabel "REF " ref
      error "TODO makeChoiceConstructor (c)"

    AttributeRef _ _ ->
      error "Not expected: makeChoiceConstructor for AttributeRef"


newAssemble ::
  QName -> Maybe (Name -> Dec) -> BlockMaker -> Maybe String -> XSDQ [Dec]
newAssemble base tyDec safeDec ifDoc = do
  whenDebugging $ dbgBLabel "newAssemble with " base

  -- TODO --- Update the type extraction here.  Write a
  -- "decodersReturnType base" function that checks the usage in the
  -- declaration/assumes Optional for groups for now.

  ifDecoderType <- indenting $ decodersReturnType base
  decoderType <- case ifDecoderType of
    Nothing -> throwError $ "No return type for " ++ showQName base
    Just x -> return x
  whenDebugging $ dbgBLabel "- decoderType " decoderType

  let baseNameStr = firstToUpper $ qName base
      typeName = mkName baseNameStr
      safeDecAsNam = mkName $ "tryDecodeAs" ++ baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      tryDecType = fn1Type contentConT (qHXBExcT decoderType)
      decType = fn1Type contentConT decoderType
  whenDebugging $ do
    dbgBLabel "- tryDecType " tryDecType
    dbgBLabel "- decType " decType
  paramName <- newName "ctnt"

  pushDeclHaddock ifDoc safeDecAsNam $
    "Attempt to decode an element represented as `"
    ++ baseNameStr ++ "`, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc decAsNam $
    "Decode an element of simple type represented as `" ++ baseNameStr
    ++ "`, or fail with a top-level `error`"

  res <- newName "res"
  decodeBody <- resultOrThrow $ AppE (VarE safeDecAsNam) (VarE paramName)
  whenDebugging $ dbgBLabel "- decodeBody " decodeBody
  let baseList = [
        SigD safeDecAsNam tryDecType,
        FunD safeDecAsNam [Clause [VarP paramName]
                            (NormalB $ DoE Nothing $
                               safeDec paramName res ++ [
                                NoBindS $ applyReturn $ VarE res
                                ]) []],
        SigD decAsNam decType,
        FunD decAsNam [Clause [VarP paramName] (NormalB decodeBody) []]
        ]

  case tyDec of
    Nothing -> return baseList
    Just tf -> do
      pushDeclHaddock ifDoc typeName $
        "Representation of the @" ++ baseNameStr ++ "@ type"
      return $ tf typeName : baseList

{-
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
    "Attempt to decode a string representing a value of the XSD simple type "
    ++ "as a `" ++ baseNameStr ++ "` value, throwing a `QDHXB.Errs.HXBErr` "
    ++ "in the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc safeDecAsNam $
    "Attempt to decode an element of simple type represented as `"
    ++ baseNameStr ++ "`, throwing a `QDHXB.Errs.HXBErr` in "
    ++ "the `Control.Monad.Except` monad if loading or parsing fails"
  pushDeclHaddock ifDoc decAsNam $
    "Decode an element of simple type represented as `" ++ baseNameStr
    ++ "`, or fail with a top-level `error`"

  strictDecBody <- resultOrThrow $ AppE (VarE safeDecAsNam) ctxtVarE

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
                           (NormalB strictDecBody)
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
  core <- getAttrRefSafeDecoder param $ qName ref
  dbgResultM "Result (A)" $
    fmap applyReturn $ unpackAttrDecoderForUsage usage core
  where unpackAttrDecoderForUsage :: AttributeUsage -> Exp -> XSDQ Exp
        unpackAttrDecoderForUsage Forbidden _ = return $ TupE []
        unpackAttrDecoderForUsage Optional expr = return expr
        unpackAttrDecoderForUsage Required expr = fmap (CaseE expr) $
          maybeMatches (throwsError "QDHXB: should not return Nothing") VarE

        getAttrRefSafeDecoder :: Name -> String -> XSDQ Exp
        getAttrRefSafeDecoder p rf =
          return $ AppE (buildDecoderNameFor rf) (VarE p)

xsdRefToSafeHaskellExpr param ref@(TypeRef qn _lower _upper _l _d) _ctxt = do
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
-}

{-
-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.  __Note__ that this function will
-- just operate on the name; there is no assurance that the name will
-- actually exist.
buildDecoderNameFor :: String -> Exp
buildDecoderNameFor ref = VarE $ mkName $ "decode" ++ firstToUpper ref
-}

-- | From an element reference name, construct the associated Haskell
-- decoder function `Exp`ression.  __Note__ that this function will
-- just operate on the name; there is no assurance that the name will
-- actually exist.
buildSafeDecoderExpFor :: String -> Exp
buildSafeDecoderExpFor ref = VarE $ mkName $ "tryDecodeAs" ++ firstToUpper ref

-- | Builds a list of two `Match`es for a `Maybe` expression, given
-- the alternative expressions for `Nothing` and `Just` (the latter
-- parameterized over a `Name`).
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
            ConT $ mkName $ firstToUpper $ qName ref {- ++ "AttrType" -} )

xsdRefToBangTypeQ (TypeRef typeName lower upper _ _) = do
  coreType <- getTypeHaskellType typeName
  typ <- containForBounds lower upper $ return coreType
  return (useBang, typ)

xsdRefToBangTypeQ (GroupRef groupName lower upper _ _) = do
  defn <- getGroupDefn groupName
  case defn of
    Just (GroupDefn _ (TypeRef typeName _ _ _ _) _ _) -> do
      coreType <- getTypeHaskellType typeName
      typ <- containForBounds lower upper $ return coreType
      return (useBang, typ)
    _ -> do throwError $
              "QDHXB: group reference " ++ showQName groupName
              ++ " to non-group definition"

  {-
  coreType <- getTypeHaskellType typeName
  typ <- containForBounds lower upper $ return coreType
  return (useBang, typ)
  -}

attrTypeForUsage :: AttributeUsage -> Type -> Type
attrTypeForUsage Forbidden _ = TupleT 0
attrTypeForUsage Optional typ = AppT maybeConT typ
attrTypeForUsage Required typ = typ

pushDeclHaddock :: Maybe String -> Name -> String -> XSDQ ()
pushDeclHaddock ifDoc = do
  pushDeclHaddock' $ maybe "." (": " ++) ifDoc

pushDeclHaddock' :: String -> Name -> String -> XSDQ ()
pushDeclHaddock' suffix name spec = do
  liftQtoXSDQ $ addModFinalizer $ putDoc (DeclDoc name) $ spec ++ suffix

-- | Called from generated code.
simpleTypeDecoder ::
  Content -> HXBErr -> (String -> HXBExcept a) -> HXBExcept a
{-# INLINE simpleTypeDecoder #-}
simpleTypeDecoder contentNode miscFailMsg stringDecoder =
  maybe (throwError miscFailMsg) stringDecoder $
    pullCRefContent contentNode


-- | Given the `QName` of an XSD type @t@, return a function from
-- `Name`s @src@ and @dest@ to the list of TH `Stmt`s which, assuming
-- that XML `Content` is stored at the identifier @src@, writes the
-- decoded value of the Haskell type corresponding to @t@ to @dest@.
getTypeDecoderFn :: QName -> XSDQ BlockMaker
getTypeDecoderFn qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $
      "No type \"" ++ bpp qn ++ "\" found"
    Just defn ->
      case defn of
        BuiltinDefn _ty _ _ efn -> do
          strmaybe <- newName "strmaybe"
          str <- newName "str"
          strBody <- justOrThrow (VarE strmaybe)
                       (LitE $ StringL "No CRef content for simple type")
          return $ \src dest -> [
            -- Built-ins are all simple types, so the first step is to
            -- extract the string contents.
            LetS [
                ValD (VarP strmaybe)
                     (NormalB $ applyPullCRefContent $ VarE src) [],

                -- Second step is to extract the actual value, or fail.
                ValD (VarP str) (NormalB strBody) []
                ],

            -- Finally convert this string into the typed value.
            BindS (VarP dest) $ efn $ VarE str
            ]
          -- fmap (qLambdaWithArg (mkName "w")) $ forSimpleType ty
        _ -> return $ \src dest -> [
          BindS (VarP dest)
                (AppE (VarE $ mkName $
                         "tryDecodeAs" ++ (firstToUpper $ qName qn))
                      (VarE src))]

  {-
  where forSimpleType :: QName -> XSDQ BlockMaker
        forSimpleType ty = do
          strDec <- getSafeStringDecoder ty
          forSimpleTypeWith strDec
          {- resultOrThrow . (forSimpleTypeWith strDec) -}

        forSimpleTypeWith :: BlockMaker -> XSDQ BlockMaker
        forSimpleTypeWith f = do
          tmp1 <- newName "maybeContent"
          tmp2 <- newName "content"
          return $ \src dest ->  [
            BindS (VarP tmp1) (applyPullCRefContent $ VarE src),
            BindS (VarP tmp2)
              (applyMaybe (qCrefMustBePresentIn (qName qn) Nothing)
                          quotedId
                          (VarE tmp1)) {- ,
            BindS (VarP dest) $
              app3Exp simpleTypeDecoderVarE
                      (VarE tmp2) (qCrefMustBePresentIn (qName qn) Nothing)
                      (VarE src) -}
                                ] ++ f tmp2 dest

                                 app3Exp simpleTypeDecoderVarE
                                  (VarE src)
                                  (qCrefMustBePresentIn (qName qn) Nothing)
                                  (qLambdaCtntArg f)
  -}
