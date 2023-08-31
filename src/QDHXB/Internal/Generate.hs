{-# LANGUAGE TemplateHaskell #-}

{-| Generate Haskell code from the flattened internal representation.
Each XSD type, attribute, and element definition is translated into a
Haskell type declaration plus several functions.

LOTS OF THIS IS OUTDATED

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
import QDHXB.Utils.TH
import QDHXB.Utils.XMLLight
import QDHXB.Utils.BPP
import QDHXB.Utils.Misc (ifAtLine)
import QDHXB.Internal.Types
import QDHXB.Internal.Block
import QDHXB.Internal.XSDQ
import QDHXB.Utils.Debugln (indenting, boxed, fileLocalDebuglnCall)
import QDHXB.Utils.DebuglnBlock (fileLocalDebuglnBlockCall)

fileLocalDebuglnCall "generate" 0 ["dbgLn", "dbgPt"]
fileLocalDebuglnBlockCall "generate" 0 [
  "dbgBLabel", "dbgBLabelFn1", "dbgResult", "dbgResultM"]

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
xsdDeclToHaskell decl@(ElementDefn nam typ implName _ln ifDoc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (e) " decl
    dbgBLabel "- typ " typ
  let origName = qName nam
      baseName = firstToUpper $ implName
      extractElemNam = mkName $ "extractElement" ++ baseName
      -- tryLoadNam = mkName $ "tryLoad" ++ baseName
  decodedType <- getTypeHaskellType typ
  decoderFn <- getTypeDecoderFn typ

  extractor <- do
    cparamName <- newName "content"
    pushDeclHaddock ifDoc extractElemNam
      ("Decode the given piece of @Content@ as a @\\<"
       ++ origName ++ ">@ element")
    return [
      SigD extractElemNam (fn1Type contentConT (qHXBExcT decodedType)),
      FunD extractElemNam [Clause [VarP cparamName]
                             (NormalB $ blockMakerClose decoderFn cparamName)
                             []]
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
                                     (NormalB $ AppE (VarE extractElemNam)
                                                     (VarE tmp1))
                                     []],
                          NoBindS $ applyReturn loadBodySrc]
                      ) []]
        ]

  dbgResult "Generated" $ extractor ++ subextractor ++ loader

xsdDeclToHaskell d@(AttributeDefn nam (AttributeGroupDefn ads) _ln doc) = do
  whenDebugging $ dbgBLabel "Generating from (f) " d
  decoder <- getSafeDecoderBody nam
  whenDebugging $ do
    dbgBLabelSrcDest "- decoder " decoder
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

xsdDeclToHaskell decl@(ComplexSynonymDefn nam typ _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (b) " decl
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType typ
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderCall typ
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    newAssemble nam (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc

xsdDeclToHaskell decl@(UnionDefn name pairs ln ifDoc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (c) UnionDefn " decl

  (safeCore, _, whereDecs) <- do
    whenDebugging $ dbgLn "- Calling unionDefnComponents"
    indenting $ unionDefnComponents getSafeDecoderCall name pairs ln
  whenDebugging $ do
    dbgLn "- whereDecs "
    indenting $ forM_ whereDecs $ dbgBLabelFn1 "- " srcName
    dbgBLabel "- safeCore " safeCore

  let makeConstr :: (QName, QName) -> XSDQ Con
      makeConstr (constructorName, tn) = do
        tyName <- getTypeHaskellType tn
        return $ NormalC (mkName $ firstToUpper $ qName constructorName)
                         [(useBang, tyName)]

  constrDefs <- mapM makeConstr pairs
  let typDef tn = DataD [] tn [] Nothing constrDefs
                    [DerivClause Nothing [eqConT, showConT]]
  whenDebugging $ dbgBLabelFn1 "- typDef " (mkName "NAME") typDef

  dbgResultM "Generated" $
    newAssemble name (Just typDef)
                (\src dest -> [
                    BindS (VarP dest) $
                      LetE (concat $ map (\f -> f src) whereDecs) safeCore
                    ])
                ifDoc


xsdDeclToHaskell decl@(ListDefn name elemTypeQName _ln ifDoc) = do
  whenDebugging $ dbgBLabel "Generating from (d) " decl
  -- error "REDO/d"
  elemTypeName <- getTypeHaskellName elemTypeQName
  let typDef tn = TySynD tn [] $ AppT ListT $ ConT $ mkName elemTypeName
  dec <- decoderForSimpleType name
  dbgResultM "Generated" $ newAssemble name (Just typDef) dec ifDoc
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


xsdDeclToHaskell decl@(ExtensionDefn qn base refs _ doc) = do
  whenDebugging $ do
    dbgBLabel "Generating from (i) " decl
  decoder <- getSafeDecoderBody qn
  hrefOut <- mapM xsdRefToBangTypeQ $ base : refs
  let typDef tn = DataD [] tn [] Nothing [NormalC tn $ hrefOut]
                    [DerivClause Nothing [eqConT, showConT]]
  dbgResultM "Generated" $
    newAssemble qn (Just typDef) decoder doc

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


xsdDeclToHaskell (ChoiceDefn name fields ln ifDoc) = do
  whenDebugging $ do
    dbgLn $ "Generating from (k)"
      ++ ifAtLine ln ++ " on ChoiceDefn " ++ showQName name
    dbgBLabel
      ("- Calling mapM (makeChoiceConstructor " ++ showQName name ++ ") on ")
      fields
  (constrDefs, _, _) <- indenting $
    fmap unzip3 $ mapM (makeChoiceConstructor name) fields
  whenDebugging $ dbgBLabel "- constrDefs " constrDefs
  let dataDef tn = DataD [] tn [] Nothing constrDefs
                     [DerivClause Nothing [eqConT, showConT]]
  whenDebugging $ dbgBLabelFn1 "- dataDef " (mkName "NAME") dataDef
  decoder <- getSafeDecoderBody name
  whenDebugging $ dbgBLabelSrcDest "- decoder " decoder
  dbgResultM "Generated" $ newAssemble name (Just dataDef) decoder ifDoc

xsdDeclToHaskell decl = do
  boxed $ do
    dbgLn "Uncaught case in xsdDeclToHaskell"
    dbgBLabel "DECL " decl
  error "Uncaught case in xsdDeclToHaskell"

-- | The `BlockMaker` result expects a `String` as its source, and the
-- Haskell type corresponding to the XSD type as its result.
getSafeStringDecoder :: QName -> XSDQ (BlockMaker String a)
getSafeStringDecoder qn = do
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Nothing -> liftExcepttoXSDQ $ throwError $ "No type " ++ bpp qn ++ " found"
    Just defn -> case defn of
      BuiltinDefn _ _ _ efn -> return $ \src dest ->
        [BindS (VarP dest) $ efn $ VarE src]
      ElementDefn _ ty _ _ _ -> getSafeStringDecoder ty
      AttributeDefn _ (SingleAttributeDefn ty _) _ _ ->
        getSafeStringDecoder ty
      SimpleSynonymDefn _ ty _ _ -> getSafeStringDecoder ty
      ListDefn _ elemTyp _ _ -> do
        elemDecoderBlockMaker <- getSafeStringDecoder elemTyp
        mapper <- abstractOnSourceName $ blockMakerClose elemDecoderBlockMaker
        return $ \src dest -> [
          BindS (VarP dest) $
            applyMapM mapper $ AppE (VarE $ mkName "words") (VarE src)
          ]
      UnionDefn nam cts ln _doc -> do
        (safeCore, _, whereDecs) <-
          unionDefnComponents getSafeStringDecoder nam cts ln
        return $ \src dest -> [
          BindS (VarP dest) $
            LetE (concat $ map (\f -> f src) whereDecs) safeCore
          ]
      ExtensionDefn _ _ _ _ _ ->
        error "TODO? string decoder for extension"
      ComplexSynonymDefn _ _ _ _ ->
        throwError "No string decoder for complex synonym type"
      SequenceDefn _ _ _ _ ->
        throwError "No string decoder for complex sequence"
      ChoiceDefn _ _ _ _ ->
        throwError "No string decoder for complex choice"
      GroupDefn _ _ _ _ ->
        throwError "No string decoder for complex group"
      AttributeDefn _ (AttributeGroupDefn _) _ _ ->
        throwError "No string decoder for attr. defn. over group"

getSafeDecoderBody :: QName -> XSDQ (BlockMaker Content dt)
getSafeDecoderBody qn = indenting $ do
  whenDebugging $ dbgBLabel "getSafeDecoderBody for " qn
  ifDefn <- getTypeDefn qn
  case ifDefn of

    Just defn -> do
      whenDebugging $ dbgBLabel "- Found type " defn
      case defn of
        BuiltinDefn ty _ _ _ -> do
          whenDebugging $ do
            dbgBLabel "- Basic type (aa) " ty
            dbgPt "Relay to decoderForSimpleType"
          decoderForSimpleType qn

        SequenceDefn nam refs _ln _doc -> do
          whenDebugging $ dbgPt "Sequence case (bb)"
          (bindingsF, boundNames) <- indenting $ makeSubexprLabeling refs
          let result :: BlockMaker Content dt
              result src dest =
                bindingsF src
                ++ [LetS [ValD (VarP dest)
                               (NormalB $ foldl (AppE)
                                      (ConE $ mkName $
                                       firstToUpper $ qName nam)
                                      (map VarE boundNames)) []]]
          dbgResultSrcDest "- result " result

        ChoiceDefn name fields _ _ -> do
          whenDebugging $ dbgPt "Choice case (cc)"
          (_, constrs, stmtsMaker) <-
            indenting $
              fmap unzip3 $ mapM (makeChoiceConstructor name) fields
          whenDebugging $ do
            dbgBLabel "- constrs " constrs
            dbgBLabel "- stmtsMaker " $
              map (\x -> x (mkName "SRC") (mkName "DEST")) stmtsMaker
          localDest <- newName "dest"
          let finishStmts :: Name -> (Exp, BlockMaker Content dt) -> Exp
              finishStmts src (constr, stmtMaker) =
                DoE Nothing $
                  stmtMaker src localDest
                  ++ [NoBindS $ applyReturn $ AppE constr $ VarE localDest]
              decoder :: BlockMaker Content dt
              decoder = \src dest ->
                let toDests :: [Exp]
                    toDests = map (finishStmts src) $ zip constrs stmtsMaker
                in [BindS (VarP dest) $ foldl1 replaceOnError toDests]
          return decoder

        ExtensionDefn edqn base refs _ _doc -> do
          whenDebugging $
            dbgBLabel "- Using getSafeDecoderBody (dd) with Extension for " edqn
          indenting $ do
            hrefOut <- mapM xsdRefToBangTypeQ $ base : refs
            whenDebugging $ dbgBLabel "- hrefOut " hrefOut

            whenDebugging $ dbgLn "- Calling makeSubexprLabeling"
            (bindingsF, boundNames) <- indenting $
              makeDecoderLabeling $ base:refs
            whenDebugging $ do
              dbgBLabelFn1 "- bindingsF " srcName bindingsF
              dbgBLabel "- boundNames " boundNames
            let result :: BlockMaker Content dt
                result src dest =
                  bindingsF src
                  ++ [LetS [ValD (VarP dest)
                                 (NormalB $
                                  foldl (AppE)
                                        (ConE $ mkName $ firstToUpper $
                                          qName edqn)
                                        (map VarE boundNames)) []]]
            dbgResultSrcDest "- result " result


        GroupDefn _name (TypeRef nam _lo _up _ln _doc) _ifLn _ifDoc -> do
          whenDebugging $
            dbgBLabel "- Using getSafeDecoderBody (ee) with Required for " nam
          getSafeDecoderUsageCall (nam, Required)
        GroupDefn _name (ElementRef _ _ _ _) _ifLn _ifDoc -> do
          error "ElementRef not allowed in GroupDefn (ff)"
        GroupDefn _name (AttributeRef _ _) _ifLn _ifDoc -> do
          error "AttributeRef not allowed in GroupDefn (gg)"

        ElementDefn _ _ty _ _ _ -> do
          error "REDO/2"
          -- getSafeDecoderBody ty
        AttributeDefn _ (SingleAttributeDefn _ty _) _ _ -> do
          error "TODO"
        SimpleSynonymDefn _ _ty _ _ -> do
          error "REDO/3"
          -- forSimpleType ty
        ComplexSynonymDefn _ _ _ _ -> do
          error "TODO"
        UnionDefn _ _ _ _ -> do
          error "TODO"
        ListDefn _listType _ _ _ -> do
          error "REDO/4"
          -- forSimpleType listType
        _ -> error "TODO"

    -- No type definition found, so look for an attribute group.
    Nothing -> do
      ifGroupDefn <- getAttributeGroup qn
      case ifGroupDefn of
        Just (AttributeGroupDefn subqns) -> do
          whenDebugging $ dbgLn "- AttributeGroupDefn (2aa) found"
          typeAndConstrName <- fmap mkName $ buildAttrOrGroupHaskellName qn
          haskellType <- buildAttrOrGroupHaskellType qn
          whenDebugging $ do
            dbgBLabel "- typeAndConstrName " typeAndConstrName
            dbgBLabel "- haskellType " haskellType
          whenDebugging $
            dbgPt "Mapping getSafeDecoderUsageCall onto group items"
          subdecoders <- indenting $ mapM getSafeDecoderUsageCall subqns
          whenDebugging $ do
            dbgLn "- subdecoders"
            indenting $ forM_ subdecoders $ \sd ->
              dbgBLabel "- " $ sd (mkName "SRC") (mkName "DEST")
          (subBinder, subNames) <- labelBlockMakers subdecoders
          whenDebugging $ do
            dbgBLabelFn1 "- subBinder " (mkName "SRC") $ subBinder
            dbgBLabel "- subNames " $ show subNames
          let res :: BlockMaker Content dt
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
          throwError $
            "Found (2bb) SingleAttributeDefn in attribute group table for "
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
              let decoder :: BlockMaker Content dt
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
              "No type or attribute/group (zz) " ++ bpp qn ++ " found"

decoderForSimpleType :: QName -> XSDQ (BlockMaker Content dt)
decoderForSimpleType qn = do
  whenDebugging $ do
    dbgLn "- decoderForSimpleType"
    dbgBLabel "  - qn " qn
  strDec <- getSafeStringDecoder qn
  retrievingCRefFor qn strDec


-- | Return an invocation of a safe decoder expected to be defined
-- elsewhere.
getSafeDecoderCall :: QName -> XSDQ (BlockMaker Content dt)
getSafeDecoderCall qn = do
  whenDebugging $ dbgPt $ "getSafeDecoderCall for " ++ showQName qn
  ifDefn <- getTypeDefn qn
  case ifDefn of
    Just defn -> do
      whenDebugging $ dbgBLabel "- Found type " defn
      indenting $ case defn of
        BuiltinDefn _ _ _ _ -> decoderForSimpleType qn
        _ -> baseByName
    _ -> baseByName

  where baseByName :: XSDQ (BlockMaker Content dt)
        baseByName =
          do let base :: BlockMaker Content dt
                 base = \src dest -> [
                   BindS (VarP dest) $
                     AppE (buildSafeDecoderExpFor $ qName qn) (VarE src)
                   ]
             return base

-- | Return an invocation of a safe decoder expected to be defined
-- elsewhere.
getSafeDecoderUsageCall ::
  (QName, AttributeUsage) -> XSDQ (BlockMaker Content dt)
getSafeDecoderUsageCall (qn, usage) = do
  whenDebugging $
    dbgPt $
      "getSafeDecoderUsageCall on " ++ showQName qn ++ " used " ++ show usage
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

-- | Given a list of `Reference`s which should match some present
-- element, produce the pair of (1) a map from the `Name` of a source
-- value to a list of TH `Stmt` statements producing these bound
-- values, and (2) the `Name`s of the bound values.
makeDecoderLabeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
makeDecoderLabeling refs = do
  whenDebugging $ dbgBLabel "- makeDecoderLabeling for " refs
  indenting $ do
    decoders <- mapM getRefSafeDecoder refs
    makeLabeling decoders [] []

  where makeLabeling ::
          [BlockMaker Content dt] -> [Name -> [Stmt]] -> [Name]
            -> XSDQ (Name -> [Stmt], [Name])
        makeLabeling [] srcFns destNames =
          return ((\src -> concat $ reverse $ map (\z -> z src) srcFns),
                  reverse destNames)
        makeLabeling (b:bs) srcFns destNames = do
          thisDest <- newName "sub"
          let thisFn = (\src -> b src thisDest)
          whenDebugging $ dbgBLabelFn1 "- makeLabeling ==> " srcName thisFn
          makeLabeling bs (thisFn:srcFns) (thisDest:destNames)

-- | Given a list of `Reference`s which should match subelements,
-- produce the pair of (1) a map from the `Name` of a source value to
-- a list of TH `Stmt` statements producing these bound values, and
-- (2) the `Name`s of the bound values.
makeSubexprLabeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
makeSubexprLabeling refs = do
  whenDebugging $ dbgBLabel "- makeSubexprLabeling for " refs
  indenting $ do
    blockMakers <- mapM referenceToBlockMaker refs
    whenDebugging $ do
      dbgLn "- blockMakers "
      forM_ blockMakers $ \b -> dbgBLabelSrcDest "  . " b
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
labelBlockMakers :: [BlockMaker st dt] -> XSDQ (Name -> [Stmt], [Name])
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
  [BlockMaker st dt] -> [String] -> [Name] -> [Name -> [Stmt]]
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
getRefSafeDecoder :: Reference -> XSDQ (BlockMaker Content dt)

getRefSafeDecoder (ElementRef nam _lower _upper _) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder ElementRef case"
  ifTypeOf <- getElementType nam
  case ifTypeOf of
    Nothing -> error $ "No type stored for element \"" ++ showQName nam ++ "\""
    Just typeOf -> getSafeDecoderCall typeOf

getRefSafeDecoder (AttributeRef ref usage) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder AttributeRef case"
  let refName = qName ref
  coreFn  <- indenting $ getAttrRefSafeDecoder refName
  usageFn <- indenting $ unpackAttrDecoderForUsage usage refName
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

getRefSafeDecoder (RawXML _ _) = do
  whenDebugging $ dbgLn "- getRefSafeDecoder RawXML"
  return $ \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ]

getAttrRefSafeDecoder :: String -> XSDQ (BlockMaker Content dt)
getAttrRefSafeDecoder rf = do
  whenDebugging $ dbgLn "getAttrRefSafeDecoder only case"
  let safeDec = buildSafeDecoderExpFor rf
  whenDebugging $ dbgBLabel "  - safeDec " safeDec
  return $ \src dest -> [ BindS (VarP dest) $ AppE safeDec (VarE src) ]

unpackAttrDecoderForUsage :: AttributeUsage -> String -> XSDQ (BlockMaker st dt)
unpackAttrDecoderForUsage Forbidden name = do
  whenDebugging $ dbgLn $
    "unpackAttrDecoderForUsage Forbidden case for " ++ name
  return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
unpackAttrDecoderForUsage Optional name = do
  whenDebugging $ dbgLn $
    "unpackAttrDecoderForUsage Optional case for " ++ name
  return $ \src dest -> [ LetS [ ValD (VarP dest) (NormalB $ VarE src) [] ] ]
unpackAttrDecoderForUsage Required name = do
  whenDebugging $ dbgLn $ "unpackAttrDecoderForUsage Required case for " ++ name
  matches <- maybeMatches (throwsError $
                           "QDHXB: got Nothing for required attribute " ++ name)
                          VarE
  return $ \src dest ->
    [LetS [ValD (VarP dest) (NormalB $ CaseE (VarE src) matches) []]]

-- | Convert a `Reference` into a `BlockMaker` calculating its value.
referenceToBlockMaker :: Reference -> XSDQ (BlockMaker Content dt)

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

referenceToBlockMaker r@(RawXML _ _) = do
  whenDebugging $ dbgBLabel "referenceToBlockMaker for" r
  return $ \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ]

refBlockMakerForBounds ::
  QName -> Type -> BlockMaker Content dt -> Maybe Int -> Maybe Int
  -> XSDQ (BlockMaker Content dt')
refBlockMakerForBounds sqn hsTyp indivF lo hi = indenting $
  refBlockMakerForBounds' sqn hsTyp (applyPullContentFrom $ qName sqn)
                          indivF lo hi

refBlockMakerForBounds' ::
  QName -> Type -> (Exp -> Exp) -> (BlockMaker Content dt)
  -> Maybe Int -> Maybe Int
  -> XSDQ (BlockMaker Content dt')
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
  -- a <- newName "eachPulled" -- unused
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

makeUsageBinder ::
  BlockMaker st dt -> AttributeUsage -> XSDQ (BlockMaker st dt)
makeUsageBinder _ Forbidden = do
  whenDebugging $ dbgLn "makeUsageBinder Forbidden case"
  return $ \_ dest -> [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]
makeUsageBinder singleTrans Optional = do
  whenDebugging $ dbgLn "makeUsageBinder Optional case"
  return $ \src dest -> singleTrans src dest
makeUsageBinder singleTrans Required = do
  whenDebugging $ dbgLn "makeUsageBinder Required case"
  return $ \src dest -> singleTrans src dest

-- | First in result triple: the `Con` spec for a `DataD` declaration.
-- Second is the constructor as a TH `Exp`.  Third is the `BlockMaker`
-- for the decoder function.
makeChoiceConstructor ::
  QName -> (QName, Reference) -> XSDQ (Con, Exp, BlockMaker Content dt)
makeChoiceConstructor name (constrSuffix, ref) = do
  whenDebugging $ dbgLn $ "Called makeChoiceConstructor "
    ++ showQName name ++ " (" ++ showQName constrSuffix ++ ", ...)"
  let typeRoot = firstToUpper $ qName name
  case ref of

    ElementRef elName ifMin ifMax _ -> do
      whenDebugging $ dbgLn $ "- With ElementRef " ++ showQName elName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName elName)
      whenDebugging $ do
        dbgBLabel "- constr " constrName
        dbgLn $ "- Calling getElementTypeOrFail " ++ showQName elName
      decQName <- getElementTypeOrFail elName
      whenDebugging $ dbgBLabel "- decQName " decQName
      decType <- getTypeHaskellType decQName
      whenDebugging $ dbgBLabel "- decType " decType
      useType <- containForBounds ifMin ifMax $ return decType
      whenDebugging $ dbgBLabel "- useType " useType
      decoderFn <- getTypeDecoderFn decQName
      return (
        NormalC constrName [(useBang, useType)],
        ConE constrName,
        decoderFn)

    GroupRef grName ifMin ifMax _ _ -> do
      whenDebugging $ dbgLn $ "- With GroupRef " ++ showQName grName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName grName)
      whenDebugging $ dbgBLabel "- constr " constrName
      groupDefn <- getGroupDefnOrFail grName
      whenDebugging $ dbgBLabel "- groupDefn " groupDefn
      case groupDefn of
        GroupDefn _ storedRef _ _ -> do
          whenDebugging $ dbgBLabel "- storedRef " storedRef
          case storedRef of
            TypeRef tyName _tyIfMin _tyIfMax _ _ -> do
              decType <- getTypeHaskellType tyName
              whenDebugging $ dbgBLabel "- decType " decType
              useType <- containForBounds ifMin ifMax $ return decType
              whenDebugging $ dbgBLabel "- useType " useType
              decoderFn <- getTypeDecoderFn tyName
              return (
                NormalC constrName [(useBang, useType)],
                ConE constrName,
                decoderFn)
            _ -> error $ "TODO makeChoiceConstructor (b/2) --- "
                   ++ "GroupDefn alias to non-type"
        _ -> do
          error $ "Ref/stored defn mismatch: expected GroupDefn but got\n"
            ++ bpp groupDefn

    TypeRef tyName ifMin ifMax _ _ -> do
      whenDebugging $ dbgLn $ "- With TypeRef " ++ showQName tyName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName tyName)
      whenDebugging $ dbgBLabel "- constr " constrName
      typeDefn <- getTypeDefn tyName
      whenDebugging $ dbgBLabel "- typeDefn " constrName
      decType <- getTypeHaskellType tyName
      whenDebugging $ dbgBLabel "- decType " decType
      useType <- containForBounds ifMin ifMax $ return decType
      whenDebugging $ dbgBLabel "- useType " useType
      decoderFn <- getTypeDecoderFn tyName
      whenDebugging $ do
        dbgLn "TODO makeChoiceConstructor - TypeRef case"
        dbgBLabel "NAME " name
        dbgBLabel "CONSTRSUFFIX " constrSuffix
        dbgBLabel "REF " ref
        dbgLn "--"
        dbgBLabel "CONSTRNAME " constrName
        dbgBLabel "TYPEDEFN " typeDefn
      return (
        NormalC constrName [(useBang, useType)],
        ConE constrName,
        decoderFn)

    AttributeRef _ _ -> do
      whenDebugging $ dbgLn $ "- With AttributeRef"
      error "Not expected: makeChoiceConstructor for AttributeRef"

    RawXML _ _ -> do
      whenDebugging $ dbgLn "- With RawXML"
      return $ (
        NormalC contentName [(useBang, contentConT)],
        ConE contentName,
        \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ])



newAssemble ::
  QName -> Maybe (Name -> Dec) -> BlockMaker Content dt -> Maybe String
  -> XSDQ [Dec]
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

xsdRefToBangTypeQ (RawXML _ _) = return (useBang, contentConT)

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
getTypeDecoderFn :: QName -> XSDQ (BlockMaker Content dt)
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


-- | The processors which produce a `BlockMaker` from a `UnionDefn`
-- all have the same structure, abstracted here.
unionDefnComponents ::
  -- Really, the abstracted bit is the actual source (`String`, XML
  -- `Content`, etc.).  Given a type, this function argument returns a
  -- `BlockMaker` whose source should be of this type, and whose
  -- result is of the given type.
  (QName -> XSDQ (BlockMaker st dt))
  -- Next the components of this `UnionDefn`
  -> QName -> [(QName, QName)] -> Maybe Line
  -- And the result is a computation returning a `BlockMaker`
  -> XSDQ (Exp, [Name], [Name -> [Dec]])
unionDefnComponents blockMakerBuilder name pairs ln = do
  let baseName = firstToUpper $ qName name
      baseType = ConT $ mkName baseName

  -- Prepare where-block bindings for calls to the alternative
  -- decoders, each of which tags their result with the appropriate
  -- constructor.
  let whereDecoderPair :: (QName, QName) -> XSDQ (Name, Name -> [Dec])
      whereDecoderPair (constr, typ) = do
        let binderName :: Name
            binderName = mkName $ "binder" ++ qName constr
        decoder <- blockMakerBuilder typ
        return (
          binderName,
          \src -> [
            SigD binderName (qHXBExcT baseType),
            ValD (VarP binderName)
                 (NormalB $
                    blockMakerCloseWith (AppE (ConE $ mkName $ firstToUpper $
                                                 qName constr))
                                        decoder src) []
            ])

  whereDecoderPairs <- mapM whereDecoderPair pairs
  let (names, decs) = unzip whereDecoderPairs

  let safeCore :: Exp
      safeCore =
        foldr (\n e -> applyCatchErrorExp (VarE n) (LamE [WildP] e))
              (qthNoValidContentInUnion baseName ln) names

  return (safeCore, names, decs)
