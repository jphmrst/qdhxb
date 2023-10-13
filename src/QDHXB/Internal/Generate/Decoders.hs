{-# LANGUAGE TemplateHaskell #-}

{-| FILLIN
-}

module QDHXB.Internal.Generate.Decoders (
  getSafeStringDecoder, getSafeDecoderBody, decoderForSimpleType,
  getSafeDecoderCall, makeChoiceConstructor, simpleTypeDecoder,
  getTypeDecoderFn, unionDefnComponents
  )
where

import Control.Monad.Except
-- import Control.Monad.Extra (whenJust)
import Language.Haskell.TH
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, Content, qName, Line)
import QDHXB.Errs
import QDHXB.Utils.TH
import QDHXB.Utils.XMLLight
import QDHXB.Utils.BPP
-- import QDHXB.Utils.Misc (ifAtLine)
import QDHXB.Internal.Types
import QDHXB.Internal.Block
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate.Types

import QDHXB.Internal.Debugln hiding (
  dbgLn, dbgPt, dbgBLabel, dbgBLabelFn1, dbgBLabelFn2,
  dbgResult, dbgResultFn2, dbgResultM)
import qualified QDHXB.Internal.Debugln as DBG
dbgLn :: (MonadDebugln m n) => String -> m ()
dbgLn = DBG.dbgLn generate 0
dbgPt :: (MonadDebugln m n) => String -> m ()
dbgPt = DBG.dbgPt generate 0
dbgBLabel :: (MonadDebugln m n, Blockable c) => String -> c -> m ()
dbgBLabel = DBG.dbgBLabel generate 0
dbgBLabelFn1 ::
  (MonadDebugln m n, Blockable r) => String -> a -> (a -> r) -> m ()
dbgBLabelFn1 = DBG.dbgBLabelFn1 generate 0
dbgBLabelFn2 ::
  (MonadDebugln m n, Blockable r) => String -> a -> b -> (a -> b -> r) -> m ()
dbgBLabelFn2 = DBG.dbgBLabelFn2 generate 0
-- dbgResult :: (MonadDebugln m n, Blockable a) => String -> a -> m a
-- dbgResult = DBG.dbgResult generate 0
-- dbgResultM :: (MonadDebugln m n, Blockable a) => String -> m a -> m a
-- dbgResultM = DBG.dbgResultM generate 0
dbgResultFn2 ::
  (MonadDebugln m n, Blockable r) =>
    String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
dbgResultFn2 = DBG.dbgResultFn2 generate 0
dbgBLabelSrcDest ::
  Blockable c => String -> (Name -> Name -> c) -> XSDQ ()
{-# INLINE dbgBLabelSrcDest #-}
dbgBLabelSrcDest msg = dbgBLabelFn2 msg srcName destName
dbgResultSrcDest ::
  Blockable c => String -> (Name -> Name -> c) -> XSDQ (Name -> Name -> c)
{-# INLINE dbgResultSrcDest #-}
dbgResultSrcDest msg = dbgResultFn2 msg srcName destName


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
      AttributeDefn _ (SingleAttributeDefn ty _ _) _ _ ->
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
      AttributeDefn _ (AttributeGroupDefn _ _) _ _ ->
        throwError "No string decoder for attr. defn. over group"

-- | Given the qualified name of an XSD-defined entity (expected to be
-- found in the XSDQ state), return the series of statement steps for
-- decoding XML `Content` into a value of that entity type.
--
-- The top-level simply decodes the storage location of the given
-- name, and then dispatches to a subsidiary definition.
getSafeDecoderBody :: QName -> XSDQ (BlockMaker Content dt)
getSafeDecoderBody qn = indenting $ do
  retr <- retrieveDeclaration qn
  case retr of
    IsTypeDefinition d -> decoderBodyForTypeDefn d
    IsAttributeGroup ad -> decoderBodyForAttributeGroupDefn ad
    IsAttributeType ad -> decoderBodyForSingleAttributeDefn ad
    IsGroupDefinition _ -> liftExcepttoXSDQ $ throwError $
      "getSafeDecoderBody received unexpected GroupDefinition for " ++ bpp qn
    IsElementType et -> liftExcepttoXSDQ $ throwError $
      "getSafeDecoderBody received unexpected Element type " ++ bpp et
       ++ " for " ++ bpp qn

  where

    decoderBodyForSingleAttributeDefn sad = case sad of
      SingleAttributeDefn typ _usage _hn -> do

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

      AttributeGroupDefn _ _ -> do
              throwError $
                "Found AttributeGroupDefn in single attribute table for "
                ++ qName qn


    decoderBodyForAttributeGroupDefn agd = case agd of
      AttributeGroupDefn subqns _ -> do
          dbgLn "- AttributeGroupDefn (2aa) found"
          typeAndConstrName <- fmap mkName $ buildAttrOrGroupHaskellName qn
          dbgBLabel "- typeAndConstrName " typeAndConstrName
          haskellType <- buildAttrOrGroupHaskellType qn
          dbgBLabel "- haskellType " haskellType
          dbgPt "Mapping getSafeDecoderUsageCall onto group items"
          subdecoders <- indenting $ mapM getSafeDecoderUsageCall subqns
          dbgLn "- subdecoders"
          whenDebugging generate 3 $ indenting $ forM_ subdecoders $ \sd ->
              dbgBLabel "- " $ sd (mkName "SRC") (mkName "DEST")
          (subBinder, subNames) <- labelBlockMakers subdecoders
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
          dbgBLabelSrcDest "- result " res
          return res

      SingleAttributeDefn typ _usage _hn -> do
          throwError $
            "Found (2bb) SingleAttributeDefn in attribute group table for "
            ++ qName typ


    decoderBodyForTypeDefn defn = do
      dbgBLabel "- Found type " defn
      case defn of
        BuiltinDefn ty _ _ _ -> do
          dbgBLabel "- Basic type (aa) " ty
          dbgPt "Relay to decoderForSimpleType"
          decoderForSimpleType qn

        SequenceDefn nam refs _ln _doc -> do
          dbgPt "Sequence case (bb)"
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
          dbgPt "Choice case (cc)"
          (_, constrs, stmtsMaker) <-
            indenting $
              fmap unzip3 $ mapM (makeChoiceConstructor name) fields
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
          dbgBLabel "- Using getSafeDecoderBody (dd) with Extension for " edqn
          indenting $ do
            hrefOut <- mapM xsdRefToBangTypeQ $ base : refs
            dbgBLabel "- hrefOut " hrefOut

            dbgLn "- Calling makeSubexprLabeling"
            (bindingsF, boundNames) <- indenting $
              makeDecoderLabeling $ base:refs
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
          dbgBLabel "- Using getSafeDecoderBody (ee) with Required for " nam
          getSafeDecoderUsageCall (nam, Required)
        GroupDefn _name (ElementRef _ _ _ _) _ifLn _ifDoc -> do
          error "ElementRef not allowed in GroupDefn (ff)"
        GroupDefn _name (AttributeRef _ _) _ifLn _ifDoc -> do
          error "AttributeRef not allowed in GroupDefn (gg)"

        ElementDefn _ _ty _ _ _ -> do
          error "REDO/2"
          -- getSafeDecoderBody ty
        AttributeDefn _ (SingleAttributeDefn _ty _ _) _ _ -> do
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


    -- | Given a list of `Reference`s which should match some present
    -- element, produce the pair of (1) a map from the `Name` of a
    -- source value to a list of TH `Stmt` statements producing these
    -- bound values, and (2) the `Name`s of the bound values.
    makeDecoderLabeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
    makeDecoderLabeling refs = do
      dbgBLabel "- makeDecoderLabeling for " refs
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
              dbgBLabelFn1 "- makeLabeling ==> " srcName thisFn
              makeLabeling bs (thisFn:srcFns) (thisDest:destNames)

    -- | Return an invocation of a safe decoder expected to be defined
    -- elsewhere.
    getSafeDecoderUsageCall ::
      (QName, AttributeUsage) -> XSDQ (BlockMaker Content dt)
    getSafeDecoderUsageCall (qname, usage) = do
      dbgPt $ "getSafeDecoderUsageCall on "
        ++ showQName qname ++ " used " ++ show usage
      base <- indenting $ getSafeDecoderCall qname
      result <- case usage of
        Optional -> return base
        Required ->  do
          tmp <- newName "ifVal"
          core <- justOrThrow (VarE tmp) (LitE $ StringL "Value required")
          return $ \src dest ->
            base src tmp ++ [LetS [ValD (VarP dest) (NormalB core) []]]
        Forbidden -> return $ \_ dest ->
          [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]

      dbgResultSrcDest "  gives " result

-- | Returns a decoder for an entity via the string-values contents of
-- an XSD simple type.
decoderForSimpleType :: QName -> XSDQ (BlockMaker Content dt)
decoderForSimpleType qn = do
  dbgLn "- decoderForSimpleType"
  dbgBLabel "  - qn " qn
  strDec <- getSafeStringDecoder qn
  retrievingCRefFor qn strDec

-- | Return an invocation of a safe decoder expected to be defined
-- elsewhere.
getSafeDecoderCall :: QName -> XSDQ (BlockMaker Content dt)
getSafeDecoderCall qn = do
  dbgPt $ "getSafeDecoderCall for " ++ showQName qn
  indenting $ do
    ifDefn <- getTypeDefn qn
    case ifDefn of
      Just defn -> do
        dbgBLabel "- Found type " defn
        indenting $ case defn of
          BuiltinDefn _ _ _ _ -> decoderForSimpleType qn
          _ -> indenting $ getAttrRefSafeDecoder qn
      _ -> do
        dbgLn "- No type found, building via baseByName"
        indenting $ getAttrRefSafeDecoder qn


-- | Given a list of `Reference`s which should match subelements,
-- produce the pair of (1) a map from the `Name` of a source value to
-- a list of TH `Stmt` statements producing these bound values, and
-- (2) the `Name`s of the bound values.
makeSubexprLabeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
makeSubexprLabeling refs = do
  dbgBLabel "- makeSubexprLabeling for " refs
  indenting $ do
    blockMakers <- mapM referenceToBlockMaker refs
    dbgLn "- blockMakers "
    whenDebugging generate 3 $
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
  dbgPt "Calling labelBlockMakers"
  indenting $
    labelBlockMakers' blockMakers (map (\z -> "s" ++ show z) ([1..] :: [Int]))
                      [] []

  where

    -- | Utility function for @makeSubexprLabeling@ and other decoder
    -- generators for terms with subexpressions.  Given a list of
    -- `Reference`s to subelements, produces two lists: (1) functions
    -- from the `Name` of a source value to a list of TH `Stmt`
    -- statements producing these bound values, and (2) the `Name`s of
    -- the bound values.  The base case joins the list of functions by
    -- assembling a new map from the concatenation of the results of
    -- the individual functions, and reversing the list of names.
    labelBlockMakers' ::
      [BlockMaker st dt] -> [String] -> [Name] -> [Name -> [Stmt]]
      -> XSDQ (Name -> [Stmt], [Name])
    labelBlockMakers' [] _ accNames accFns = do
      dbgLn "- End of labelBlockMakers'"
      return
        ((\src -> concat $ reverse $ map (\x -> x src) accFns),
         reverse accNames)

    labelBlockMakers' (bmk:bmks) (n:ns) accN accF = do
      fresh <- newName n
      dbgBLabel "- labelBlockMakers': " $ bmk (mkName "SRC") fresh
      labelBlockMakers' bmks ns (fresh : accN) ((\src -> bmk src fresh) : accF)

    labelBlockMakers' _ [] _ _ =
      error "Internal error --- end of infinite list"

{-
-- For an `ElementRef` of type @qn@, first calculate the
-- single-element safe-decoder, and then pass it to
-- `makeSubelementBinder` to adjust for the given lower/upper
-- occurrence bounds.
labelBlockMakers' (r@(ElementRef eqn lo hi _):rs) (n:ns) accN accF = do
  dbgBLabel "- labelBlockMakers' for " r
  singleDecoder <- indenting $ getRefSafeDecoder r
  dbgLn $ outBlock $
    labelBlock "  getRefSafeDecoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  f' <- indenting $ makeSubelementBinder eqn singleDecoder lo hi
  dbgLn $ outBlock $
    labelBlock "  makeSubelementBinder gives " $
      stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  labelBlockMakers' rs ns (n : accN) ((\src -> f' src n) : accF)
 -- For an `AttributeRef` to values of type @qn@, first build a
-- single-value decoder, and then adjust for the attrubte
-- usage.
labelBlockMakers' (r@(AttributeRef _ usage):rs) (n:ns) accNs accFns = do
  dbgBLabel "- labelBlockMakers' for " r
  safeDec <- indenting $ getRefSafeDecoder r
  f' <- indenting $ makeUsageBinder safeDec usage
  let res src = f' src n
  dbgBLabel "  - safeDec " $ safeDec (mkName "SRC") (mkName "DEST")
  dbgBLabel "  - f' " $ f' (mkName "SRC") (mkName "DEST")
  dbgBLabel "  - res " $ res (mkName "SRC")
  labelBlockMakers' rs ns (n : accNs) (res : accFns)

-- `TypeRef`s can occur e.g. when ChoiceDefn are lifted out to
-- flatten declarations.
labelBlockMakers' (r@(TypeRef tqn lo hi _ _):rs) (n:ns) accNs accFns = do
  dbgBLabel "- labelBlockMakers' for " r
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- getRefSafeDecoder r
  dbgLn $ outBlock $
    labelBlock "  getRefSafeDecoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  f' <- indenting $ makeSubelementBinder tqn singleDecoder lo hi
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
  dbgLn "- getRefSafeDecoder ElementRef case"
  indenting $ do
    ifTypeOf <- getElementType nam
    case ifTypeOf of
      Nothing -> error $
        "No type stored for element \"" ++ showQName nam ++ "\""
      Just typeOf -> getSafeDecoderCall typeOf

getRefSafeDecoder (AttributeRef ref usage) = do
  dbgLn $
    "- getRefSafeDecoder AttributeRef case: "
    ++ showQName ref ++ " " ++ show usage
  indenting $ do
    coreFn  <- indenting $ getAttrRefSafeDecoder ref
    dbgBLabelSrcDest "  - coreFn " coreFn
    usageFn <- indenting $ unpackAttrDecoderForUsage usage ref
    dbgBLabelSrcDest "  - usageFn " usageFn
    tmp <- newName "attr"
    return $ \src dest -> coreFn src tmp ++ usageFn tmp dest

  where
    unpackAttrDecoderForUsage ::
      AttributeUsage -> QName -> XSDQ (BlockMaker st dt)
    unpackAttrDecoderForUsage Forbidden name = do
      dbgLn $
        "unpackAttrDecoderForUsage Forbidden case for " ++ qName name
      return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
    unpackAttrDecoderForUsage Optional name = do
      dbgLn $ "unpackAttrDecoderForUsage Optional case for " ++ qName name
      return $
        \src dest -> [ LetS [ ValD (VarP dest) (NormalB $ VarE src) [] ] ]
    unpackAttrDecoderForUsage Required name = do
      dbgLn $ "unpackAttrDecoderForUsage Required case for " ++ qName name
      matches <- maybeMatches (throwsError $
                               "QDHXB: got Nothing for required attribute "
                               ++ qName name)
                              VarE
      return $ \src dest ->
        [LetS [ValD (VarP dest) (NormalB $ CaseE (VarE src) matches) []]]

    -- | Builds a list of two `Match`es for a `Maybe` expression,
    -- given the alternative expressions for `Nothing` and `Just` (the
    -- latter parameterized over a `Name`).
    maybeMatches :: Exp -> (Name -> Exp) -> XSDQ [Match]
    maybeMatches zeroCase oneCaseF = do
      newX <- newName "x"
      return $ [
        Match nothingPat (NormalB zeroCase) [],
        Match (justPat newX) (NormalB $ oneCaseF newX) []
        ]


getRefSafeDecoder (TypeRef nam lower upper _ _) = do
  dbgLn "- getRefSafeDecoder TypeRef case"
  indenting $ do
    singleBlockMaker <- getSafeDecoderCall nam
    scaleBlockMakerToBounds singleBlockMaker lower upper

getRefSafeDecoder (GroupRef nam lower upper _ _) = do
  dbgLn $
    "- getRefSafeDecoder GroupRef case for " ++ showQName nam
  indenting $ do
    defn <- getGroupDefn nam
    case defn of
      Just (GroupDefn _ (TypeRef typ _ _ _ _) _ _) -> do
        singleBlockMaker <- getSafeDecoderCall typ
        scaleBlockMakerToBounds singleBlockMaker lower upper
      _ -> do throwError $
                "QDHXB: group reference " ++ showQName nam
                ++ " to non-group definition"

getRefSafeDecoder (RawXML _ _) = do
  dbgLn "* getRefSafeDecoder RawXML"
  return $ \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ]

getAttrRefSafeDecoder :: QName -> XSDQ (BlockMaker Content dt)
getAttrRefSafeDecoder ref = do
  dbgLn "getAttrRefSafeDecoder only case"
  indenting $ do
    typeHName <- getTypeHaskellName ref
    dbgBLabel "  - typeHName " typeHName
    -- TODO --- This isn't (generally) right --- need to look at the
    -- type, and actually extract the attribute.
    let safeDec = VarE $ mkName $ "tryDecodeAs" ++ firstToUpper typeHName
    dbgBLabel "  - safeDec " safeDec
    let result = \src dest -> [ BindS (VarP dest) $ AppE safeDec (VarE src) ]
    dbgBLabelSrcDest "- result " result
    return result


-- | Convert a `Reference` into a `BlockMaker` calculating its value.
referenceToBlockMaker :: Reference -> XSDQ (BlockMaker Content dt)

referenceToBlockMaker r@(ElementRef eqn lo hi _) = do
  dbgBLabel "referenceToBlockMaker for" r
  singleDecoder <- indenting $ getRefSafeDecoder r
  dbgLn $ outBlock $
    labelBlock "  getRefSafeDecoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  ifTqn <- getElementType eqn
  let tqn = maybe (error $ "No element type for " ++ bpp eqn) id ifTqn
  hsType <- getTypeHaskellType tqn
  f' <- refBlockMakerForBounds eqn hsType singleDecoder lo hi
  dbgLn $ outBlock $
    labelBlock "  refBlockMakerForBounds gives " $
      stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

referenceToBlockMaker r@(AttributeRef _ usage) = do
  dbgBLabel "referenceToBlockMaker for" r
  safeDec <- indenting $ getRefSafeDecoder r
  f' <- indenting $ makeUsageBinder safeDec usage
  dbgBLabelSrcDest "  - safeDec " safeDec
  dbgBLabelSrcDest "  - f' " f'
  return f'

referenceToBlockMaker r@(TypeRef tqn lo hi _ _) = do
  dbgBLabel "referenceToBlockMaker for" r
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- getRefSafeDecoder r
  dbgLn $ outBlock $
    labelBlock "  getRefSafeDecoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  hsType <- getTypeHaskellType tqn
  f' <- indenting $ refBlockMakerForBounds tqn hsType singleDecoder lo hi
  dbgLn $ outBlock $
    labelBlock "  refBlockMakerForBounds gives " $
      stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

referenceToBlockMaker r@(GroupRef rqn lo hi _ _) = do
  dbgBLabel "referenceToBlockMaker for" r
  defn <- getGroupDefn rqn
  case defn of
    Just (GroupDefn _ (TypeRef gtyp _ _ _ _) _ _) -> do
      singleBlockMaker <- getSafeDecoderCall gtyp
      scaleBlockMakerToBounds singleBlockMaker lo hi
    _ -> throwError $
           "QDHXB: group reference " ++ showQName rqn
           ++ " to non-group definition"

referenceToBlockMaker r@(RawXML _ _) = do
  dbgBLabel "referenceToBlockMaker for" r
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
  dbgLn "refBlockMakerForBounds' unit case"
  return $ \ _ dest -> [ BindS (VarP dest) $ TupE [] ]

refBlockMakerForBounds' tqn _ puller indivF (Just 1) (Just 1) = do -- Single
  dbgLn "refBlockMakerForBounds' single case"
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
  dbgBLabelSrcDest "- result " result
  return result

refBlockMakerForBounds' _tqn hsType puller indivF _ _ = do             -- List
  dbgLn "refBlockMakerForBounds' list case"
  pull <- newName "pullForList"
  asList <- newName "asList"
  tmp <- newName "postpull"
  a <- newName "a"
  dbgBLabel "- hsType" hsType
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
  dbgLn "makeUsageBinder Forbidden case"
  return $ \_ dest -> [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]
makeUsageBinder singleTrans Optional = do
  dbgLn "makeUsageBinder Optional case"
  return $ \src dest -> singleTrans src dest
makeUsageBinder singleTrans Required = do
  dbgLn "makeUsageBinder Required case"
  return $ \src dest -> singleTrans src dest

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


-- | First in result triple: the `Con` spec for a `DataD` declaration.
-- Second is the constructor as a TH `Exp`.  Third is the `BlockMaker`
-- for the decoder function.
makeChoiceConstructor ::
  QName -> (QName, Reference) -> XSDQ (Con, Exp, BlockMaker Content dt)
makeChoiceConstructor name (constrSuffix, ref) = do
  dbgLn $ "Called makeChoiceConstructor "
    ++ showQName name ++ " (" ++ showQName constrSuffix ++ ", ...)"
  let typeRoot = firstToUpper $ qName name
  case ref of

    ElementRef elName ifMin ifMax _ -> do
      dbgLn $ "- With ElementRef " ++ showQName elName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName elName)
      dbgBLabel "- constr " constrName
      dbgLn $ "- Calling getElementTypeOrFail " ++ showQName elName
      decQName <- getElementTypeOrFail elName
      dbgBLabel "- decQName " decQName
      decType <- getTypeHaskellType decQName
      dbgBLabel "- decType " decType
      useType <- containForBounds ifMin ifMax $ return decType
      dbgBLabel "- useType " useType
      decoderFn <- getTypeDecoderFn decQName
      return (
        NormalC constrName [(useBang, useType)],
        ConE constrName,
        decoderFn)

    GroupRef grName ifMin ifMax _ _ -> do
      dbgLn $ "- With GroupRef " ++ showQName grName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName grName)
      dbgBLabel "- constr " constrName
      groupDefn <- getGroupDefnOrFail grName
      dbgBLabel "- groupDefn " groupDefn
      case groupDefn of
        GroupDefn _ storedRef _ _ -> do
          dbgBLabel "- storedRef " storedRef
          case storedRef of
            TypeRef tyName _tyIfMin _tyIfMax _ _ -> do
              decType <- getTypeHaskellType tyName
              dbgBLabel "- decType " decType
              useType <- containForBounds ifMin ifMax $ return decType
              dbgBLabel "- useType " useType
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
      dbgLn $ "- With TypeRef " ++ showQName tyName
      let constrName = mkName $ typeRoot ++ firstToUpper (qName tyName)
      dbgBLabel "- constr " constrName
      typeDefn <- getTypeDefn tyName
      dbgBLabel "- typeDefn " constrName
      decType <- getTypeHaskellType tyName
      dbgBLabel "- decType " decType
      useType <- containForBounds ifMin ifMax $ return decType
      dbgBLabel "- useType " useType
      decoderFn <- getTypeDecoderFn tyName
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
      dbgLn $ "- With AttributeRef"
      error "Not expected: makeChoiceConstructor for AttributeRef"

    RawXML _ _ -> do
      dbgLn "- With RawXML"
      return $ (
        NormalC contentName [(useBang, contentConT)],
        ConE contentName,
        \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ])


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
