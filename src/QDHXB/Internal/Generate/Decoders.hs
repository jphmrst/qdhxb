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
import QDHXB.Utils.Misc (ifAtLine)
import QDHXB.Internal.Types
import QDHXB.Internal.Block
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate.Types

import QDHXB.Internal.Debugln (MonadDebugln, indenting, whenDebugging, generate)
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
dbgResult :: (MonadDebugln m n, Blockable a) => String -> a -> m a
dbgResult = DBG.dbgResult generate 0
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
    decl <- retrieveDeclaration qn
    case decl of
      IsElementType qn' -> getSafeDecoderCall qn'
      IsAttributeType (SingleAttributeDefn typeQN usage _base) -> do
        dbgLn "Relay to single_attribute_decoder at (1a)"
        indenting $ single_attribute_decoder qn typeQN usage
        {-
        safeDecodingBlockMakerByName qn
        -}
      IsAttributeGroup (AttributeGroupDefn _elems _base) -> do
        dbgLn "* safeDecodingBlockMakerByName via (1b)"
        safeDecodingBlockMakerByName qn
      IsTypeDefinition defn -> do
        dbgLn "Dispatcher (2)"
        dispatchDefinition defn
      IsGroupDefinition defn -> do
        dbgLn "Dispatcher (3)"
        dispatchDefinition defn
      NotDefinedInXSDQ -> do
        dbgLn "* safeDecodingBlockMakerByName via (1c)"
        safeDecodingBlockMakerByName qn
      _ -> error "Internal error --- attribute type/group mismatch"

  where
    dispatchDefinition defn = indenting $ case defn of
      BuiltinDefn _ _ _ _ -> decoderForSimpleType qn
      ElementDefn _ _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (a)"
        safeDecodingBlockMakerByName qn
      AttributeDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (b)"
        safeDecodingBlockMakerByName qn
      SimpleSynonymDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (c)"
        safeDecodingBlockMakerByName qn
      ComplexSynonymDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (d)"
        safeDecodingBlockMakerByName qn
      SequenceDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (e)"
        safeDecodingBlockMakerByName qn
      UnionDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (f)"
        safeDecodingBlockMakerByName qn
      ChoiceDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (g)"
        safeDecodingBlockMakerByName qn
      ExtensionDefn _ _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (h)"
        safeDecodingBlockMakerByName qn
      GroupDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (i)"
        safeDecodingBlockMakerByName qn
      ListDefn _ _ _ _ -> indenting $ do
        dbgLn "* safeDecodingBlockMakerByName via (j)"
        safeDecodingBlockMakerByName qn

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
    IsTypeDefinition d -> decoder_body_for_type_defn d
    IsAttributeGroup ad -> decoder_body_for_attribute_group_defn ad
    IsAttributeType ad -> decoder_body_for_single_attribute_defn ad
    IsGroupDefinition _ -> liftExcepttoXSDQ $ throwError $
      "getSafeDecoderBody received unexpected GroupDefinition for " ++ bpp qn
    IsElementType et -> liftExcepttoXSDQ $ throwError $
      "getSafeDecoderBody received unexpected Element type " ++ bpp et
       ++ " for " ++ bpp qn

  where

    decoder_body_for_single_attribute_defn attrDefn = case attrDefn of

      SingleAttributeDefn typ usage _hn ->
        single_attribute_decoder qn typ usage

      AttributeGroupDefn _ _ -> do
        throwError $ "Found AttributeGroupDefn in single attribute table for "
          ++ qName qn

    decoder_body_for_attribute_group_defn agd = case agd of
      AttributeGroupDefn subqns _ -> do
          dbgLn "- AttributeGroupDefn (2aa) found"
          typeAndConstrName <- fmap mkName $ buildAttrOrGroupHaskellName qn
          dbgBLabel "- typeAndConstrName " typeAndConstrName
          haskellType <- buildAttrOrGroupHaskellType qn
          dbgBLabel "- haskellType " haskellType
          dbgPt "Mapping get_safe_decoder_usage_call onto group items"
          subdecoders <- indenting $ mapM get_safe_decoder_usage_call subqns
          dbgLn "- subdecoders"
          whenDebugging generate 3 $ indenting $ forM_ subdecoders $ \sd ->
              dbgBLabel "- " $ sd (mkName "SRC") (mkName "DEST")
          (subBinder, subNames) <- label_block_makers subdecoders
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
                      []]]
          dbgBLabelSrcDest "- result " res
          return res

      SingleAttributeDefn typ _usage _hn -> do
          throwError $
            "Found (2bb) SingleAttributeDefn in attribute group table for "
            ++ qName typ


    decoder_body_for_type_defn defn = do
      dbgBLabel "- Found type " defn
      case defn of
        BuiltinDefn ty _ _ _ -> do
          dbgBLabel "- Basic type (aa) " ty
          dbgPt "Relay to decoderForSimpleType"
          decoderForSimpleType qn

        SequenceDefn nam refs ln _doc -> do
          dbgPt $ "Sequence case (bb)" ++ ifAtLine ln
          (bindingsF, boundNames) <- indenting $ make_subexpr_labeling refs
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

        ExtensionDefn edqn base refs ln _doc -> do
          dbgBLabel "- Using getSafeDecoderBody (dd) with Extension for " edqn
          indenting $ do
            dbgBLabel "- base " base
            dbgBLabel "- refs " refs
            dbgLn $ "- ln " ++ show ln
            hrefOut <- mapM xsdRefToBangTypeQ $ base : refs
            dbgBLabel "- hrefOut " hrefOut

            dbgLn "- Calling make_subexpr_labeling"
            (bindingsF, boundNames) <- indenting $
              make_decoder_labeling $ base:refs
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
          get_safe_decoder_usage_call (nam, Required)
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
    make_decoder_labeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
    make_decoder_labeling refs = do
      dbgBLabel "- make_decoder_labeling for " refs
      indenting $ do
        decoders <- mapM get_ref_safe_decoder refs
        make_labeling decoders [] []

      where make_labeling ::
              [BlockMaker Content dt] -> [Name -> [Stmt]] -> [Name]
                -> XSDQ (Name -> [Stmt], [Name])
            make_labeling [] srcFns destNames =
              return ((\src -> concat $ reverse $ map (\z -> z src) srcFns),
                      reverse destNames)
            make_labeling (b:bs) srcFns destNames = do
              thisDest <- newName "sub"
              let thisFn = (\src -> b src thisDest)
              dbgBLabelFn1 "- make_labeling ==> " srcName thisFn
              make_labeling bs (thisFn:srcFns) (thisDest:destNames)

    -- | Return an invocation of a safe decoder expected to be defined
    -- elsewhere.
    get_safe_decoder_usage_call ::
      (QName, AttributeUsage) -> XSDQ (BlockMaker Content dt)
    get_safe_decoder_usage_call (qname, usage) = do
      dbgPt $ "get_safe_decoder_usage_call on "
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


-- | Given a list of `Reference`s which should match subelements,
-- produce the pair of (1) a map from the `Name` of a source value to
-- a list of TH `Stmt` statements producing these bound values, and
-- (2) the `Name`s of the bound values.
make_subexpr_labeling :: [Reference] -> XSDQ (Name -> [Stmt], [Name])
make_subexpr_labeling refs = do
  dbgBLabel "- make_subexpr_labeling for " refs
  indenting $ do
    blockMakers <- mapM reference_to_block_maker refs
    dbgLn "- blockMakers "
    whenDebugging generate 3 $
      forM_ blockMakers $ \b -> dbgBLabelSrcDest "  . " b
    label_block_makers blockMakers

-- | Utility function for @make_subexpr_labeling@ and other decoder
-- generators for terms with subexpressions.  Given a list of
-- `Reference`s to subelements, produces a pair of two values: (1) a
-- function from the `Name` of a source value to a list of TH `Stmt`
-- statements bindings names to the subexpressions, and (2) the
-- `Name`s of the bound values.  The base case joins the list of
-- functions by assembling a new map from the concatenation of the
-- results of the individual functions, and reversing the list of
-- names.
label_block_makers :: [BlockMaker st dt] -> XSDQ (Name -> [Stmt], [Name])
label_block_makers blockMakers = do
  dbgPt "Calling label_block_makers"
  indenting $
    label_block_makers' blockMakers (map (\z -> "s" ++ show z) ([1..] :: [Int]))
                        [] []

  where

    -- | Utility function for @make_subexpr_labeling@ and other
    -- decoder generators for terms with subexpressions.  Given a list
    -- of `Reference`s to subelements, produces two lists: (1)
    -- functions from the `Name` of a source value to a list of TH
    -- `Stmt` statements producing these bound values, and (2) the
    -- `Name`s of the bound values.  The base case joins the list of
    -- functions by assembling a new map from the concatenation of the
    -- results of the individual functions, and reversing the list of
    -- names.
    label_block_makers' ::
      [BlockMaker st dt] -> [String] -> [Name] -> [Name -> [Stmt]]
      -> XSDQ (Name -> [Stmt], [Name])
    label_block_makers' [] _ accNames accFns = do
      dbgLn "- End of label_block_makers'"
      return
        ((\src -> concat $ reverse $ map (\x -> x src) accFns),
         reverse accNames)

    label_block_makers' (bmk:bmks) (n:ns) accN accF = do
      fresh <- newName n
      dbgBLabel "- label_block_makers': " $ bmk (mkName "SRC") fresh
      label_block_makers' bmks ns (fresh : accN) $
        (\src -> bmk src fresh) : accF

    label_block_makers' _ [] _ _ =
      error "Internal error --- end of infinite list"

{-
-- For an `ElementRef` of type @qn@, first calculate the
-- single-element safe-decoder, and then pass it to
-- `makeSubelementBinder` to adjust for the given lower/upper
-- occurrence bounds.
labelBlockMakers' (r@(ElementRef eqn lo hi _):rs) (n:ns) accN accF = do
  dbgBLabel "- labelBlockMakers' for " r
  singleDecoder <- indenting $ get_ref_safe_decoder r
  dbgLn $ outBlock $
    labelBlock "  get_ref_safe_decoder gives " $
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
  safeDec <- indenting $ get_ref_safe_decoder r
  f' <- indenting $ make_usage_binder safeDec usage
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
                            -- get_ref_safe_decoder r
  dbgLn $ outBlock $
    labelBlock "  get_ref_safe_decoder gives " $
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
get_ref_safe_decoder :: Reference -> XSDQ (BlockMaker Content dt)

get_ref_safe_decoder (ElementRef nam _lower _upper _) = do
  dbgLn "- get_ref_safe_decoder ElementRef case"
  indenting $ do
    ifTypeOf <- getElementType nam
    case ifTypeOf of
      Nothing -> error $
        "No type stored for element \"" ++ showQName nam ++ "\""
      Just typeOf -> getSafeDecoderCall typeOf

get_ref_safe_decoder (AttributeRef ref usage) = do
  dbgLn $
    "- get_ref_safe_decoder AttributeRef case: "
    ++ showQName ref ++ " " ++ show usage
  indenting $ do
    coreFn  <- indenting $ safeDecodingBlockMakerByName ref
    dbgBLabelSrcDest "  - coreFn " coreFn
    usageFn <- indenting $ unpack_attr_decoder_for_usage usage ref
    dbgBLabelSrcDest "  - usageFn " usageFn
    tmp <- newName "attr"
    return $ \src dest -> coreFn src tmp ++ usageFn tmp dest

  where
    unpack_attr_decoder_for_usage ::
      AttributeUsage -> QName -> XSDQ (BlockMaker st dt)
    unpack_attr_decoder_for_usage Forbidden name = do
      dbgLn $
        "unpack_attr_decoder_for_usage Forbidden case for " ++ qName name
      return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
    unpack_attr_decoder_for_usage Optional name = do
      dbgLn $ "unpack_attr_decoder_for_usage Optional case for " ++ qName name
      return $
        \src dest -> [ LetS [ ValD (VarP dest) (NormalB $ VarE src) [] ] ]
    unpack_attr_decoder_for_usage Required name = do
      dbgLn $ "unpack_attr_decoder_for_usage Required case for " ++ qName name
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


get_ref_safe_decoder (TypeRef nam lower upper _ _) = do
  dbgLn "- get_ref_safe_decoder TypeRef case"
  indenting $ do
    singleBlockMaker <- getSafeDecoderCall nam
    scaleBlockMakerToBounds singleBlockMaker lower upper

get_ref_safe_decoder (GroupRef nam lower upper _ _) = do
  dbgLn $
    "- get_ref_safe_decoder GroupRef case for " ++ showQName nam
  indenting $ do
    defn <- getGroupDefn nam
    case defn of
      Just (GroupDefn _ (TypeRef typ _ _ _ _) _ _) -> do
        singleBlockMaker <- getSafeDecoderCall typ
        scaleBlockMakerToBounds singleBlockMaker lower upper
      _ -> do throwError $
                "QDHXB: group reference " ++ showQName nam
                ++ " to non-group definition"

get_ref_safe_decoder (RawXML _ _) = do
  dbgLn "* get_ref_safe_decoder RawXML"
  return $ \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ]

safeDecodingBlockMakerByName :: QName -> XSDQ (BlockMaker Content dt)
safeDecodingBlockMakerByName ref = do
  dbgLn "safeDecodingBlockMakerByName only case"
  indenting $ do
    -- TODO --- This isn't (generally) right --- need to look at the
    -- type, and actually extract the attribute.  But the fix has been
    -- to catch incorrect uses higher in the AST of the XSD file than
    -- where we call this function.
    safeDec <- fmap VarE $ get_safe_decoder_fn_by_name ref
    dbgBLabel "  - safeDec " safeDec
    let result = \src dest -> [ BindS (VarP dest) $ AppE safeDec (VarE src) ]
    dbgBLabelSrcDest "- result " result
    return result

single_attribute_decoder ::
  QName -> QName -> AttributeUsage -> XSDQ (BlockMaker Content dt)
single_attribute_decoder name typ usage = do
  dbgPt $
    "single_attribute_decoder for " ++ showQName name ++ " :: " ++ showQName typ
  indenting $ do
    coreDecoder <- getSafeStringDecoder typ
    dbgBLabelSrcDest "- coreDecoder " coreDecoder
    attrDecoder <- stringBlockToAttributeBlock coreDecoder name
    dbgBLabelSrcDest "- attrDecoder " attrDecoder
    finalDecoder <- indenting $ make_usage_binder attrDecoder usage
    dbgBLabelSrcDest "finalDecoder " finalDecoder
    return finalDecoder

-- | Returns a safe-decoder function `Name`, considering only the text
-- of the given qualified name.  It is entirely the duty of the
-- calling function to make sure that the function is generated.
get_safe_decoder_fn_by_name :: QName -> XSDQ Name
get_safe_decoder_fn_by_name qn = do
  dbgPt $ "get_safe_decoder_fn_by_name for " ++ showQName qn
  indenting $ do
    typeHName <- getTypeHaskellName qn
    dbgResult "Built name " $ mkName $ "tryDecodeAs" ++ firstToUpper typeHName


-- | Convert a `Reference` into a `BlockMaker` calculating its value.
reference_to_block_maker :: Reference -> XSDQ (BlockMaker Content dt)

reference_to_block_maker r@(ElementRef eqn lo hi _) = do
  dbgBLabel "reference_to_block_maker for" r
  singleDecoder <- indenting $ get_ref_safe_decoder r
  dbgLn $ outBlock $
    labelBlock "  get_ref_safe_decoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  ifTqn <- getElementType eqn
  let tqn = maybe (error $ "No element type for " ++ bpp eqn) id ifTqn
  hsType <- getTypeHaskellType tqn
  f' <- ref_block_maker_for_bounds eqn hsType singleDecoder lo hi
  dbgLn $ outBlock $
    labelBlock "  ref_block_maker_for_bounds gives " $
      stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

reference_to_block_maker r@(AttributeRef _ usage) = do
  dbgBLabel "reference_to_block_maker for" r
  safeDec <- indenting $ get_ref_safe_decoder r
  dbgBLabelSrcDest "  - safeDec " safeDec
  f' <- indenting $ make_usage_binder safeDec usage
  dbgBLabelSrcDest "  - f' " f'
  return f'

reference_to_block_maker r@(TypeRef tqn lo hi _ _) = do
  dbgBLabel "reference_to_block_maker for" r
  singleDecoder <- indenting $ getTypeDecoderFn tqn
                            -- get_ref_safe_decoder r
  dbgLn $ outBlock $
    labelBlock "  get_ref_safe_decoder gives " $
      stringToBlock $
        pprint $ singleDecoder (mkName "SRC") (mkName "DEST")
  hsType <- getTypeHaskellType tqn
  f' <- indenting $ ref_block_maker_for_bounds tqn hsType singleDecoder lo hi
  dbgLn $ outBlock $
    labelBlock "  ref_block_maker_for_bounds gives " $
      stringToBlock $ pprint $ f' (mkName "SRC") (mkName "DEST")
  return f'

reference_to_block_maker r@(GroupRef rqn lo hi _ _) = do
  dbgBLabel "reference_to_block_maker for" r
  defn <- getGroupDefn rqn
  case defn of
    Just (GroupDefn _ (TypeRef gtyp _ _ _ _) _ _) -> do
      singleBlockMaker <- getSafeDecoderCall gtyp
      scaleBlockMakerToBounds singleBlockMaker lo hi
    _ -> throwError $
           "QDHXB: group reference " ++ showQName rqn
           ++ " to non-group definition"

reference_to_block_maker r@(RawXML _ _) = do
  dbgBLabel "reference_to_block_maker for" r
  return $ \src dest -> [ LetS [ValD (VarP dest) (NormalB $ VarE src) [] ] ]

ref_block_maker_for_bounds ::
  QName -> Type -> BlockMaker Content dt -> Maybe Int -> Maybe Int
  -> XSDQ (BlockMaker Content dt')
ref_block_maker_for_bounds sqn hsTyp indivF lo hi = indenting $
  ref_block_maker_for_bounds' sqn hsTyp (applyPullContentFrom $ qName sqn)
                          indivF lo hi

ref_block_maker_for_bounds' ::
  QName -> Type -> (Exp -> Exp) -> (BlockMaker Content dt)
  -> Maybe Int -> Maybe Int
  -> XSDQ (BlockMaker Content dt')
ref_block_maker_for_bounds' _ _ _ _ _ (Just 0) = do                  -- Unit
  dbgLn "ref_block_maker_for_bounds' unit case"
  return $ \ _ dest -> [ BindS (VarP dest) $ TupE [] ]

ref_block_maker_for_bounds' tqn _ puller indivF (Just 1) (Just 1) = do -- Single
  dbgLn "ref_block_maker_for_bounds' single case"
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

ref_block_maker_for_bounds' _ _ puller indivF _ (Just 1) = do        -- Maybe
  dbgLn "ref_block_maker_for_bounds' maybe case"
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

ref_block_maker_for_bounds' _tqn hsType puller indivF _ _ = do             -- List
  dbgLn "ref_block_maker_for_bounds' list case"
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


make_usage_binder ::
  BlockMaker st dt -> AttributeUsage -> XSDQ (BlockMaker st dt)
make_usage_binder _ Forbidden = do
  dbgLn "make_usage_binder Forbidden case"
  return $ \_ dest -> [ LetS [ValD (VarP dest) (NormalB $ TupE []) []] ]
make_usage_binder singleTrans Optional = do
  dbgLn "make_usage_binder Optional case"
  return $ \src dest -> singleTrans src dest
make_usage_binder singleTrans Required = do
  dbgLn "make_usage_binder Required case"
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
  let where_decoder_pair :: (QName, QName) -> XSDQ (Name, Name -> [Dec])
      where_decoder_pair (constr, typ) = do
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

  whereDecoderPairs <- mapM where_decoder_pair pairs
  let (names, decs) = unzip whereDecoderPairs

  let safeCore :: Exp
      safeCore =
        foldr (\n e -> applyCatchErrorExp (VarE n) (LamE [WildP] e))
              (qthNoValidContentInUnion baseName ln) names

  return (safeCore, names, decs)
