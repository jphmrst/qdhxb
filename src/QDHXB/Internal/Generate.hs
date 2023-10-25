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
import Language.Haskell.TH
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, qName)
import QDHXB.Utils.TH
import QDHXB.Utils.XMLLight
import QDHXB.Utils.BPP
import QDHXB.Utils.Misc (ifAtLine)
import QDHXB.Internal.Types
import QDHXB.Internal.Block
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Generate.Assembly
import QDHXB.Internal.Generate.Decoders
import QDHXB.Internal.Generate.Types

import QDHXB.Internal.Debugln hiding (
  dbgLn, dbgPt, dbgBLabel, dbgBLabelFn1, dbgBLabelFn2,
  dbgResult, dbgResultFn2, dbgResultM)
import qualified QDHXB.Internal.Debugln as DBG
dbgLn :: (MonadDebugln m n) => String -> m ()
dbgLn = DBG.dbgLn generate 0
-- dbgPt :: (MonadDebugln m n) => String -> m ()
-- dbgPt = DBG.dbgPt generate 0
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
dbgResultM :: (MonadDebugln m n, Blockable a) => String -> m a -> m a
dbgResultM = DBG.dbgResultM generate 0
-- dbgResultFn2 ::
--   (MonadDebugln m n, Blockable r) =>
--     String -> a -> b -> (a -> b -> r) -> m (a -> b -> r)
-- dbgResultFn2 = DBG.dbgResultFn2 generate 0
dbgBLabelSrcDest ::
  Blockable c => String -> (Name -> Name -> c) -> XSDQ ()
{-# INLINE dbgBLabelSrcDest #-}
dbgBLabelSrcDest msg = dbgBLabelFn2 msg srcName destName
-- dbgResultSrcDest ::
--   Blockable c => String -> (Name -> Name -> c) -> XSDQ (Name -> Name -> c)
-- {-# INLINE dbgResultSrcDest #-}
-- dbgResultSrcDest msg = dbgResultFn2 msg srcName destName


-- | Translate a list of XSD definitions to top-level Haskell
-- declarations in the Template Haskell quotation monad.
xsdDeclsToHaskell :: [Definition] -> XSDQ [Dec]
xsdDeclsToHaskell defns = do
  dbgLn "Generating Haskell declarations from definitions"
  dbgResultM "Declarations:" $
    -- Translate each declaration individually, and then concatenate
    -- the results together.
    fmap concat $ indenting $ mapM xsdDeclToHaskell defns


-- | Translate one XSD definition to a list of top-level Haskell
-- declarations in the Template Haskell quotation monad, and record
-- the associated Haddock documentation.
xsdDeclToHaskell :: Definition -> XSDQ [Dec]
xsdDeclToHaskell decl@(ElementDefn nam typ implName ln ifDoc) = do
  dbgBLabel ("Generating from (e" ++ ifAtLine ln ++ ") ") decl
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
    pushDeclHaddock ifDoc extractSubElemsNam
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


xsdDeclToHaskell d@(AttributeDefn nam (AttributeGroupDefn ads _hn) ln doc) = do
  dbgLn $ "Generating from (f" ++ ifAtLine ln ++ ") "
  dbgBLabel "  " d
  decoder <- getSafeDecoderBody nam
  dbgBLabelSrcDest "- decoder " decoder
  dbgLn "getAttributeOrGroupTypeForUsage on each AttributeGroupDefn item:"
  hrefOut <- indenting $ mapM (getAttributeOrGroupTypeForUsage ln) ads
  dbgBLabel "- hrefOut " hrefOut
  dbgResultM "Generated" $ indenting $
    assembleDecs nam (Just $ \tn ->
                        DataD [] tn [] Nothing [
                          NormalC tn $ map (\x -> (useBang, x)) hrefOut
                          ]
                          [DerivClause Nothing [eqConT, showConT]])
                    decoder doc


xsdDeclToHaskell d@(AttributeDefn nam (SingleAttributeDefn typ _ hnam)
                                  ln ifd) = do
  dbgBLabel ("Generating from (g" ++ ifAtLine ln ++ ") ") d
  dbgBLabel "typ " typ
  let xmlName = hnam -- qName nam
      origRoot = firstToUpper xmlName
  dbgBLabel "xmlName " xmlName
  rootName <- applyTypeRenames origRoot
  dbgBLabel "rootName " rootName
  let rootTypeName = mkName $ rootName -- ++ "AttrType"
      decNam = mkName $ "decode" ++ rootName
      safeDecNam = mkName $ prefixCoreName "tryDecodeAs" rootName
  dbgBLabel "rootName " rootName
  dbgBLabel "rootTypeName " rootTypeName

  -- TODO Much of this is in getSafeDecoderBody --- prune out duplication

  paramName <- newName "content"
  attrName <- newName "attr"
  puller <- [| pullAttrFrom $(return $ quoteStr $ qName nam)
                            $(return $ VarE paramName) |]
  -- let (haskellTyp, basicDecoder) = xsdNameToTypeTranslation $ qName typ
  -- let coreDecoder = basicDecoder $ VarE xName
  haskellTyp <- getTypeHaskellType typ
  dbgBLabel "haskellTyp " haskellTyp
  coreDecoder <- getSafeStringDecoder typ
  dbgBLabelSrcDest "coreDecoder " coreDecoder
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
  dbgBLabel "decoder " decoder

  pushDeclHaddock ifd safeDecNam $
    "Attempt to decode the @" ++ showQName nam
    ++ "@ attribute as a `" ++ show rootTypeName
    ++ "`, throwing a `QDHXB.Errs.HXBErr` in the `Control.Monad.Except`"
    ++ " monad if extraction fails"
  pushDeclHaddock ifd decNam $
    "Decode the @" ++ showQName nam ++ "@ attribute as a `"
    ++ show rootTypeName ++ "`"
  pushDeclHaddock ifd rootTypeName $
    "Representation of the @" ++ showQName nam ++ "@ attribute"

  decBody <- resultOrThrow $ AppE (VarE safeDecNam) (VarE paramName)
  dbgBLabel "decBody " decBody
  let typeDef = TySynD rootTypeName [] haskellTyp
  dbgBLabel "typeDef " typeDef
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


xsdDeclToHaskell decl@(SimpleSynonymDefn nam typ ln ifDoc) = do
  dbgBLabel ("Generating from (a" ++ ifAtLine ln ++ ") ") decl
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType typ
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderCall typ
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    assembleDecs nam (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc

xsdDeclToHaskell decl@(ComplexSynonymDefn nam typ ln ifDoc) = do
  dbgBLabel ("Generating from (b" ++ ifAtLine ln ++ ") ") decl
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType typ
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderCall typ
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    assembleDecs nam (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc

xsdDeclToHaskell decl@(UnionDefn name pairs ln ifDoc) = do
  dbgBLabel ("Generating from (c" ++ ifAtLine ln ++ ") UnionDefn ") decl

  (safeCore, _, whereDecs) <- do
    dbgLn "- Calling unionDefnComponents"
    indenting $ unionDefnComponents getSafeDecoderCall name pairs ln
  whenDebugging generate 3 $ do
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
  dbgBLabelFn1 "- typDef " (mkName "NAME") typDef

  dbgResultM "Generated" $
    assembleDecs name (Just typDef)
                (\src dest -> [
                    BindS (VarP dest) $
                      LetE (concat $ map (\f -> f src) whereDecs) safeCore
                    ])
                ifDoc


xsdDeclToHaskell decl@(ListDefn name elemTypeQName ln ifDoc) = do
  dbgBLabel ("Generating from (d" ++ ifAtLine ln ++ ") ") decl
  -- error "REDO/d"
  elemTypeName <- getTypeHaskellName elemTypeQName
  let typDef tn = TySynD tn [] $ AppT ListT $ ConT $ mkName elemTypeName
  dec <- decoderForSimpleType name
  dbgResultM "Generated" $ assembleDecs name (Just typDef) dec ifDoc
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
    assembleDecs name (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc
  -}

xsdDeclToHaskell decl@(SequenceDefn nam refs ln ifDoc) = do
  dbgBLabel ("Generating from (h" ++ ifAtLine ln ++ ") ") decl
  decoder <- getSafeDecoderBody nam
  dbgBLabelSrcDest "- decoder " decoder
  hrefOut <- indenting $ mapM xsdRefToBangTypeQ refs
  dbgBLabel "- field types " hrefOut
  dbgResultM "Generated" $
    assembleDecs nam (Just $ \tn ->
                        DataD [] tn [] Nothing [NormalC tn $ hrefOut]
                              [DerivClause Nothing [eqConT, showConT]])
                    decoder ifDoc


xsdDeclToHaskell decl@(ExtensionDefn qn base refs ln doc) = do
  dbgBLabel ("Generating from (i" ++ ifAtLine ln ++ ") ") decl
  decoder <- getSafeDecoderBody qn
  hrefOut <- mapM xsdRefToBangTypeQ $ base : refs
  let typDef tn = DataD [] tn [] Nothing [NormalC tn $ hrefOut]
                    [DerivClause Nothing [eqConT, showConT]]
  dbgResultM "Generated" $
    assembleDecs qn (Just typDef) decoder doc

xsdDeclToHaskell decl@(GroupDefn _qn (TypeRef _tqn _ _ _ _) ln _ifDoc) = do
  dbgBLabel ("Generating from (j" ++ ifAtLine ln ++ ") ") decl
  throwError
    "Should not encounter GroupDefn in flattened code, for XSDQ state only"
  {-
  -- Get the Haskell type name of the base type
  haskellType <- getTypeHaskellType tqn
  dbgBLabel "- haskellType " haskellType
  -- Make the safe decoder
  decoder <- indenting $ getSafeDecoderBody qn
  -- Assemble the various declarations from the Haskell type synonym
  -- declaration, and the safe decoder steps transformer.
  dbgResultM "Generated" $
    assembleDecs qn (Just $ \tn -> TySynD tn [] haskellType) decoder ifDoc
  -}


xsdDeclToHaskell (ChoiceDefn name fields ln ifDoc) = do
  dbgLn $ "Generating from (k" ++ ifAtLine ln ++ ")"
      ++ ifAtLine ln ++ " on ChoiceDefn " ++ showQName name
  dbgBLabel
      ("- Calling mapM (makeChoiceConstructor " ++ showQName name ++ ") on ")
      fields
  (constrDefs, _, _) <- indenting $
    fmap unzip3 $ mapM (makeChoiceConstructor name) fields
  dbgBLabel "- constrDefs " constrDefs
  let dataDef tn = DataD [] tn [] Nothing constrDefs
                     [DerivClause Nothing [eqConT, showConT]]
  dbgBLabelFn1 "- dataDef " (mkName "NAME") dataDef
  decoder <- getSafeDecoderBody name
  dbgBLabelSrcDest "- decoder " decoder
  dbgResultM "Generated" $ assembleDecs name (Just dataDef) decoder ifDoc

xsdDeclToHaskell decl = do
  boxed $ do
    dbgLn "Uncaught case in xsdDeclToHaskell"
    dbgBLabel "DECL " decl
  error "Uncaught case in xsdDeclToHaskell"
