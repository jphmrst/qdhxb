
{-| Helpers for assembling declarations. -}

module QDHXB.Internal.Generate.Assembly (assembleDecs, pushDeclHaddock) where

import Control.Monad.Except
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer)
import Text.XML.Light.Output (showQName)
import Text.XML.Light.Types (QName, Content)
import QDHXB.Utils.BPP
import QDHXB.Utils.TH
import QDHXB.Internal.XSDQ
import QDHXB.Internal.Block
import QDHXB.Internal.Types
import QDHXB.Internal.Generate.Types

import QDHXB.Internal.Debugln hiding (
  dbgLn, dbgPt, dbgBLabel, dbgBLabelFn1, dbgBLabelFn2,
  dbgResult, dbgResultFn2, dbgResultM)
import qualified QDHXB.Internal.Debugln as DBG
dbgBLabel :: (MonadDebugln m n, Blockable c) => String -> c -> m ()
dbgBLabel = DBG.dbgBLabel generate 0
{-
dbgLn :: (MonadDebugln m n) => String -> m ()
dbgLn = DBG.dbgLn generate 0
dbgPt :: (MonadDebugln m n) => String -> m ()
dbgPt = DBG.dbgPt generate 0
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
-}


-- | Given various components of the Haskell translation of a single,
-- flattened XSD declaration, assemble the corresponding TH `Dec`s.
assembleDecs ::
  QName -> Maybe (Name -> Dec) -> BlockMaker Content dt -> Maybe String
  -> XSDQ [Dec]
assembleDecs base tyDec safeDec ifDoc = do
  dbgBLabel "assembleDecs with " base

  -- TODO --- Update the type extraction here.  Write a
  -- "decodersReturnType base" function that checks the usage in the
  -- declaration/assumes Optional for groups for now.

  ifDecoderType <- indenting $ decodersReturnType base
  decoderType <- case ifDecoderType of
    Nothing -> throwError $ "No return type for " ++ showQName base
    Just x -> return x
  dbgBLabel "- decoderType " decoderType

  baseNameStr <- getTypeHaskellName base
  let typeName = mkName baseNameStr
      safeDecAsNam = mkName $ prefixCoreName "tryDecodeAs" baseNameStr
      decAsNam = mkName $ "decodeAs" ++ baseNameStr
      tryDecType = fn1Type contentConT (qHXBExcT decoderType)
      decType = fn1Type contentConT decoderType
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
  dbgBLabel "- decodeBody " decodeBody
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


-- | Calculate the Haskell type corresponding to a bound QName.
-- Checks the usage in the declaration/assumes Optional for groups for
-- now.
decodersReturnType :: QName -> XSDQ (Maybe Type)
decodersReturnType qn = indenting $ do
  dbgBLabel "decodersReturnType for " qn
  ifDefn <- getTypeDefn qn
  case ifDefn of

    Just defn -> do
      dbgBLabel "- Found type " defn
      fmap Just $ getTypeHaskellType qn

    Nothing -> do
      ifGroupDefn <- getAttributeGroup qn
      case ifGroupDefn of
        Just defn -> do
          dbgBLabel "- Found attribute group " defn
          case defn of
            SingleAttributeDefn _ _ _ -> throwError $
              "Expected AttributeGroupDefn but found SingleAttributeDefn"
            AttributeGroupDefn _ _ -> do
              -- Groups are always Optional for now, so wrap the base
              -- type in `Maybe`.
              fmap Just $ fmap appMaybeType $ buildAttrOrGroupHaskellType qn

        Nothing -> do
          ifSingleDefn <- getAttributeDefn qn
          case ifSingleDefn of
            Just defn -> do
              dbgBLabel "- Found single attribute " defn
              case defn of
                AttributeGroupDefn _ _ -> throwError $
                  "Expected SingleAttributeDefn but found AttributeGroupDefn"
                SingleAttributeDefn _ usage _ -> do
                  -- Groups are always Optional for now, so wrap the
                  -- base type in `Just`.
                  fmap Just $ fmap (attrTypeForUsage usage) $
                    buildAttrOrGroupHaskellType qn

            Nothing -> liftExcepttoXSDQ $ throwError $
              "No type or attribute/group " ++ bpp qn ++ " found"


-- | Install Haddock documentation for the top-level declaration
-- associated with the given name.
pushDeclHaddock ::
  Maybe String -- ^ If present, is separated from the leading
               -- documentation with a colon.  If absent, a full
               -- stop/period is appended to the leading
               -- documentation.
  -> Name
  -> String -- ^ The leading documentation for the declaration.
  -> XSDQ ()
pushDeclHaddock ifDoc = do
  pushDeclHaddock' $ maybe "." (": " ++) ifDoc

pushDeclHaddock' :: String -> Name -> String -> XSDQ ()
pushDeclHaddock' suffix name spec = do
  liftQtoXSDQ $ addModFinalizer $ putDoc (DeclDoc name) $ spec ++ suffix
