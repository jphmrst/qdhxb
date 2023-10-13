{-# LANGUAGE TemplateHaskell #-}

{-| Encoding `QDHXB.Internal.Types.Reference`s and
  `QDHXB.Internal.Types.AttributeUsage` records as Haskell `Type`s.
-}

module QDHXB.Internal.Generate.Types (xsdRefToBangTypeQ, attrTypeForUsage) where

import Control.Monad.Except
-- import Control.Monad.Extra (whenJust)
import Language.Haskell.TH
import Text.XML.Light.Output (showQName)
import QDHXB.Utils.TH
import QDHXB.Utils.BPP
-- import QDHXB.Utils.Misc (ifAtLine)
import QDHXB.Internal.Types
import QDHXB.Internal.XSDQ

import QDHXB.Internal.Debugln hiding (
  dbgLn, dbgPt, dbgBLabel, dbgBLabelFn1, dbgBLabelFn2,
  dbgResult, dbgResultFn2, dbgResultM)
import qualified QDHXB.Internal.Debugln as DBG
dbgBLabel :: (MonadDebugln m n, Blockable c) => String -> c -> m ()
dbgBLabel = DBG.dbgBLabel generate 0

-- | Translate a reference to an XSD element type to a Template Haskell
-- quotation monad returning a type.
xsdRefToBangTypeQ :: Reference -> XSDQ BangType

xsdRefToBangTypeQ (ElementRef ref lower upper _ln) = do
  typeName <- getElementTypeOrFail ref
  coreType <- getTypeHaskellType typeName
  typ <- containForBounds lower upper $ return coreType
  return (useBang, typ)

xsdRefToBangTypeQ (AttributeRef ref usage) = do
  dbgBLabel "xsdRefToBangTypeQ call " ref
  coreType <- getTypeHaskellType ref
  dbgBLabel "xsdRefToBangTypeQ coreType" coreType
  return (useBang, attrTypeForUsage usage coreType)

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

-- | Given a description of how an attribute is used, modify its
-- Haskell `Type` to reflect that use.
attrTypeForUsage :: AttributeUsage -> Type -> Type
attrTypeForUsage Forbidden _ = TupleT 0
attrTypeForUsage Optional typ = AppT maybeConT typ
attrTypeForUsage Required typ = typ
