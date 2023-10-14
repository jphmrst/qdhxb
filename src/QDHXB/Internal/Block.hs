{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

{-| Basic blocks of Template Haskell `Stmt` statements used in
  generating code for XSD definitions.  The main type here is
  `BlockMaker`, which maps two TH `Name`s to a list of `Stmt`s for use
  in a @do@-block.  This module contains builders and combinators for
  `BlockMaker`s.
-}

module QDHXB.Internal.Block (
  -- * Basic block type
  BlockMaker, blockMakerClose, blockMakerCloseWith, abstractOnSourceName,
  Unitized,
  -- ** Combinators on `BlockMaker`s
  retrievingCRefFor, scaleBlockMakerToBounds, stringBlockToAttributeBlock,
  -- ** Some atomic `BlockMaker`s
  listToMaybe, listToSingle,
  )
where

import Language.Haskell.TH
import Text.XML.Light.Types (Content, QName, qName)
import QDHXB.Utils.ZeroOneMany
import QDHXB.Utils.TH
import QDHXB.Internal.XSDQ

import QDHXB.Internal.Debugln hiding (dbgLn, dbgBLabelFn2, dbgResultFn2)
import qualified QDHXB.Internal.Debugln as DBG
dbgLn :: (MonadDebugln m n) => String -> m ()
dbgLn = DBG.dbgLn blocks 0

-- | The destination (second) argument of `BlockMaker` contains a
-- simplified version of types, with list/`Maybe`/other type
-- constructors wrapping only the unit type.  Added to (hopefully)
-- ease debugging in the @Generate@ module.
type family Unitized t where
  Unitized [t] = [Unitized t]
  Unitized (Maybe t) = Maybe (Unitized t)
  Unitized (ZeroOneMany t) = ZeroOneMany (Unitized t)
  Unitized _ = ()

-- | Map from @source@ and @dest@ identifiers to a list of TH `Stmt`
-- statements implementing some calculation.
type BlockMaker srcType destShape = Name -> Name -> [Stmt]

-- | Given a `BlockMaker` which operates on a `String`, return a
-- `BlockMaker` which fetches a `String` from the @CRef@ innards of a
-- piece of XML `Text.XML.Light.Types.Content`, and use it as the
-- `String` for the `BlockMaker`.
retrievingCRefFor ::
  QName -> BlockMaker String a -> XSDQ (BlockMaker Content a)
retrievingCRefFor qn strDec = do
  tmp1 <- newName "maybeContent"
  tmp2 <- newName "content"
  return $ \src dest -> [
    LetS [
        ValD (VarP tmp1) (NormalB $ applyPullCRefContent $ VarE src) []
        ],
      BindS (VarP tmp2)
        (applyMaybe (applyThrowExp $
                      qCrefMustBePresentIn (qName qn) Nothing)
          quotedReturnId (VarE tmp1))
    ] ++ strDec tmp2 dest

-- | Given a `BlockMaker` for a single instance of some artifact,
-- produce a (possibly different, but possibly the same thing)
-- `BlockMaker` which acts for a possible number of instances within
-- the given bounds.  This function interprets `Nothing` in a bound as
-- "unbounded" rather than "default."
--
-- No help from the second type argument to `BlockMaker` here --- no
-- dependent types.
scaleBlockMakerToBounds ::
  BlockMaker a b -> Maybe Int -> Maybe Int -> XSDQ (BlockMaker a c)
scaleBlockMakerToBounds _ _ (Just 0) = do
  whenAnyDebugging $ dbgLn "- scaleBlockMakerToBounds none case"
  return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
scaleBlockMakerToBounds k (Just 1) (Just 1) = do
  whenAnyDebugging $ dbgLn "- scaleBlockMakerToBounds single case"
  return k
scaleBlockMakerToBounds k _ (Just 1) = do
  whenAnyDebugging $ dbgLn "- scaleBlockMakerToBounds maybe case"
  tmp <- newName "possible"
  return $ \src dest ->
    [BindS (VarP dest) $
       (DoE Nothing $ k src tmp ++ [NoBindS $ applyReturn $ VarE tmp])
         `replaceOnError` (applyReturn $ nothingConE)]
scaleBlockMakerToBounds k _ _ = do
  whenAnyDebugging $ dbgLn "- scaleBlockMakerToBounds list case"
  x <- newName "x"
  local <- newName "local"
  return $ \src dest ->
    [BindS (VarP dest) $
       applyMapM
         (LamE [VarP x] $
            DoE Nothing $ k x local ++ [NoBindS $ applyReturn $ VarE local])
         (VarE src)]

-- | Atomic `BlockMaker` which expects a list value at its source.  If
-- the value is a singleton list, it writes the element at its
-- destination; otherwise it throws a run-time error.
listToSingle :: String -> BlockMaker [a] (Unitized a)
listToSingle msg src dest =
  [ LetS [
     ValD (VarP dest)
       (NormalB $
          zomCaseSingle' (VarE src) uName (VarE uName) $ throwsError msg)
       []
     ]
  ]
  where uName = mkName "u"

-- | Atomic `BlockMaker` which expects a list value at its source.  If
-- the value is either empty or a singleton list, then it writes a
-- `Maybe`-value containing any element at its destination; otherwise
-- it throws a run-time error.
listToMaybe :: BlockMaker [a] (Maybe (Unitized a))
listToMaybe src dest =
  [ LetS [
      ValD (VarP dest)
        (NormalB $
           zomCase (VarE src) nothingConE
             vName (AppE justConE (VarE vName))
             vName (throwsError
                 "Zero or single element required, multiple found"))
        []]]
  where vName = mkName "v"

-- | Turn a `BlockMaker` @b@ into an `Exp` for a do-expression
-- returning @b@'s result, transformed as given by @expF@, all given
-- the `Name` of a source.
blockMakerCloseWith :: (Exp -> Exp) -> BlockMaker st dt -> Name -> Exp
blockMakerCloseWith expF b src =
  let res = mkName "result"
  in DoE Nothing $ b src res ++ [ NoBindS $ applyReturn $ expF $ VarE res ]

-- | Turn a `BlockMaker` @b@ into an `Exp` for a do-expression
-- returning @b@'s result, all given the `Name` of a source.
blockMakerClose :: BlockMaker st dt -> Name -> Exp
blockMakerClose = blockMakerCloseWith id

-- | Build a TH lambda abstraction over the @src@ name over e.g. the
-- result of a partial application of `blockMakerClose`.
abstractOnSourceName :: (Name -> Exp) -> XSDQ Exp
abstractOnSourceName ef = do
  src <- newName "src"
  return $ LamE [VarP src] $ ef src

-- | Convert a `BlockMaker` operating on `String` input to a
-- `BlockMaker` which uses the value of the given attribute of a piece
-- of XML `Content`.
stringBlockToAttributeBlock ::
  BlockMaker String a -> QName -> XSDQ (BlockMaker Content a)
stringBlockToAttributeBlock maker attrQName = do
  attrName <- newName "attr"
  let decoder :: BlockMaker Content dt
      decoder src dest = [
        LetS [SigD attrName (AppT maybeConT stringConT),
              ValD (VarP attrName)
                   (NormalB $ applyPullAttrFrom (qName attrQName) (VarE src))
                   []],
        BindS (VarP dest) $
          caseNothingJust' (VarE attrName) (applyReturn nothingConE)
            xName (DoE Nothing $ maker xName resName ++ [
                     NoBindS $ applyReturn $ applyJust (VarE resName)
                     ])]
  return decoder

