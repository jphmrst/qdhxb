{-# LANGUAGE TemplateHaskell #-}

{-| Basic blocks of Template Haskell `Stmt` statements used in
  generating code for XSD definitions.  The main type here is
  `BlockMaker`, which maps two TH `Name`s to a list of `Stmt`s for use
  in a @do@-block.  This module contains builders and combinators for
  `BlockMaker`s.
-}

module QDHXB.Internal.Block (
  -- * Basic block type
  BlockMaker,
  -- ** Combinators on `BlockMaker`s
  retrievingCRefFor, scaleBlockMakerToBounds,
  -- ** Some atomic `BlockMaker`s
  listToMaybe, listToSingle
  )
where

import Language.Haskell.TH
import Text.XML.Light.Types (QName, qName)
import QDHXB.Internal.Utils.TH
import QDHXB.Internal.XSDQ

-- | Map from @source@ and @dest@ identifiers to a list of TH `Stmt`
-- statements implementing some calculation.
type BlockMaker = Name -> Name -> [Stmt]

-- | Given a `BlockMaker` which operates on a `String`, return a
-- `BlockMaker` which fetches a `String` from the @CRef@ innards of a
-- piece of XML `Text.XML.Light.Types.Content`, and use it as the
-- `String` for the `BlockMaker`.
retrievingCRefFor :: QName -> BlockMaker -> XSDQ BlockMaker
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
scaleBlockMakerToBounds ::
  BlockMaker -> Maybe Int -> Maybe Int -> XSDQ BlockMaker
scaleBlockMakerToBounds _ _ (Just 0) = do
  whenDebugging $ dbgLn "- scaleBlockMakerToBounds none case"
  return $ \_ dest -> [ LetS [ ValD (VarP dest) (NormalB $ TupE []) [] ] ]
scaleBlockMakerToBounds k (Just 1) (Just 1) = do
  whenDebugging $ dbgLn "- scaleBlockMakerToBounds single case"
  return k
scaleBlockMakerToBounds k _ (Just 1) = do
  whenDebugging $ dbgLn "- scaleBlockMakerToBounds maybe case"
  tmp <- newName "possible"
  return $ \src dest ->
    [BindS (VarP dest) $
       (DoE Nothing $ k src tmp ++ [NoBindS $ applyReturn $ VarE tmp])
         `replaceOnError` (applyReturn $ nothingConE)]
scaleBlockMakerToBounds k _ _ = do
  whenDebugging $ dbgLn "- scaleBlockMakerToBounds list case"
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
listToSingle :: BlockMaker
listToSingle src dest =
  [ LetS [
     ValD (VarP dest)
       (NormalB $
         zomCaseSingle' (VarE src) uName (VarE uName)
           (throwsError "Single element required, multiple found")) []
     ]
  ]
  where uName = mkName "u"

-- | Atomic `BlockMaker` which expects a list value at its source.  If
-- the value is either empty or a singleton list, then it writes a
-- `Maybe`-value containing any element at its destination; otherwise
-- it throws a run-time error.
listToMaybe :: BlockMaker
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
