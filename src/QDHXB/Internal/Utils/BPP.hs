{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Block Pretty-Printer, a quick-and-dirty pretty printer that uses
-- a list of `String`s for a multi-line representation.
module QDHXB.Internal.Utils.BPP (
  -- * Top-level pretty-printer functions
  bpp, bpp', bprint, bprintLn, bLabelPrint, bLabelPrintln,
  -- * Main type and classes
  Block(Block), Blockable,
  -- ** Deriving new `Blockable` types
  verticalBlockList, bulletedVerticalBlockList,
  verticalBlockListFn, bulletedVerticalBlockListFn,
  verticalBlockablePair, verticalBlockablePairFn,
  horizontalBlockablePair, horizontalBlockablePairFn,
  horizontalPair, horizontalBlocksPair,
  -- * Operations on `Block`s
  -- ** Creating a blocks
  block, stringToBlock,
  -- ** Combining blocks
  follow, indent, stack2, stackBlocks, labelBlock, postlabelBlock, outBlock,
  -- ** Printing blocks directly
  putBlockLn, putBlock
  ) where
import Language.Haskell.TH
import Data.List (intercalate)
import Text.XML.Light.Types
import Text.XML.Light.Output

-- | The working type for formatting a value.
newtype Block = Block {
  openBlock :: [String]
  -- ^ Expose the list of `String`s encapsulated in a `Block`.
  }

-- | For `Block`s, `show` is an alias to `outBlock`.
instance Show Block where show = outBlock

-- | Convert a `String` to a `Block`.
stringToBlock :: String -> Block
stringToBlock s = Block $ lines s

-- | Main class of types whose values can be pretty-printed via this
-- class.
class Blockable c where
  -- | Given a pretty-printable value, format it as a `Block`.
  block :: c -> Block

-- | Convenience instance: break the string up into lines, and then
-- stack them.
instance Blockable [Char] where block = stringToBlock
-- | Convenience instance via `show`.
instance Blockable Double where block = stringToBlock . show
-- | Convenience instance via `show`.
instance Blockable Float  where block = stringToBlock . show
-- | Convenience instance via `show`.
instance Blockable Bool   where block = stringToBlock . show
-- | Convenience instance via `show`.
instance Blockable Int    where block = stringToBlock . show

instance Blockable c => Blockable (Maybe c) where
  block (Just x) = block x
  block Nothing  = stringToBlock "Nothing"

-- | Print a `Blockable` value to standard output.
putBlock :: Block -> IO ()
putBlock = putStr . outBlock

-- | Print a `Blockable` value to standard output.
putBlockLn :: Block -> IO ()
putBlockLn b = do
  putBlock b
  putStrLn ""

-- | Print a `Blockable` value to standard output.
bprint :: Blockable c => c -> IO ()
bprint = putStr . bpp

-- | Print a `Blockable` value, followed by a newline, to standard output.
bprintLn :: Blockable c => c -> IO ()
bprintLn = putStrLn . bpp

-- | Print a `String` label followed by a `Blockable` value to
-- standard output.
bLabelPrint :: Blockable c => String -> c -> IO ()
bLabelPrint l = putStr . outBlock . labelBlock l . block

-- | Print a `String` label, a `Blockable` value, and a newline, to
-- standard output.
bLabelPrintln :: Blockable c => String -> c -> IO ()
bLabelPrintln l = putStrLn . outBlock . labelBlock l . block

-- | Top-level pretty-printer
bpp :: Blockable c => c -> String
bpp = bpp' ""

-- | Pretty-printer specifying indentation of all lines.
bpp' :: Blockable c => String -> c -> String
bpp' ind = outBlock . indent ind . block

-- | Convert
outBlock :: Block -> String
outBlock (Block ls) = intercalate "\n" ls

-- | Vertically arrange and left-align two `Block`s
stack2 :: Block -> Block -> Block
stack2 (Block ss1) (Block ss2) = Block $ ss1 ++ ss2

-- | Vertically arrange and left-align a list of `Blockable` values
stackBlocks :: [Block] -> Block
stackBlocks = Block . concat . map openBlock

-- | Append two block, with the first line of the second appearing after
-- the last line of the first.
follow :: Block -> Block -> Block
(Block ss1) `follow` (Block ss2) = Block $ helper ss1 ss2
  where helper :: [String] -> [String] -> [String]
        helper [] ss = ss
        helper [x] (s:ss) = (x ++ s) : map (asSpaces x ++) ss
        helper xs@[_] [] = xs
        helper (x:xs) ss = x : helper xs ss

        asSpaces :: [a] -> String
        asSpaces [] = ""
        asSpaces (_:xs) = ' ' : asSpaces xs

-- | Label a block with a (one-line) string, indenting subsequent lines
-- in alignment.
labelBlock :: String -> Block -> Block
labelBlock l b = stringToBlock l `follow` b

-- | Follow the last line of a block with a (one-line) string.
postlabelBlock :: String -> Block -> Block
postlabelBlock suffix = Block . helper . openBlock
  where helper :: [String] -> [String]
        helper [] = [suffix]
        helper [x] = [x ++ suffix]
        helper (x:xs) = x : helper xs

-- | Standalone function rendering a list as a vertical rendering of
-- its elements.
verticalBlockListFn :: Blockable c => [c] -> Block
verticalBlockListFn = Block . concat . map (openBlock . block)

-- | For some type @t@, declare @[t]@ to be rendered as a vertical
-- rendering of its elements.  Will raise an error if @t@ is not
-- `Blockable`.
verticalBlockList :: Q Type -> Q [Dec]
verticalBlockList name =
  [d|instance QDHXB.Internal.Utils.BPP.Blockable [$name]
       where block = QDHXB.Internal.Utils.BPP.verticalBlockListFn|]

-- | Standalone function rendering a list as a bulleted vertical
-- rendering of its elements.
bulletedVerticalBlockListFn :: Blockable c => [c] -> Block
bulletedVerticalBlockListFn =
  Block . concat . map (openBlock . labelBlock "- " . block)

-- | For some type @t@, declare @[t]@ to be rendered as a vertical
-- bulleted list of elements.  Will raise an error if @t@ is not
-- `Blockable`.
bulletedVerticalBlockList :: Q Type -> Q [Dec]
bulletedVerticalBlockList name =
  [d|instance QDHXB.Internal.Utils.BPP.Blockable [$name]
       where block = QDHXB.Internal.Utils.BPP.bulletedVerticalBlockListFn|]

-- | Standalone function rendering a pair with the first component
-- directly above the second.
verticalBlockablePairFn :: (Blockable m, Blockable n) => (m, n) -> Block
verticalBlockablePairFn (a, b) = labelBlock "(" $ stackBlocks [
  postlabelBlock "," $ block a,
  postlabelBlock ")" $ block b
  ]

-- | For some types @s@ and @t@, declare @(s,t)@ to be rendered
-- vertically, one element atop the other.  Will raise an error if
-- either @s@ or @t@ is not `Blockable`.
verticalBlockablePair :: Q Type -> Q Type -> Q [Dec]
verticalBlockablePair t1 t2 =
  [d|instance QDHXB.Internal.Utils.BPP.Blockable ($t1,$t2)
       where block = QDHXB.Internal.Utils.BPP.verticalBlockablePairFn|]

-- | Standalone function rendering a pair on one line.
horizontalBlockablePairFn :: (Blockable m, Blockable n) => (m, n) -> Block
horizontalBlockablePairFn (a, b) = labelBlock "(" $
    (postlabelBlock "," $ block a) `follow` (postlabelBlock ")" $ block b)

-- | For some types @s@ and @t@, declare @(s,t)@ to be rendered on one
-- line.  Will raise an error if either @s@ or @t@ is not `Blockable`.
horizontalBlockablePair :: Q Type -> Q Type -> Q [Dec]
horizontalBlockablePair t1 t2 =
  [d|instance QDHXB.Internal.Utils.BPP.Blockable ($t1,$t2)
       where block = QDHXB.Internal.Utils.BPP.horizontalBlockablePairFn|]

-- | Given two blockable values, format them as an ordered pair.
horizontalPair :: (Blockable a, Blockable b) => a -> b -> Block
horizontalPair a b = horizontalBlocksPair (block a) (block b)

-- | Given two `Block`s, format them as an ordered pair.
horizontalBlocksPair :: Block -> Block -> Block
horizontalBlocksPair a b =
  labelBlock "(" $ (postlabelBlock "," a) `follow` (postlabelBlock ")" b)

-- | Indent every line of a pretty-printed representation with the given
-- string.
indent :: String -> Block -> Block
indent ind (Block xs) = Block $ map (ind ++) xs

-- | Allow `Element` from the @XMLLight@ library to be pretty-printed.
instance Blockable Element where block = stringToBlock . showElement

-- | Allow `Content` from the @XMLLight@ library to be pretty-printed.
instance Blockable Content where block = stringToBlock . showContent
instance Blockable [Content] where block = verticalBlockListFn

-- | Allow `Attr` from the @XMLLight@ library to be pretty-printed.
instance Blockable Attr where block = stringToBlock . showAttr
instance Blockable [Attr] where block = verticalBlockListFn

-- | Allow `QName` from the @XMLLight@ library to be pretty-printed.
instance Blockable QName where block = stringToBlock . showQName
-- | Allow [`QName`] from the @XMLLight@ library to be pretty-printed.
instance Blockable [QName] where block = verticalBlockListFn
instance Blockable (QName, QName) where block = horizontalBlockablePairFn
instance Blockable [(QName, QName)] where block = verticalBlockListFn

-- | Allow `Dec` from the Template Haskell library to be pretty-printed.
instance Blockable Dec where block = stringToBlock . pprint
-- | Allow [`Dec`] from the Template Haskell library to be pretty-printed.
instance Blockable [Dec] where block = verticalBlockListFn

-- | Allow `Stmt` from the Template Haskell library to be pretty-printed.
instance Blockable Stmt where block = stringToBlock . pprint
-- | Allow [`Stmt`] from the Template Haskell library to be pretty-printed.
instance Blockable [Stmt] where block = verticalBlockListFn
instance Blockable [[Stmt]] where block = bulletedVerticalBlockListFn

-- | Allow `Exp` from the Template Haskell library to be pretty-printed.
instance Blockable Exp where block = stringToBlock . pprint
-- | Allow [`Exp`] from the Template Haskell library to be pretty-printed.
instance Blockable [Exp] where block = verticalBlockListFn

-- | Allow `Type` from the Template Haskell library to be pretty-printed.
instance Blockable Type where block = stringToBlock . pprint
-- | Allow [`Type`] from the Template Haskell library to be pretty-printed.
instance Blockable [Type] where block = verticalBlockListFn

-- | Allow `Con` from the Template Haskell library to be pretty-printed.
instance Blockable Con where block = stringToBlock . pprint
-- | Allow [`Con`] from the Template Haskell library to be pretty-printed.
instance Blockable [Con] where block = verticalBlockListFn

-- | Allow `Name` from the Template Haskell library to be pretty-printed.
instance Blockable Name where block = stringToBlock . pprint
-- | Allow [`Name`] from the Template Haskell library to be pretty-printed.
instance Blockable [Name] where block = verticalBlockListFn
