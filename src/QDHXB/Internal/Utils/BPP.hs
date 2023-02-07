{-# LANGUAGE MultiParamTypeClasses #-}

-- | Block Pretty-Printer, a quick-and-dirty pretty printer that uses
-- a list of `String`s for a multi-line representation.
module QDHXB.Internal.Utils.BPP (
  -- * Top-level pretty-printer functions
  bpp, bpp', bprint, bprintLn, bLabelPrint, bLabelPrintln,
  -- * Main type and classes
  Block(Block), Blockable, block,
  VerticalBlockList, stringToBlock,
  VerticalBlockablePair, horizontalPair, horizontalBlocksPair,
  -- * Operations on `Block`s
  follow, indent, stack2, stackBlocks, labelBlock, postlabelBlock, outBlock
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

instance Blockable c => Blockable (Maybe c) where
  block (Just x) = block x
  block Nothing  = stringToBlock "Nothing"

-- | For any two `Blockable` types, an ordered pair consisting of
-- values which are an instance of this class will be `Blockable`, and
-- moreover will format the pair with one element above the other.
class (Blockable m, Blockable n) => VerticalBlockablePair m n

-- | Automatically infer `Blockable` for any instance of
-- `VerticalBlockablePair`.
instance VerticalBlockablePair m n => Blockable (m,n) where
  block (a, b) =
    labelBlock "(" $ stackBlocks [
    postlabelBlock "," $ block a,
    postlabelBlock ")" $ block b
    ]

{-
class (Blockable m, Blockable n) => HorizontalBlockablePair m n
instance HorizontalBlockablePair m n => Blockable (m,n) where
  block (a, b) =
    labelBlock "(" $
      (postlabelBlock "," $ block a) `follow` (postlabelBlock ")" $ block b)
 -}

-- | Given two blockable values, format them as an ordered pair.
horizontalPair :: (Blockable a, Blockable b) => a -> b -> Block
horizontalPair a b = horizontalBlocksPair (block a) (block b)

-- | Given two `Block`s, format them as an ordered pair.
horizontalBlocksPair :: Block -> Block -> Block
horizontalBlocksPair a b =
  labelBlock "(" $ (postlabelBlock "," a) `follow` (postlabelBlock ")" b)

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

-- | When a class @c@ is a member of the `VerticalBlockList` class,
-- Haskell is allowed to derive a `Blockable` instance for @[c]@.
class Blockable c => VerticalBlockList c

-- | Actual derivations of a `Blockable` instance for @[c]@ when @c@ is
-- a member of the `VerticalBlockList` class.
instance VerticalBlockList c => Blockable [c] where
  block = Block . concat . map (openBlock . block)

-- | Indent every line of a pretty-printed representation with the given
-- string.
indent :: String -> Block -> Block
indent ind (Block xs) = Block $ map (ind ++) xs

-- | Allow `Content` from the @XMLLight@ library to be pretty-printed.
instance Blockable Element where block = stringToBlock . showElement
-- | Allow `Content` from the @XMLLight@ library to be pretty-printed.
instance Blockable Content where block = stringToBlock . showContent
-- | Allow `Attr` from the @XMLLight@ library to be pretty-printed.
instance Blockable Attr    where block = stringToBlock . showAttr
-- | Allow `QName` from the @XMLLight@ library to be pretty-printed.
instance Blockable QName   where block = stringToBlock . showQName
-- | Allow [`QName`] from the @XMLLight@ library to be pretty-printed.
instance VerticalBlockList QName
-- | Allow `Dec` from the Template Haskell library to be pretty-printed.
instance Blockable Dec   where block = stringToBlock . pprint
-- | Allow [`Dec`] from the Template Haskell library to be pretty-printed.
instance VerticalBlockList Dec

