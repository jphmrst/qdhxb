
-- | Block Pretty-Printer, a quick-and-dirty pretty printer that uses
-- a list of `String`s for a multi-line representation.
module QDHXB.Internal.Utils.BPP (
  -- * Top-level pretty-printer functions
  bpp, bpp', bprint, bprintLn, bLabelPrint, bLabelPrintln,
  -- * Main type and classes
  Block(Block), Blockable, block,
  VerticalBlockList, stringToBlock,
  -- * Operations on `Block`s
  follow, indent, stack2, stackBlocks, labelBlock
  ) where
import Data.List (intercalate)
import Text.XML.Light.Types

newtype Block = Block { openBlock :: [String] }

stringToBlock :: String -> Block
stringToBlock s = Block $ lines s

bprint :: Blockable c => c -> IO ()
bprint = putStr . bpp

bprintLn :: Blockable c => c -> IO ()
bprintLn = putStrLn . bpp

bLabelPrint :: Blockable c => String -> c -> IO ()
bLabelPrint l = putStr . outBlock . labelBlock l . block

bLabelPrintln :: Blockable c => String -> c -> IO ()
bLabelPrintln l = putStrLn . outBlock . labelBlock l . block

-- | Top-level pretty-printer
bpp :: Blockable c => c -> String
bpp = bpp' ""

-- | Pretty-printer specifying indentation of all lines.
bpp' :: Blockable c => String -> c -> String
bpp' ind = outBlock . indent ind . block

outBlock :: Block -> String
outBlock (Block ls) = intercalate "\n" ls

class Blockable c where
  block :: c -> Block

-- Vertically arrange and left-align two `Block`s
stack2 :: Block -> Block -> Block
stack2 (Block ss1) (Block ss2) = Block $ ss1 ++ ss2

-- Vertically arrange and left-align a list of `Blockable` values
stackBlocks :: [Block] -> Block
stackBlocks = Block . concat . map openBlock

-- Append two block, with the first line of the second appearing after
-- the last line of the first.  No space in inserted; use `follow_`.
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

-- Label a block with a (one-line) string, indenting subsequent lines
-- in alignment.
labelBlock :: String -> Block -> Block
labelBlock l b = stringToBlock l `follow` b

class Blockable c => VerticalBlockList c

instance VerticalBlockList c => Blockable [c] where
  block = Block . concat . map (openBlock . block)

indent :: String -> Block -> Block
indent ind (Block xs) = Block $ map (ind ++) xs

instance Blockable Content where
  block = stringToBlock . show
instance Blockable Attr where
  block = stringToBlock . show
