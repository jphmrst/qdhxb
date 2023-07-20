
-- | Utilities based on the @XMLLight@ library.
module QDHXB.Utils.ZeroOneMany (
    ZeroOneMany(Zero, One, Many),
    zomToMaybe, zomToSingle, zomToList, listToZom,
    zappend, lzappend, zomfilter, zommap, zommapM, zomintercalate
    )
where
import Data.List (intercalate)
import Control.Monad.Except
import QDHXB.Utils.BPP

-- | Explicit tagging of whether a list has zero, one, or more than
-- one elements.
data ZeroOneMany a = Zero -- ^ Zero elements
  | One a    -- ^ One element
  | Many [a] -- ^ More then one element
  deriving Show

instance Blockable c => Blockable (ZeroOneMany c) where
  block Zero = stringToBlock "{Zero}"
  block (One a) = stringToBlock "{One}" `follow` block a
  block (Many xs) =
    stringToBlock "{Many}" `follow` (stackBlocks $ map block xs)

-- | Convert a `ZeroOneMany` type to a `Maybe` value, or fail, in an
-- error-handling monad.
zomToMaybe :: MonadError e m => e -> ZeroOneMany a -> m (Maybe a)
zomToMaybe _ Zero = return Nothing
zomToMaybe _ (One m) = return $ Just m
zomToMaybe e (Many _) = throwError e

-- | Convert a `ZeroOneMany` type to a single element, or fail, in an
-- error-handling monad.
zomToSingle :: MonadError e m => e -> ZeroOneMany a -> m a
zomToSingle e Zero = throwError e
zomToSingle _ (One m) = return m
zomToSingle e (Many _) = throwError e

-- | Convert a `ZeroOneMany` type to a list type, in an error-handling
-- monad.
zomToList :: MonadError e m => ZeroOneMany a -> m [a]
zomToList Zero = return []
zomToList (One m) = return [m]
zomToList (Many ms) = return ms

-- | Convert a `Data.List` to a `ZeroOneMany` type.
listToZom :: [a] -> ZeroOneMany a
listToZom [] = Zero
listToZom [m] = (One m)
listToZom ms = (Many ms)

-- | Append together two `ZeroOneMany` values.
zappend :: ZeroOneMany a -> ZeroOneMany a -> ZeroOneMany a
zappend Zero m = m
zappend m Zero = m
zappend (One s) (One t) = Many [s, t]
zappend (One s) (Many ts) = Many $ s : ts
zappend (Many ss) (One t) = Many $ ss ++ [t]
zappend (Many ss) (Many ts) = Many $ ss ++ ts

-- | Append together a list values and a `ZeroOneMany` value as
-- another `ZeroOneMany` value.
lzappend :: [a] -> ZeroOneMany a -> ZeroOneMany a
lzappend [] m = m
lzappend [s] Zero = One s
lzappend ms Zero = Many ms
lzappend [s] (One t) = Many [s, t]
lzappend ms  (One t) = Many $ ms ++ [t]
lzappend [s] (Many ns) = Many $ s : ns
lzappend ms  (Many ns) = Many $ ms ++ ns

-- | Return a new collection of elements satisfying the given
-- predicate.
zomfilter :: (a -> Bool) -> ZeroOneMany a -> ZeroOneMany a
zomfilter _ z@Zero = z
zomfilter p o@(One x) = if p x then o else Zero
zomfilter p (Many xs) = listToZom $ filter p xs

-- | Version of `map` for `ZeroOneMany` collections.
zommap :: (a -> b) -> ZeroOneMany a -> ZeroOneMany b
zommap _ Zero = Zero
zommap f (One m) = One $ f m
zommap f (Many ms) = Many $ map f ms

-- | Version of `mapM` for `ZeroOneMany` collections.
zommapM :: Monad m => (a -> m b) -> ZeroOneMany a -> m (ZeroOneMany b)
zommapM _ Zero = return Zero
zommapM f (One m) = fmap One $ f m
zommapM f (Many ms) = fmap Many $ mapM f ms

-- | Version of `intercalate` for `ZeroOneMany` instead of a list of
-- lists.
zomintercalate :: [a] -> ZeroOneMany [a] -> [a]
zomintercalate _ Zero = []
zomintercalate _ (One x) = x
zomintercalate s (Many xs) = intercalate s xs
