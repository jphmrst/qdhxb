
-- | Miscellaneous helper functions.
module QDHXB.UtilMisc (applyFst, applySnd) where

-- | Apply a function to the first element of a pair, and return a
-- pair with the original second element.
applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)

-- | Apply a function to the second element of a pair, and return a
-- pair with the original first element.
applySnd :: (a -> b) -> (c, a) -> (c, b)
applySnd f (x,y) = (x, f y)
