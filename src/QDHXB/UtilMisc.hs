{-# LANGUAGE TemplateHaskell #-}

module QDHXB.UtilMisc (applyFst, applySnd) where

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)
applySnd :: (a -> b) -> (c, a) -> (c, b)
applySnd f (x,y) = (x, f y)
