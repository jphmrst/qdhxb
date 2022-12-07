{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import QDHXB

$(qdhxb)

main :: IO ()
main = do
  putStrLn (show x)
