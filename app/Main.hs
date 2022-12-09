{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import QDHXB
qdhxb ["xsd.xsd"]

main :: IO ()
main = putStrLn "OK"
