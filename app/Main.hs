{-# LANGUAGE TemplateHaskell #-}

module Main (main) where
import QDHXB

qdhxb ["shiporder.xsd"]

main :: IO ()
main = putStrLn "OK"
