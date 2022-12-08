{-# LANGUAGE TemplateHaskell #-}

module Main (main) where
import QDHXB

qdhxb ["shiporder1.xsd"]

main :: IO ()
main = putStrLn "OK"
