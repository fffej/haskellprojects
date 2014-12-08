{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith (circle 1 :: Diagram B R2)
