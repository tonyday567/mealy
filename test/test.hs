{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import NumHask.Prelude
import Test.DocTest
import Data.Mealy

main :: IO ()
main =
  doctest
  [ "src/Data/Mealy.hs",
    "src/Data/Mealy/Quantiles.hs",
    "src/Data/Mealy/Simulate.hs"
  ]
