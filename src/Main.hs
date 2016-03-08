module Main where

import Demo.EvenOdd
import Demo.PNCalc
import Criterion.Main

main :: IO ()
main = do
   defaultMain [evenOddBenchSuite,
                pnCalcBenchSuite]
