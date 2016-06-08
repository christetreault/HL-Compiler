module Main where

import Demo.EvenOdd
import Demo.PNCalc
import Demo.LambdaCalc
import Criterion.Main

main :: IO ()
main = do
   defaultMain [evenOddBenchSuite,
                pnCalcBenchSuite,
                lambdaCalcBenchSuite]
