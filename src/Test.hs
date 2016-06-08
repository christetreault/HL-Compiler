module Main where

import Demo.EvenOdd
import Demo.PNCalc
import Demo.LambdaCalc
import Test.Tasty
import Test.Tasty.HUnit

main = do
   defaultMain $ testGroup "Tests"
      [evenOddTestSuite,
       pnCalcTestSuite,
       lambdaCalcTestSuite]
