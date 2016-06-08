module Main where

import Demo.EvenOdd
import Demo.PNCalc
import Demo.LambdaCalc
import Test.Tasty

main = do
   defaultMain $ testGroup "Tests"
      [evenOddTestSuite,
       pnCalcTestSuite,
       lambdaCalcTestSuite]
