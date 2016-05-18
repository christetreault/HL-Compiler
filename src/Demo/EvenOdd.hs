{-# LANGUAGE FlexibleContexts #-}

module Demo.EvenOdd where

import Util
import HL
import Term
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.State
import HL.Optimize
import HL.Query
import Criterion.Main
import Test.Tasty
import Test.Tasty.SmallCheck


mkEven t = App 0 [t]
mkOdd t = App 1 [t]
mkSucc t = App 2 [t]
mkZero = App 3 []

mkN :: Integer -> Term Integer Integer
mkN n
   | n >= 0 = case n of
                 0 -> mkZero
                 _ -> mkSucc $ mkN $ n - 1
   | otherwise = impossible "n must be positive"

varZero = Var 0

ruleEO = Rule { ruleVars = 1,
                rulePrems = [mkOdd $ varZero],
                ruleConcl = mkEven $ mkSucc $ varZero }

ruleOE = Rule { ruleVars = 1,
                rulePrems = [mkEven $ varZero],
                ruleConcl = mkOdd $ mkSucc $ varZero }

ruleO = Rule { ruleVars = 0,
               rulePrems = [],
               ruleConcl = mkEven mkZero }

evenOddTac = fromJust $ makeProg fnMap entryPoint
   where
      entryPoint = "evenOdd"
      tacs = [HLApply ruleEO, HLApply ruleOE, HLApply ruleO]
      fnMap = Map.fromList
                 [(entryPoint,
                   HLSeq (hlFirst tacs)
                         (HCAll $ HLCall entryPoint))]

evenOddTacOpt = fromJust $ makeProg fnMap entryPoint
   where
      entryPoint = "evenOdd"
      fnOdd = "odd"
      fnEven = "even"
      evenTacs = [HLSeq (HLApply ruleEO) (HCAll $ HLCall fnOdd),
                  HLApply ruleO]
      fnMap = Map.fromList
                 [(fnOdd, HLSeq (HLApply ruleOE) (HCAll $ HLCall fnEven)),
                  (fnEven, hlFirst evenTacs),
                  (entryPoint, hlFirst [HLCall fnEven, HLCall fnOdd])]

custom t = case t of
   App 0 [t'] -> checkEven t'
   App 1 [t'] -> checkOdd t'
   _ -> False
   where
      checkEven (App 2 [t'']) = checkOdd t''
      checkEven (App 3 _) = True
      checkEven _ = False
      checkOdd (App 2 [t'']) = checkEven t''
      checkOdd _ = False

evens n =
    query (Just n) 1 evenOddTac $ mkEven $ UVar 0

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

evenOddBenchSuite :: Benchmark
evenOddBenchSuite =
   env (return $ mkEven $ mkN (10000 :: Integer)) $
   \ ~(t) -> bgroup "Even-Odd"
             [bench "Standard" $ nf (evenOddTac `unifies`) t,
              bench "Manual" $ nf (evenOddTacOpt `unifies`) t,
              bench "Optimized" $ nf ((normalizeProg evenOddTac) `unifies`) t]

evenOddTestSuite :: TestTree
evenOddTestSuite =
   testGroup "Even-Odd"
   [testProperty "Positive even numbers" posEven,
    testProperty "Positive odd numbers" posOdd,
    testProperty "If n is even, succ(n) is not" succNotSame]
   where
      posEven :: Integer -> Property IO
      posEven a = (>= (0 :: Integer))
                  ==> \n -> case n `mod` 2 of
                               0 -> (evenOddTac `unifies` (mkEven $ mkN n))
                               1 -> not (evenOddTac `unifies` (mkEven $ mkN n))
      posOdd :: Integer -> Property IO
      posOdd a = (>= (0 :: Integer))
                 ==> \n -> case n `mod` 2 of
                              1 -> (evenOddTac `unifies` (mkOdd $ mkN n))
                              0 -> not (evenOddTac `unifies` (mkOdd $ mkN n))
      succNotSame :: Integer -> Property IO
      succNotSame a = (>= (0 :: Integer))
                      ==> \n -> (evenOddTac `unifies` (mkEven
                                                       $ mkN n))
                                /= (evenOddTac `unifies` (mkEven
                                                          $ mkSucc $ mkN n))
