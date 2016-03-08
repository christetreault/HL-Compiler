{-# LANGUAGE FlexibleContexts #-}

module Demo.EvenOdd where

import Util
import HL
import Term
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.State
import HL.Compile
import HL.Optimize
import HL.Query
import Criterion.Main
import Test.Tasty
import Test.Tasty.SmallCheck



tac_evenOdd t = tac t evenOddTac
tac_evenOddMan t = tac t evenOddTacOpt
tac_evenOddOpt t = tac t $ normalizeProg evenOddTac

tac :: Term Integer VarId -> HLProg Integer VarId ->Maybe [Term Integer VarId]
tac t p = evalStateT (action t) empty
   where
      action :: Term Integer VarId
                -> StateT (SubstEnv Integer VarId) Maybe [Term Integer VarId]
      action = compileMaybe p


mkEven t = App 0 [t]
mkOdd t = App 1 [t]
mkSucc t = App 2 [t]
mkZero = App 3 []
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

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

evenOddBenchSuite :: Benchmark
evenOddBenchSuite =
   env (return $ mkEven $ mkN (10000 :: Integer)) $
   \ ~(t) -> bgroup "Even-Odd"
             [bench "Standard" $ nf tac_evenOdd t,
              bench "Manual" $ nf tac_evenOddMan t,
              bench "Optimized" $ nf tac_evenOddOpt t]

evenOddTestSuite :: TestTree
evenOddTestSuite =
   testGroup "Even-Odd"
   [testProperty "Positive even numbers" posEven,
    testProperty "Positive odd numbers" posOdd,
    testProperty "If n is even, succ(n) is not" succNotSame]
   where
      posEven :: Integer -> Property IO
      posEven a = (>= (0 :: Integer))
                  ==> \n -> case (n `mod` 2 :: Integer) of
                               0 -> (tac_evenOdd (mkEven $ mkN n))
                                    == Just []
                               1 -> (tac_evenOdd (mkEven $ mkN n))
                                    == Nothing
      posOdd :: Integer -> Property IO
      posOdd a = (>= (0 :: Integer))
                 ==> \n -> case (n `mod` 2 :: Integer) of
                              1 -> (tac_evenOdd (mkOdd $ mkN n))
                                   == Just []
                              0 -> (tac_evenOdd (mkOdd $ mkN n))
                                   == Nothing
      succNotSame :: Integer -> Property IO
      succNotSame a = (>= (0 :: Integer))
                      ==> \n -> (tac_evenOdd (mkEven $ mkN (n :: Integer)))
                                /= (tac_evenOdd (mkEven $ mkSucc $ mkN n))
