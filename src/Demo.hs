module Demo where

import HL
import Types
import qualified Data.Map as Map
import Data.Maybe (fromJust)

mkEven t = App 0 [t]
mkOdd t = App 1 [t]
mkSucc t = App 2 [t]
mkZero = App 3 []
mkN n = case n of
   0 -> mkZero
   _ -> mkSucc $ mkN $ n - 1

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
