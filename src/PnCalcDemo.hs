module PnCalcDemo where

import HL
import HL.Compile
import Control.Monad.State
import Term
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.PrettyPrint.HughesPJClass hiding (empty)

data CalcTerm =
   CTRec
   | CTCons
   | CTNil
   | CTChar Char
   deriving (Eq, Ord)

instance Show CalcTerm where
   show = render . pPrint

instance Pretty CalcTerm where
   pPrint CTRec = text "Recognized"
   pPrint CTCons = text "Cons"
   pPrint CTNil = text "Nil"
   pPrint (CTChar c) = quotes (pPrint c)

tryTac :: Term CalcTerm VarId
          -> HLProg CalcTerm VarId
          -> Maybe [Term CalcTerm VarId]
tryTac t p = evalStateT (action t) empty
   where
      action :: Term CalcTerm VarId
                -> StateT (SubstEnv CalcTerm VarId)
                          Maybe [Term CalcTerm VarId]
      action = compile p

isRec t = App CTRec [t]
isCons l r = App CTCons [l, r]
isNil = App CTNil []
isChar d = App (CTChar d) []

recN n = Rule { ruleVars = 0,
                rulePrems = [],
                ruleConcl = isRec $ isChar n }

recNil = Rule { ruleVars = 0,
                rulePrems = [],
                ruleConcl = isRec $ isNil }

recCons = Rule { ruleVars = 0,
                 rulePrems = [],
                 ruleConcl = isRec $ isCons (Var 0) $ Var 1 }

rec1 = recN '1'
rec2 = recN '2'
rec3 = recN '3'
rec4 = recN '4'
rec5 = recN '5'
rec6 = recN '6'
rec7 = recN '7'
rec8 = recN '8'
rec9 = recN '9'
rec0 = recN '0'

recPlus = Rule { ruleVars = 3,
                 rulePrems = [isRec $ Var 0, isRec $ Var 1],
                 ruleConcl = isRec
                             $ isCons (isChar '+')
                             $ isCons (Var 0)
                             $ isCons (Var 1) $ Var 2 }

basicBinAdd = fromJust $ makeProg fnMap entryPoint
   where
      entryPoint = "isBinAdd"
      tacs = [HLApply recPlus,
              HLApply rec1,
              HLApply rec0,
              HLApply recCons,
              HLApply recNil]
      fnMap = Map.fromList
              [(entryPoint,
                HLSeq
                   (hlFirst tacs)
                   (HCAll $ HLCall entryPoint))]
