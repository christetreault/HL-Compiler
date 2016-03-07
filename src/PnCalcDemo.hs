{-# LANGUAGE DeriveGeneric #-}

module PnCalcDemo where

import HL
import HL.Compile
import HL.Optimize
import HL.Query
import Control.Monad.State
import Term
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Control.DeepSeq
import GHC.Generics

data CalcTerm =
   CTRec
   | CTCons
   | CTNil
   | CTChar Char
   | CTRecThen
   deriving (Eq, Ord, Generic)

instance NFData CalcTerm

instance Show CalcTerm where
   show = render . pPrint

instance Pretty CalcTerm where
   pPrint CTRec = text "Recognized"
   pPrint CTCons = text "Cons"
   pPrint CTNil = text "Nil"
   pPrint (CTChar c) = quotes (pPrint c)
   pPrint (CTRecThen) = text "Recognized then"

tryBasicBin = (flip tryTac) basicBinAdd
tryOptBasicBin = (flip tryTac) $ normalizeProg basicBinAdd

tryTac :: Term CalcTerm VarId
          -> HLProg CalcTerm VarId
          -> [[Term CalcTerm VarId]]
tryTac t p = evalStateT (action (isRec t)) empty
   where
      action :: Term CalcTerm VarId
                -> StateT (SubstEnv CalcTerm VarId)
                   [] [Term CalcTerm VarId]
      action = compileList p

isRec t = App CTRec [t]
isCons l r = App CTCons [l, r]
isNil = App CTNil []
isChar d = App (CTChar d) []
isRecThen x z =  App CTRecThen [x, z]


recN n = Rule { ruleVars = 1,
                rulePrems = [],
                ruleConcl = isRecThen (isCons (isChar n) (Var 0)) (Var 0) }

recNil = Rule { ruleVars = 1,
                rulePrems = [isRecThen (Var 0) isNil],
                ruleConcl = isRec (Var 0)}

reallyLong = "+1+1++11+++111++11++11+1++11+++111+1+1++11+1+1++11+1+1+11"


unifyPrefix s = isRec $ isCons (UVar 0) (buildString s)
testUnifyPrefix s = query 1 basicBinAdd (unifyPrefix s)

unifyPrefix2 s = isRec $ isCons (UVar 0) (isCons (UVar 1) (buildString s))
testUnifyPrefix2 s = query 2 basicBinAdd (unifyPrefix2 s)

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
                 rulePrems = [isRecThen (Var 0) (Var 1),
                              isRecThen (Var 1) (Var 2)],
                 ruleConcl = isRecThen
                              (isCons (isChar '+') (Var 0))
                              (Var 2) }

buildString :: String -> Term CalcTerm VarId
buildString [] = isNil
buildString (s:xs) = isCons (isChar s) (buildString xs)

basicBinAdd = fromJust $ makeProg fnMap entryPoint
   where
      entryPoint = "isBinAdd"
      tacs = [HLApply recPlus,
              HLApply rec1,
              HLApply rec0,
              HLApply recNil]
      fnMap = Map.fromList
              [(entryPoint,
                hlFirst [ HLSeq x $ HCAll $ HLCall entryPoint | x <- tacs ])] {-
                HLSeq
                   (hlFirst tacs)
                   (HCAll $ HLCall entryPoint))]-}
