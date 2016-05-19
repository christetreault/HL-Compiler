{-# LANGUAGE DeriveGeneric #-}

module Demo.PNCalc where

import HL
import HL.Optimize
import HL.Query
import Control.Monad.State
import Term
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Control.DeepSeq
import GHC.Generics
import Criterion.Main
import Test.Tasty
import Test.Tasty.HUnit

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
testUnifyPrefix s = query (Just 100) basicBinAdd (unifyPrefix s)

unifyPrefix2 s = isRec $ isCons (UVar 0) (isCons (UVar 1) (buildString s))
testUnifyPrefix2 s = query (Just 100) basicBinAdd (unifyPrefix2 s)

unifySuffix2 = isRec
               $ isCons (isChar '+') (isCons (UVar 0) (isCons (UVar 1) (isNil)))
testUnifySuffix2 = query (Just 100) basicBinAdd unifySuffix2

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

recString = isRec . buildString

basicBinAdd = fromJust $ makeProg fnMap entryPoint
   where
      entryPoint = "isBinAdd"
      tacs = [HLApply rec1,
              HLApply rec0,
              HLApply recNil,
              HLApply recPlus]
      fnMap = Map.fromList
              [(entryPoint,
                hlFirst [ HLSeq x $ HCAll $ HLCall entryPoint | x <- tacs ])]

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

pnCalcBenchSuite :: Benchmark
pnCalcBenchSuite =
   env (return $ buildString reallyLong) $
   \ ~(t) -> bgroup "PN Calculator"
             [bench "Standard" $ nf (basicBinAdd `unifies`) t,
              bench "Optimized" $ nf ((normalizeProg basicBinAdd) `unifies`) t]

pnCalcTestSuite :: TestTree
pnCalcTestSuite = testGroup "PN Calculator"
                  [testCase "Empty string not recognized" testEmpty,
                   testCase "One recognized" (testChar '1'),
                   testCase "Zero recognized" (testChar '0'),
                   testCase "++111 recognized" testAdd,
                   testCase "Really long string recognized" testRL,
                   testCase "+(?0)(?1) query success" testQuery
                  ]
   where
      testEmpty = (basicBinAdd `unifies` (recString "")) @?= False
      testChar c = (basicBinAdd `unifies` (recString [c])) @?= True
      testAdd = (basicBinAdd `unifies` (recString "++111")) @?= True
      testRL = (basicBinAdd `unifies` (recString reallyLong)) @?= True
      testQuery = let (QueryYes us2 solns) = testUnifySuffix2 in
                  (length solns, us2) @?= (4, unifySuffix2)
