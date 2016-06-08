module Demo.Std where

import Util
import Term

import Data.List (nub)

type StringTerm = (Term String VarId)

termToInt :: StringTerm -> Int
termToInt = tti 0
   where
      tti n (App "zero" []) = n
      tti n (App "succ" [t]) = tti (n + 1) t
      tti _ _ = impossible "Must be a mkZero or mkSucc!"

mkZero :: StringTerm
mkZero = App "zero" []

mkSucc :: StringTerm -> StringTerm
mkSucc t = App "succ" [t]

mkN :: Integer -> StringTerm
mkN n
   | n >= 0 = case n of
                 0 -> mkZero
                 _ -> mkSucc $ mkN $ n - 1
   | otherwise = impossible "n must be positive"

mkNil :: StringTerm
mkNil = App "nil" []

mkCons :: StringTerm -> StringTerm -> StringTerm
mkCons l r = App "cons" [l, r]

mkList :: [StringTerm] -> StringTerm
mkList [] = mkNil
mkList (x:xs) = mkCons x $ mkList xs

mkNth xs n x = App "nth" [xs, n, x]

-- | Construct a rule; calculates the number of Vars in the rule automatically
infixl 5 =>>
prems =>> concl = Rule { ruleVars = (countVars prems concl),
                         rulePrems = prems,
                         ruleConcl = concl }
   where
      countVars :: [StringTerm] -> StringTerm -> Integer
      countVars p c = toInteger $ length $ nub $ concat $ fmap getVars (c:p)
         where
            getVars (UVar _) = impossible "UVar in call to countVars!"
            getVars (App _ ts) = concat $ fmap getVars ts
            getVars v = [v]
