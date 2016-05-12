module Demo.Std where

import Util
import Term

type TermInt = (Term String VarId)

termToInt :: TermInt -> Int
termToInt = tti 0
   where
      tti n (App "zero" []) = n
      tti n (App "succ" [t]) = tti (n + 1) t
      tti _ _ = impossible "Must be a mkZero or mkSucc!"

mkZero :: TermInt
mkZero = App "zero" []

mkSucc :: TermInt -> TermInt
mkSucc t = App "succ" [t]

mkN :: Integer -> TermInt
mkN n
   | n >= 0 = case n of
                 0 -> mkZero
                 _ -> mkSucc $ mkN $ n - 1
   | otherwise = impossible "n must be positive"
