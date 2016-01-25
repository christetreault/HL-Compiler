module Term where

import Types
import Util

instantiateTerm :: (a -> Term a) -> Term a -> Term a
instantiateTerm lf (App f xs) = App f $ fmap (instantiateTerm lf) xs
instantiateTerm lf (UVar u) = lf u
instantiateTerm _ v = v

varsToUVars :: (Integer -> Term t) -> Term u -> Term t
varsToUVars on (App f xs) = App f $ fmap (varsToUVars on) xs
varsToUVars on (Var v) = on v
varsToUVars _ _ = impossible "varsToUVars called on UVar"
