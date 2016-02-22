module Term where

import Types
import Util

instantiateTerm :: (a -> Term v a) -> Term v a -> Term v a
instantiateTerm lf (App f xs) = App f $ fmap (instantiateTerm lf) xs
instantiateTerm lf (UVar u) = lf u
instantiateTerm _ v = v

varsToUVars :: (Integer -> Term v t) -> Term v u -> Term v t
varsToUVars on (App f xs) = App f $ fmap (varsToUVars on) xs
varsToUVars on (Var v) = on v
varsToUVars _ _ = impossible "varsToUVars called on UVar"
