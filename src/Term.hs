{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Term where

import qualified Data.Map as Map
import Util

type VarId = Integer

data Term b a =
   App b [Term b a]
   | Var VarId
   | UVar a
   deriving (Eq, Ord, Show)

data Rule b a =
   Rule { ruleVars :: VarId,
          rulePrems :: [Term b a],
          ruleConcl :: Term b a}
   deriving (Eq, Ord, Show)

data SubstEnv b a =
   SubstEnv (Map.Map VarId (Term b a)) Integer
   deriving (Show)

class Subst key val subst | subst -> key , subst -> val where
   fresh :: Integer -> subst -> ([key], subst)
   empty :: subst
   lkup :: subst -> key -> Maybe val
   inst :: key -> val -> subst -> Maybe subst

instance Subst VarId (Term val VarId) (SubstEnv val VarId) where
   fresh n (SubstEnv m i) = ([i .. (i + n)], SubstEnv m (i + n))
   empty = SubstEnv Map.empty 0
   lkup (SubstEnv m _) k = Map.lookup k m
   inst k t (SubstEnv m i) = if k `Map.member` m
                             then Nothing
                             else Just $ SubstEnv (Map.insert k t m) i

instantiateTerm :: (a -> Term v a) -> Term v a -> Term v a
instantiateTerm lf (App f xs) = App f $ fmap (instantiateTerm lf) xs
instantiateTerm lf (UVar u) = lf u
instantiateTerm _ v = v

varsToUVars :: (Integer -> Term v t) -> Term v u -> Term v t
varsToUVars on (App f xs) = App f $ fmap (varsToUVars on) xs
varsToUVars on (Var v) = on v
varsToUVars _ _ = impossible "varsToUVars called on UVar"
