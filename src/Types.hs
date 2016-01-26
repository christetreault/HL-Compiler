{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Control.Monad.State

type VarId = Integer

data Term a =
   App Integer [Term a]
   | Var VarId
   | UVar a
   deriving (Eq, Ord, Show)

{-
data RefSelf a =
   Cyc (RefSelf (Maybe (Term a)))
   deriving (Eq, Ord)
-}

data Rule a =
   Rule { vars :: VarId,
          prems :: [Term a],
          concl :: Term a}
   deriving (Eq, Ord, Show)

data SubstEnv a =
   SubstEnv (Map.Map VarId (Term a)) Integer

class Subst key subst | subst -> key where
   fresh :: Integer -> subst -> ([key], subst)
   empty :: subst
   lkup :: subst -> key -> Maybe (Term key)
   inst :: key -> Term key -> subst -> Maybe subst

instance Subst VarId (SubstEnv VarId) where
   fresh n (SubstEnv m i) = ([i .. (i + n)], SubstEnv m (i + n))
   empty = SubstEnv Map.empty 0
   lkup (SubstEnv m _) k = Map.lookup k m
   inst k t (SubstEnv m i) = if k `Map.member` m
                             then Nothing
                             else Just $ SubstEnv (Map.insert k t m) i

type WithSubst = StateT

runWithSubst m = fst $ runState m empty
