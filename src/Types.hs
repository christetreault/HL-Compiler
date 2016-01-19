{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

type VarId = Integer

data Term a =
   App Int [Term a]
   | Var VarId
   | UVar a
   deriving (Eq, Ord)

{-
data RefSelf a =
   Cyc (RefSelf (Maybe (Term a)))
   deriving (Eq, Ord)
-}

data Rule a =
   Rule { vars :: VarId,
          prems :: [Term a],
          concl :: Term a}
   deriving (Eq, Ord)

type TermMap a = Map.Map VarId (Term a)

class Subst key subst where
   fresh :: Integer -> subst -> ([key], subst)
   empty :: subst
   lookup :: subst -> key -> Maybe (Term key)
   insert :: key -> Term key -> subst -> Maybe subst

instance Subst VarId ((TermMap VarId), Integer) where
   fresh n (m, i) = ([i .. (i + n)], (m, i + n))
   empty = (Map.empty, 0)
   lookup (m, _) k = Map.lookup k m
   insert k t (m, i) = if k `Map.member` m
                       then Nothing
                       else Just ((Map.insert k t m), i)
