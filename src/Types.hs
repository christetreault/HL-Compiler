{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import qualified Data.Map.Strict as Map

type VarId = String

data Term a =
   App Int [Term a]
   | Var VarId
   | UVar a
   deriving (Eq, Ord)

data RefSelf a =
   Cyc (RefSelf (Maybe (Term a)))
   deriving (Eq, Ord)

data Rule a =
   Rule { vars :: VarId,
          prems :: [Term a],
          concl :: Term a}
   deriving (Eq, Ord)

type TermMap a = Map.Map VarId (Term a)

class Subst key subst where
   fresh :: VarId -> subst -> ([key], subst)
   empty :: subst
   lookup :: subst -> key -> Maybe (Term key)
   insert :: key -> Term key -> subst -> Maybe subst
