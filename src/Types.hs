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

data TermMap a =
   TermMap (Map.Map VarId (Term a)) Integer

class Subst key subst | subst -> key where
   fresh :: Integer -> subst -> ([key], subst)
   empty :: subst
   lookup :: subst -> key -> Maybe (Term key)
   insert :: key -> Term key -> subst -> Maybe subst

instance Subst VarId (TermMap VarId) where
   fresh n (TermMap m i) = ([i .. (i + n)], TermMap m (i + n))
   empty = TermMap Map.empty 0
   lookup (TermMap m _) k = Map.lookup k m
   insert k t (TermMap m i) = if k `Map.member` m
                       then Nothing
                       else Just $ TermMap (Map.insert k t m) i
