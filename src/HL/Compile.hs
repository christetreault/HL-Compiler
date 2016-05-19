{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module HL.Compile where

import Tactic
import HL
import Term
import Util
import Control.Monad.State
import Control.Monad.Logic.Class
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJClass

-- The executable type of a tactic (Tactic) and a tactic continuation (TacticK)
-- In both cases, [m] should be a Monad with
-- 1) A State of [s] such that [Subst s v]
-- 2) A MonadPlus (used for errors)
type Tactic m v s = Term v s -> m [Term v s]
type TacticK m v s = [Term v s] -> m [Term v s]

-- Compile a program
compile :: (Subst VarId (Term v VarId) s, Eq v,
            Monad m, MonadLogic m, MonadState s m, MonadPlus m, Pretty v)
           => (forall a. m a -> m a)
           -> HLProg v VarId
           -> Tactic m v VarId
compile delay prg =
    case progEntry prg `Map.lookup` progNames prg of
      Nothing ->
          impossible $ "compile: '" ++ progEntry prg ++ "' is not defined"
      Just e -> case e `Map.lookup` env of
                  Nothing -> impossible $ "compile: invariant violation"
                  Just entry -> entry
    where
      env = fmap (compileHl delay env) $ progFns prg

-- Compile a tactic given the environment of defined tactics
compileHl :: (Ord f, Eq v, Eq val, Show f, Pretty f, Show v, Subst v (Term val v) s,
              Monad m, MonadLogic m, MonadState s m, MonadPlus m, Pretty v, Pretty val)
             => (forall a. m a -> m a)
             -> Map.Map f (Tactic m val v)
             -> HL val v f
             -> Tactic m val v
compileHl _ _ (HLApply r) = \gl -> do
   s <- get
   let (l, s') = fresh (ruleVars r) s
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl = v2u $ ruleConcl r
   s'' <- fnUnify concl gl s'
   put s''
   sequence $ fmap (\a -> instantiate $ v2u a) (rulePrems r)
compileHl delay env (HLCall name) =
    case name `Map.lookup` env of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just tac -> \gl -> delay $ tac gl
compileHl _ _ HLFail = \_ -> mzero
compileHl _ _ HLIdTac = \gl -> return [gl]
compileHl delay env (HLOr lhs rhs) =
    compileHl delay env (HLOnce $ HLPlus lhs rhs)
compileHl delay env (HLPlus lhs rhs) =
   let lhsT = compileHl delay env lhs in
   let rhsT = compileHl delay env rhs in
   \gl -> (lhsT gl) `interleave` (rhsT gl)
compileHl delay env (HLOnce h) =
   let lstT = compileHl delay env h in
   \gl -> once $ lstT gl
compileHl delay env (HLSeq lhs rhs) =
   let lhsT = compileHl delay env lhs in
   let rhsT = compileHc delay env rhs in
   \gl -> lhsT gl >>- rhsT
compileHl delay env (HLAssert _ hl) = compileHl delay env hl
compileHl delay env (HLK hc) =
   let hcT = compileHc delay env hc in
   \gl -> hcT [gl]

-- Compile a tactic continuation given the environment of defined tactics
compileHc :: (Ord f, Eq v, Eq val, Show f, Pretty f, Show v, Subst v (Term val v) s,
              Monad m, MonadState s m, MonadPlus m, MonadLogic m, Pretty val, Pretty v)
             => (forall a. m a -> m a)
             -> Map.Map f (Tactic m val v)
             -> HC val v f
             -> TacticK m val v
compileHc delay env (HCAll hl) =
    let hlT = compileHl delay env hl in
    foldM (\ acc gl ->
              let res = hlT gl in
              let final r = return (acc ++ r) in
              res >>- final)
          []
compileHc delay env (HCEach hls) =
    let hlsT = fmap (compileHl delay env) hls in
    \gls ->
        if length gls /= length hlsT
        then mzero
        else
           let pairs = zip hlsT gls in
           foldM (\ acc (t,gl) -> let res x = x gl in
                                  let final x = return $ acc ++ x in
                                  res t >>- final)
                [] pairs
compileHc _ _ HCIdTacK = \gls -> return gls
compileHc _ _ HCFailK = \_ -> mzero
