{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
            Monad m, MonadLogic m, MonadState s m, MonadPlus m, Show s, Pretty v)
           => HLProg v VarId
           -> Tactic m v VarId
compile prg =
    case progEntry prg `Map.lookup` progNames prg of
      Nothing ->
          impossible $ "compile: '" ++ progEntry prg ++ "' is not defined"
      Just e -> case e `Map.lookup` env of
                  Nothing -> impossible $ "compile: invariant violation"
                  Just entry -> entry
    where
      env = fmap (\t -> compileHl t env) $ progFns prg

-- Compile a tactic given the environment of defined tactics
compileHl :: (Ord f, Eq v, Eq val, Show f, Pretty f, Show v, Subst v (Term val v) s,
              Monad m, MonadLogic m, MonadState s m, MonadPlus m, Show s, Pretty v, Pretty val)
             => HL val v f -> Map.Map f (Tactic m val v) -> Tactic m val v
compileHl (HLApply r) _ = \gl -> do
   s <- get
   let (l, s') = fresh (ruleVars r) s
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl = v2u $ ruleConcl r
   s'' <- fnUnify concl gl s'
   put s''
   sequence $ fmap (\a -> instantiate $ v2u a) (rulePrems r)
compileHl (HLCall name) env =
    case name `Map.lookup` env of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just tac -> tac
compileHl HLFail _ = \_ -> mzero
compileHl HLIdTac _ = \gl -> return [gl]
compileHl (HLOr lhs rhs) env =
   compileHl (HLOnce $ HLPlus lhs rhs) env
compileHl (HLPlus lhs rhs) env =
   let lhsT = compileHl lhs env in
   let rhsT = compileHl rhs env in
   \gl -> interleave (lhsT gl) (rhsT gl)
compileHl (HLOnce h) env = \gl -> once $ compileHl h env gl
compileHl (HLSeq lhs rhs) env =
   let lhsT = compileHl lhs env in
   let rhsT = compileHc rhs env in
   \gl -> lhsT gl >>- rhsT
compileHl (HLAssert _ hl) env = compileHl hl env
compileHl (HLK hc) env =
   let hcT = compileHc hc env in
   \gl -> hcT [gl]

-- Compile a tactic continuation given the environment of defined tactics
compileHc :: (Ord f, Eq v, Eq val, Show f, Pretty f, Show v, Subst v (Term val v) s,
              Monad m, MonadState s m, MonadPlus m, MonadLogic m, Show s, Pretty val, Pretty v)
             => HC val v f
             -> Map.Map f (Tactic m val v)
             -> TacticK m val v
compileHc (HCAll hl) env =
    let hlT = compileHl hl env in
    foldM (\ acc gl ->
              let res = hlT gl in
              let final r = return (acc ++ r) in
              res >>- final)
          []
compileHc (HCEach hls) env =
    let hlsT = fmap (\t -> compileHl t env) hls in
    \gls ->
        if length gls /= length hlsT
        then mzero
        else
           let pairs = zip hlsT gls in
           foldM (\ acc (t,gl) -> let res x = x gl in
                                  let final x = return $ acc ++ x in
                                  res t >>- final)
                [] pairs
compileHc HCIdTacK _ = \gls -> return gls
compileHc HCFailK _ = \_ -> mzero
