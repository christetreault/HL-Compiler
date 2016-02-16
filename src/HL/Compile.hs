{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module HL.Compile where

import Types
import Tactic
import HL
import Term
import Util
import Control.Monad.State
import qualified Data.Map as Map

{-
   For now, implementing without memoization. Will revisit
   this when I know it all works.
-}

-- The executable type of a tactic (Tactic) and a tactic continuation (TacticK)
-- In both cases, [m] should be a Monad with
-- 1) A State of [s] such that [Subst s v]
-- 2) A MonadPlus (used for errors)
type Tactic m v = Term v -> m [Term v]
type TacticK m v = [Term v] -> m [Term v]

-- Compile a program
compile :: (Subst VarId s, Monad m, MonadState s m, MonadPlus m, Show s) =>
           HLProg VarId -> Tactic m VarId
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
compileHl :: (Ord f, Eq v, Show f, Show v, Subst v s,
              Monad m, MonadState s m, MonadPlus m, Show s) =>
             HL v f -> Map.Map f (Tactic m v) -> Tactic m v
compileHl (HLApply r) _ = \gl -> do
   s <- get
   let (l, s') = fresh (ruleVars r) s
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl = v2u $ ruleConcl r
   case fnUnify concl gl s' of
     Nothing -> mzero
     Just s'' -> do
        put s''
        sequence $ fmap (\a -> instantiate $ v2u a) (rulePrems r)
compileHl (HLCall name) env =
    case name `Map.lookup` env of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just tac -> tac
compileHl HLFail _ = \_ -> mzero
compileHl HLIdTac _ = \gl -> return [gl]
compileHl (HLOr lhs rhs) env =
    let lhsT = compileHl lhs env in
    let rhsT = compileHl rhs env in
    \gl -> mplus (lhsT gl) (rhsT gl)
compileHl (HLSeq lhs rhs) env =
   let lhsT = compileHl lhs env in
   let rhsT = compileHc rhs env in
   \gl -> lhsT gl >>= rhsT
compileHl (HLAssert _ hl) env = compileHl hl env
compileHl (HLK hc) env =
    let hcT = compileHc hc env in
    \gl -> hcT [gl]

-- Compile a tactic continuation given the environment of defined tactics
compileHc :: (Ord f, Eq v, Show f, Show v, Subst v s,
              Monad m, MonadState s m, MonadPlus m, Show s) =>
             HC v f -> Map.Map f (Tactic m v) -> TacticK m v
compileHc (HCAll hl) env =
    let hlT = compileHl hl env in
    foldM (\ acc gl -> do res <- hlT gl
                          return (acc ++ res))
          []
compileHc (HCEach hls) env =
    let hlsT = fmap (\t -> compileHl t env) hls in
    \gls ->
        if length gls /= length hlsT
        then mzero
        else do
          let pairs = zip hlsT gls
          foldM (\ acc (t,gl) -> do res <- t gl
                                    return (acc ++ res))
                [] pairs
compileHc HCIdTacK _ = \gls -> return gls
compileHc HCFailK _ = \_ -> mzero
