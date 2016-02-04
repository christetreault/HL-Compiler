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


compile :: HLProg VarId
           -> Term VarId
           -> State (SubstEnv VarId) (Maybe [Term VarId])
compile prg = do
   compiled <- return $ fmap compileHl $ progFns prg
   case (progEntry prg `Map.lookup` progNames prg) of
      Nothing -> impossible ("compile: "
                             ++ progEntry prg
                             ++ " not defined!")
      Just e -> case (e `Map.lookup` compiled) of
         Nothing -> impossible ("compile: "
                                ++ progEntry prg
                                ++ " not compiled!")
         Just compiledFn -> compiledFn

compileHl :: HL VarId Integer
             -> Term VarId
             -> State (SubstEnv VarId) (Maybe [Term VarId])
compileHl (HLApply r) t = do
   env <- get
   let (l, _) = fresh (ruleVars r) env
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl' = v2u $ ruleConcl r
   res <- unify concl' t
   if res
      then
      return $ Just $ map v2u $ rulePrems r
      else do
         put env        -- Should be superfluous, but doing anyways in case the
         return Nothing -- behavior of unify changes
compileHl (HLCall name) t = do
   env <- get
   let sub = lkup env name
   case sub of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just t' -> return $ Just [t']
compileHl HLFail _ = return Nothing
compileHl HLIdTac t = return $ Just [t]
compileHl (HLOr lhs rhs) t = do
   lhs' <- compileHl lhs t
   case lhs' of
      Nothing -> compileHl rhs t
      lhs'' -> return lhs''
compileHl (HLSeq lhs rhss) t = do
   lhs' <- compileHl lhs t
   case lhs' of
      Nothing -> return Nothing
      Just gls -> compileHc rhss gls
compileHl (HLAssert _ hl) t = compileHl hl t
compileHl (HLK hc) t = compileHc hc [t]


compileHc :: HC VarId Integer
             -> [Term VarId]
             -> State (SubstEnv VarId) (Maybe [Term VarId])
compileHc (HCAll hl) ts = do
   ms <- sequence $ compileHl <$> pure hl <*> ts
   return $ mconcat ms
compileHc (HCEach hls) ts
   | length ts /= length hls = impossible "HCEach: |terms| ≠ |HLs|!"
   | otherwise = do
      let pairs = zip hls ts
      ms <- sequence $ map (uncurry compileHl) pairs
      return $ mconcat ms
compileHc HCIdTacK ts = return $ Just ts
compileHc HCFailK _ = return Nothing


smartK :: HC a b -> HL a b
smartK HCIdTacK = HLIdTac
smartK (HCAll x) = x
smartK (HCEach [x]) = x
smartK _ = HLFail

smartSeq :: HL a b -> HC a b -> HL a b
smartSeq HLIdTac r = smartK r
smartSeq HLFail _ = HLFail
smartSeq x HCIdTacK = x
smartSeq x y = HLSeq x y

smartAll :: HL a b -> HC a b
smartAll HLIdTac = HCIdTacK
smartAll HLFail = HCFailK
smartAll (HLK k) = k
smartAll x = HCAll x

normalizeHl :: HC a b -> HL a b -> HL a b
normalizeHl k (HLOr l r) = HLOr (normalizeHl k l) (normalizeHl k r)
normalizeHl _ HLFail = HLFail
normalizeHl k HLIdTac = smartK k
normalizeHl k (HLApply r)
   | length (rulePrems r) == 0 = HLApply r
   | otherwise = undefined
normalizeHl k (HLSeq c cc) = normalizeHl (normalizeHc k cc) c
normalizeHl k (HLCall n) = smartSeq (HLCall n) k
normalizeHl k (HLK r) = smartK (normalizeHc k r)
normalizeHl k (HLAssert t c) = HLAssert t $ normalizeHl k c

normalizeHc = undefined
