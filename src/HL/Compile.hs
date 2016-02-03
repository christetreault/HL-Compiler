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
           -> State (Term VarId) (Maybe [Term VarId])
compile prg = undefined




compileHl :: HL VarId Integer
             -> Term VarId
             -> State (SubstEnv VarId) (Maybe [Term VarId])
compileHl (HLApply r) t = do
   env <- get
   let (l, x) = fresh (ruleVars r) env
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl' = v2u $ ruleConcl r
   res <- unify concl' t
   if res
      then
      return $ Just $ map v2u $ rulePrems r
      else
      return Nothing
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
      Just gls -> compileHc gls rhss
compileHl (HLAssert _ hl) t = compileHl hl t
compileHl (HLK hc) t = compileHc [t] hc


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
