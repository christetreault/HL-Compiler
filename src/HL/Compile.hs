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




compileHl :: Term VarId
             -> HL VarId Integer
             -> State (SubstEnv VarId) (Maybe [Term VarId])
compileHl t (HLApply r) = undefined
compileHl t (HLCall name) = do
   env <- get
   let sub = lkup env name
   case sub of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just t' -> return $ Just [t']
compileHl _ HLFail = return Nothing
compileHl t HLIdTac = return $ Just [t]
compileHl t (HLOr lhs rhs) = do
   lhs' <- compileHl t lhs
   case lhs' of
      Nothing -> compileHl t rhs
      lhs'' -> return lhs''
compileHl t (HLSeq lhs rhss) = do
   lhs' <- compileHl t lhs
   case lhs' of
      Nothing -> return Nothing
      Just gls -> compileHc gls rhss
compileHl t (HLAssert _ hl) = compileHl t hl
compileHl t (HLK hc) = compileHc [t] hc


compileHc :: [Term VarId]
             -> HC VarId Integer
             -> State (SubstEnv VarId) (Maybe [Term VarId])
compileHc ts (HCAll hl) = do
   ms <- sequence $ compileHl <$> ts <*> pure hl
   return $ mconcat ms
compileHc ts (HCEach hls)
   | length ts /= length hls = impossible "HCEach: |terms| ≠ |HLs|!"
   | otherwise = do
      let pairs = zip ts hls
      ms <- sequence $ map (uncurry compileHl) pairs
      return $ mconcat ms
compileHc ts HCIdTacK = return $ Just ts
compileHc _ HCFailK = return Nothing
