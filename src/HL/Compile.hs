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

type CompileState = (Map.Map VarId (Maybe [Term VarId]), SubstEnv VarId)

compile :: HLProg VarId
           -> Term VarId
           -> Maybe [Term VarId]
compile prg t = do
   let compiled = sequence $ Map.mapWithKey compileHl $ progFns prg
   case (progEntry prg `Map.lookup` progNames prg) of
      Nothing -> impossible ("compile: "
                             ++ progEntry prg
                             ++ " not defined!")
      Just e -> case (e `Map.lookup` (compiled t)) of
         Nothing -> impossible ("compile: "
                                ++ progEntry prg
                                ++ " not compiled!")
         Just compiledFn -> evalState compiledFn (Map.empty, empty)

compileHl :: VarId
             -> HL VarId Integer
             -> Term VarId
             -> State CompileState (Maybe [Term VarId])
compileHl k (HLApply r) t = do
   (fs, env) <- get
   let (l, _) = fresh (ruleVars r) env
   let v2u = varsToUVars (\y -> UVar (l !! fromIntegral y))
   let concl' = v2u $ ruleConcl r
   let res = fnUnify concl' t env
   case res of
      Nothing -> do
         updateCSt k Nothing fs env
      Just env' -> do
         let ts = Just $ map v2u $ rulePrems r
         updateCSt k ts fs env'
compileHl k (HLCall name) t = do
   (fs, env) <- get
   let rhs = Map.lookup name fs
   case rhs of
      Nothing -> impossible ("HLCall: " ++ show name ++ " undefined!")
      Just ts -> return ts
compileHl k HLFail _ = addCSt k Nothing
compileHl k HLIdTac t = addCSt k $ Just [t]
compileHl k (HLOr lhs rhs) t = do
   lhs' <- compileHl k lhs t
   case lhs' of
      Nothing -> compileHl k rhs t
      lhs'' -> addCSt k lhs''
compileHl k (HLSeq lhs rhss) t = do
   lhs' <- compileHl k lhs t
   case lhs' of
      Nothing -> addCSt k Nothing
      Just gls -> compileHc k rhss gls
compileHl k (HLAssert _ hl) t = compileHl k hl t
compileHl k (HLK hc) t = compileHc k hc [t]


compileHc :: VarId
             -> HC VarId Integer
             -> [Term VarId]
             -> State CompileState (Maybe [Term VarId])
compileHc k (HCAll hl) ts = do
   ms <- sequence $ (compileHl k) <$> pure hl <*> ts
   addCSt k $ mconcat ms
compileHc k (HCEach hls) ts
   | length ts /= length hls = impossible "HCEach: |terms| ≠ |HLs|!"
   | otherwise = do
      let pairs = zip hls ts
      ms <- sequence $ map (uncurry (compileHl k)) pairs
      addCSt k $ mconcat ms
compileHc k HCIdTacK ts = addCSt k $ Just ts
compileHc k HCFailK _ = addCSt k Nothing

updateCSt :: VarId
             -> Maybe [Term VarId]
             -> Map.Map VarId (Maybe [Term VarId])
             -> SubstEnv VarId
             -> State CompileState (Maybe [Term VarId])
updateCSt k ts fs env = do
   let curr = case (Map.lookup k fs) of
          Nothing -> ts
          Just ts' -> ts `mappend` ts'
   put (Map.insert k curr fs, env)
   return ts

addCSt k ts = do
   (fs, env) <- get
   updateCSt k ts fs env
