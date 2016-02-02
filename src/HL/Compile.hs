module HL.Compile where

import Types
import Tactic
import HL
import Control.Monad.State
import qualified Data.Map as Map

{-
   For now, implementing without memoization. Will revisit
   this when I know it all works.
-}


compile :: (MonadState s m, Subst k s)
           => HLProg k
           -> m (Maybe [Term k])
compile prg = undefined



compileHl :: HL k Integer
             -> State (SubstEnv VarId) (Maybe [Term k])
compileHl (HLCall name) = do
   env <- get
   let sub = lkup env name
   undefined

compileHl HLFail = return Nothing


compileHc :: HC k Integer
             -> State (SubstEnv VarId) (Maybe [Term k])
compileHc (HCAll hl) = compileHl hl
compileHc (HCEach hls) = do
   res <- sequence $ map compileHl hls
   return $ mconcat res
compileHc HCIdTacK = return $ Just []
compileHc HCFailK = return Nothing
