module HL.Compile where

import Types
import Tactic
import HL
import Control.Monad.State
import qualified Data.Map.Strict as Map

{-
   For now, implementing without memoization. Will revisit
   this when I know it all works.
-}


compile :: (MonadState s m, Subst k s)
           => HLProg k
           -> m (Maybe [Term k])
compile prg = undefined



compileHl :: (Subst k s)
             => HL k Integer
             -> State s (Maybe [Term k])
compileHl hl = undefined

compileHc :: (Subst k s)
             => HC k Integer
             -> State s (Maybe [Term k])
compileHc (HCAll hl) = compileHl hl
compileHc (HCEach hls) = do
   res <- sequence $ map compileHl hls
   return $ mconcat res
compileHc HCIdTacK = return $ Just []
compileHc HCFailK = return Nothing
