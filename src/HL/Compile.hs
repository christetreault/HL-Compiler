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



compileHl :: (MonadState s m, Subst k s)
             => HL k Integer
             -> m (Maybe [Term k])
compileHl hl = undefined

compileHc :: (MonadState s m, Subst k s)
             => HC k Integer
             -> m (Maybe [Term k])
compileHc (HCAll hc) = undefined
