module Main where

import HL.Compile
import HL.Optimize
import HL
import Demo
import Term
import Control.Monad.State

main :: IO ()
main = do
   let n = 1
   let compiled = doN n (tac_evenOddOpt)
   putStrLn $ show compiled
   return ()

tac_evenOdd t = tac t evenOddTac
tac_evenOddMan t = tac t evenOddTacOpt
tac_evenOddOpt t = tac t $ normalizeProg evenOddTac

tac :: Term Integer VarId -> HLProg Integer VarId ->Maybe [Term Integer VarId]
tac t p = evalStateT (action t) empty
   where
      action :: Term Integer VarId -> StateT (SubstEnv Integer VarId) Maybe [Term Integer VarId]
      action = compile p



doN :: Int
       -> (Term Integer VarId -> Maybe [Term Integer VarId])
       -> Maybe [Term Integer VarId]
doN n t = do
   let problem = mkEven $ mkN (10000 :: Integer)
   let repetitions = take n $ repeat (t problem)
   mconcat $ repetitions
