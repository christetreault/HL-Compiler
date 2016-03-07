module Main where

import HL.Compile
import HL.Optimize
import HL
import Demo
import PnCalcDemo
import Term
import Control.Monad.State
import Criterion.Main

main :: IO ()
main = do
   defaultMain [env (return $ mkN (10000 :: Integer)) $
                    \ ~(t) -> bgroup "Even-Odd"
                              [bench "Standard" $ nf tac_evenOdd t,
                               bench "Manual" $ nf tac_evenOddMan t,
                               bench "Optimized" $ nf tac_evenOddOpt t]
                ,
                env (return $ buildString reallyLong) $
                    \ ~(t) -> bgroup "PN Calculator"
                              [bench "Standard" $ nf tryBasicBin t,
                               bench "Optimized" $ nf tryOptBasicBin t]
               ]

tac_evenOdd t = tac t evenOddTac
tac_evenOddMan t = tac t evenOddTacOpt
tac_evenOddOpt t = tac t $ normalizeProg evenOddTac

tac :: Term Integer VarId -> HLProg Integer VarId ->Maybe [Term Integer VarId]
tac t p = evalStateT (action t) empty
   where
      action :: Term Integer VarId
                -> StateT (SubstEnv Integer VarId) Maybe [Term Integer VarId]
      action = compileMaybe p

{-
   do
   let n = 1
   let compiled = doN n (tac_evenOddOpt)
   putStrLn $ show compiled
   return ()



 -}



doN :: Int
       -> (Term Integer VarId -> Maybe [Term Integer VarId])
       -> Maybe [Term Integer VarId]
doN n t = do
   let problem = mkEven $ mkN (10000 :: Integer)
   let repetitions = take n $ repeat (t problem)
   mconcat $ repetitions


{-

implement coq 8.5 + (maybe m arg inn compile should be a list instead of maybe)
implement coq once combinator

query: run tactic on Term [something] (?var)
   - should say yes or no
   - should print out what subst makes it unify
   - need to know number of vars, fresh with that much
- queryTac
  {
      run fresh (number of free vars)
      tryTac (with original term) -- should not drop state
      if success, instantiate term with its resulting substitution from resulting state
- embed haskell functions into the langauge


  }


-}
