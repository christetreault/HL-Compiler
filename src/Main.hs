module Main where

import HL.Compile
import HL
import Demo
import Types
import Control.Monad.State

main :: IO ()
main = do
   let n = 1
   putStrLn $ show (evenOddTac :: HLProg VarId)
   let compiled = doN n tac
   putStrLn $ show compiled
   return ()

tac :: Term VarId -> Maybe [Term VarId]
tac t = evalStateT (action t) empty
   where
      action :: Term VarId -> StateT (SubstEnv VarId) Maybe [Term VarId]
      action = compile evenOddTac

{-tacOpt = compile evenOddTacOpt-}

doN :: Int
       -> (Term VarId -> Maybe [Term VarId])
       -> Maybe [Term VarId]
doN n t = do
   let problem = mkEven $ mkN (10000 :: Integer)
   let repetitions = take n $ repeat (t problem)
   mconcat $ repetitions
