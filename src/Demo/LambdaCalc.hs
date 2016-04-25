module Demo.LambdaCalc where

import Util

import Test.Tasty
import Criterion.Main

{-
definitions:

  λ - a function
  τ - type variable
  Γ - environment of variables in context
  ⊢ - P ⊢ Q --> "From P, I know that Q"
    P | Q | P ⊢ Q
    --+---+------
    F | F |   T (vaccuously true)
    F | T |   T (vaccuously true)
    T | F |   F
    T | T |   T
  . - function application
  : - a : b --> a is of type b
  :: - cons (it is probably a bit more common to use the symbol ',')
  # - #n --> a variable n places to the left

grammar:

e::= λ τ . e
   | e1 e2
   | #n

examples:

λ int . #0

λ int . λ bool . #0
(in haskell: fun x y -> y

rules:

nth Γ n τ
---------- Tvar
Γ ⊢ #n : τ

Γ ⊢ e1 : τ' -> τ
Γ ⊢ e2 : τ'
---------------- Tapp
Γ ⊢ e1 e2 : τ

Γ :: τ ⊢ e : τ'
--------------------- Tabs
Γ ⊢ λ τ . e : τ -> τ'

-}

----------------------------------------------------------------------
-- Tests - TODO
----------------------------------------------------------------------

lambdaCalcBenchSuite :: Benchmark
lambdaCalcBenchSuite = todo "Write benchmark suite"

lambdaCalcTestSuite :: TestTree
lambdaCalcTestSuite = todo "Write test suite"
