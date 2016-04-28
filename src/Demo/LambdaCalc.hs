module Demo.LambdaCalc where

import Util
import Term

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

-}

mkAbs f t e = App "abs" [t, e]
mkApp e1 e2 = App "app" [e1, e2]
mkVar n = App "var" [n]

mkType t = App t []

{-

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
-- Haskell implementation
----------------------------------------------------------------------

data TCResult =
   Type String
   | Free
   | Error

typeCheck g (App "var" [Number n]) = if (length g >= n)
                                        -- lookup the nth element of g
                                     then undefined
                                     else Free
typeCheck g (App "app" [e1, e2]) = undefined
                                   -- if e1 has a function type, result is
                                   -- applying e2 to e1, otherwise error
typecheck g (App "abs" [t, e]) = undefined
                                 -- type is function from t -> [type of e]

--type Zipper = ([Branch], Term String VarId)

--data Branch =
--   BinaryLHS
--   | BinaryRHS
--   | UnaryAbs
--   | UnaryVar



----------------------------------------------------------------------
-- Tests - TODO
----------------------------------------------------------------------

lambdaCalcBenchSuite :: Benchmark
lambdaCalcBenchSuite = todo "Write benchmark suite"

lambdaCalcTestSuite :: TestTree
lambdaCalcTestSuite = todo "Write test suite"
