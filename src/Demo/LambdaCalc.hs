module Demo.LambdaCalc where

import Util
import Term
import Demo.Std

import Test.Tasty
import Criterion.Main
import Debug.Trace
import Text.PrettyPrint.HughesPJClass


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

tau ::= tau1 -> tau2
      | ..
e::= λ τ . e
   | e1 e2
   | #n

-}


mkAbs t e = App "abs" [(mkType t), e]
   where
      mkType t = App t []

mkApp e1 e2 = App "app" [e1, e2]
mkVar n = App "var" [mkN n]
mkArr a b = App "arr" [a, b]



{-

examples:

λ int . #0

λ int . λ bool . #0
(in haskell: fun x y -> y

rules:

----------------
nth (x:xs) O x

nth xs n y
-------------------
nth (x:xs) (S n) y

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
   Type (Term String VarId)
   | Error
     deriving (Ord, Eq, Show)

typeCheck t = typeCheck' [] t

typeCheck' g (App "var" [n']) =
   let n = termToInt n' in
   if (length g > n)
   then Type (g !! n)
   else Error {- Undefined variable -}
typeCheck' g (App "app" [e1, e2]) =
   let lhs = typeCheck' g e1 in
   let rhs = typeCheck' g e2 in
   case (lhs, rhs) of
      (Type (App "arr" [d,r]), Type d') ->
         if d == d' then Type r
         else Error {- applied a function of type [a -> b] to a [c] -}
      (Type _ , Type _) -> Error {- e1 is not a function -}
      (_ , _) -> Error {- one of the subterms does not type check -}
typeCheck' g f@(App "abs" [t, e]) =
   case typeCheck' (t:g) e of
      Type t' -> Type (mkArr t t')
      Error -> Error {- body does not type check -}
typeCheck' g f = impossible ("Got:\n"
                             ++ show (g) ++ "\n"
                             ++ show f)



----------------------------------------------------------------------
-- Tests - TODO
----------------------------------------------------------------------

lambdaCalcBenchSuite :: Benchmark
lambdaCalcBenchSuite = todo "Write benchmark suite"

lambdaCalcTestSuite :: TestTree
lambdaCalcTestSuite = todo "Write test suite"
