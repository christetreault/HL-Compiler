module Demo.LambdaCalc where

import Util
import Term

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

e::= λ τ . e
   | e1 e2
   | #n

-}

termToInt :: TermInt -> Int
termToInt = tti 0
   where
      tti n (App "zero" []) = n
      tti n (App "succ" [t]) = tti (n + 1) t
      tti _ _ = impossible "Must be a mkZero or mkSucc!"

type TermInt = (Term String VarId)

mkZero = App "zero" []
mkSucc t = App "succ" [t]

mkN :: Integer -> TermInt
mkN n
   | n >= 0 = case n of
                 0 -> mkZero
                 _ -> mkSucc $ mkN $ n - 1
   | otherwise = impossible "n must be positive"


mkAbs t e = App "abs" [t, e]
mkApp e1 e2 = App "app" [e1, e2]
mkVar n = App "var" [mkN n]

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
   Type (Term String VarId)
   | Function (Term String VarId) TCResult
   | Free Int
   | Error TCResult TCResult
     deriving (Ord, Eq, Show)

typeCheck t = typeCheck' [] t

typeCheck' g (App "var" [n']) = --trace (show g) $
   let n = termToInt n' in
   if (length g > n)
   then Type (g !! n)
   else Free n
typeCheck' g (App "app" [e1, e2]) = --trace (show g) $
   let lhs = typeCheck' (e1:g) e1 in
   let rhs = typeCheck' (e2:g) e2 in
   case (lhs, rhs) of
      ((Error _ _), rhs) -> Error lhs rhs
      (lhs, (Error _ _)) -> Error lhs rhs
      (f@(Function lhsT rhsT), Free _) -> f
      (Function lhsT rhsT, (Type t)) ->
         if lhsT == t
         then rhsT
         else Error (Function lhsT rhsT) (Type t)
      _ -> Error lhs rhs
typeCheck' g f@(App "abs" [t, e]) = --trace (show g) $
   Function t (typeCheck' (f:g) e)
typeCheck' g f = impossible ("Got:\n"
                 ++ show (g) ++ "\n"
                 ++ show f)

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
