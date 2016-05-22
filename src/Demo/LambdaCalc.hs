module Demo.LambdaCalc where

import Util
import Term
import Demo.Std
import HL.Query
import HL

import Data.Maybe (fromJust)
import Test.Tasty
import Criterion.Main
import Debug.Trace
import Text.PrettyPrint.HughesPJClass
import qualified Data.Map as Map


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


mkAbs t e = App "abs" [t, e]
mkType t = App t []
mkApp e1 e2 = App "app" [e1, e2]
mkVar n = App "var" [mkN n]
mkArr a b = App "arr" [a, b]
--mkHasType e t = App ":" [e, t] ----- \
mkHasType g e t = App "|- _ :" [g, e, t] -- + -- collapse into one ternary constructor

mkFn :: [StringTerm] -> Integer -> StringTerm
mkFn [] n = mkVar n
mkFn (x:xs) n = mkAbs x (mkFn xs n)


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

ruleNthZero = [] ==> mkNth (mkCons (Var 1) (Var 0)) mkZero (Var 1)

ruleNth = [mkNth (Var 0) (Var 1) (Var 2)]
          ==> mkNth (mkCons (Var 3) (Var 0)) (mkSucc (Var 1)) (Var 2)

ruleTVar = [mkNth (Var 0) (Var 1) (Var 2)]
           ==> mkHasType (Var 0) (App "var" [(Var 1)]) (Var 2)

ruleTApp = [mkHasType (Var 0) (Var 1) (mkArr (Var 2) (Var 3)),
            mkHasType (Var 0) (Var 5) (Var 2)]
           ==> mkHasType (Var 0) (mkApp (Var 1) (Var 5)) (Var 2)

ruleTAbs = [mkHasType (mkCons (Var 1) (Var 0)) (Var 2) (Var 3)]
           ==> mkHasType (Var 0) (mkAbs (Var 1) (Var 2))
                                 (mkArr (Var 1) (Var 3))

typecheck = fromJust $ makeProg fnMap ep
   where
      ep = "typechecks"
      epDef = hlAny [ HLSeq x
                      $ HCAll
                      $ HLCall ep | x <- cases ]
      nth = "nth"
      nthDef = HLSeq (hlAny [HLApply ruleNth, HLApply ruleNthZero])
                     (HCAll $ HLCall nth)
      tVar = "TVar"
      tVarDef = HLSeq (HLApply ruleTVar)
                      (HCAll $ HLCall nth)
      tApp = "TApp"
      tAppDef = (HLApply ruleTApp)
      tAbs = "TAbs"
      tAbsDef = (HLApply ruleTAbs)
      cases = [ HLCall tVar,
                HLCall tApp,
                HLCall tAbs]
      fnMap = Map.fromList
              [(ep, epDef),
               (tVar, tVarDef),
               (tApp, tAppDef),
               (tAbs, tAbsDef),
               (nth, nthDef)]


----------------------------------------------------------------------
-- Haskell implementation
----------------------------------------------------------------------

data TCResult =
   Type StringTerm
   | Error
     deriving (Ord, Eq, Show)

hsTypeCheck t = hsTypeCheck' [] t

hsTypeCheck' g (App "var" [n']) =
   let n = termToInt n' in
   if (length g > n)
   then Type (g !! n)
   else Error {- Undefined variable -}
hsTypeCheck' g (App "app" [e1, e2]) =
   let lhs = hsTypeCheck' g e1 in
   let rhs = hsTypeCheck' g e2 in
   case (lhs, rhs) of
      (Type (App "arr" [d,r]), Type d') ->
         if d == d' then Type r
         else Error {- applied a function of type [a -> b] to a [c] -}
      (Type _ , Type _) -> Error {- e1 is not a function -}
      (_ , _) -> Error {- one of the subterms does not type check -}
hsTypeCheck' g f@(App "abs" [t, e]) =
   case hsTypeCheck' (t:g) e of
      Type t' -> Type (mkArr t t')
      Error -> Error {- body does not type check -}
hsTypeCheck' _ _ = Error -- otherwise (I assume)



----------------------------------------------------------------------
-- Tests - TODO
----------------------------------------------------------------------

lambdaCalcBenchSuite :: Benchmark
lambdaCalcBenchSuite = todo "Write benchmark suite"

lambdaCalcTestSuite :: TestTree
lambdaCalcTestSuite = todo "Write test suite"
