module Demo.LambdaCalc where

import Term
import Demo.Std
import HL.Query
import HL.Optimize
import HL

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit
import Criterion.Main
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
mkHasType g e t = App "|- _ :" [g, e, t]

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

ruleNthZero = [] =>> mkNth (mkCons (Var 1) (Var 0)) mkZero (Var 1)

ruleNth = [mkNth (Var 0) (Var 1) (Var 2)]
          =>> mkNth (mkCons (Var 3) (Var 0)) (mkSucc (Var 1)) (Var 2)

ruleTVar = [mkNth (Var 0) (Var 1) (Var 2)]
           =>> mkHasType (Var 0) (App "var" [(Var 1)]) (Var 2)

ruleTApp = [mkHasType (Var 0) (Var 1) (mkArr (Var 2) (Var 3)),
            mkHasType (Var 0) (Var 4) (Var 2)]
           =>> mkHasType (Var 0) (mkApp (Var 1) (Var 4)) (Var 3)

ruleTAbs = [mkHasType (mkCons (Var 1) (Var 0)) (Var 2) (Var 3)]
           =>> mkHasType (Var 0) (mkAbs (Var 1) (Var 2))
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

-- TODO: -- arithmetic operators?
         -- polymorphism (a -> b) forall
              {-

G : * |- T : *
-------------------- Tforall
G |- forall *. t : *

G |- f : forall x . t
G |- f' : x : t'
----------------------
G |- f t' : t [ x |-> t']

-}


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

testLHSTerm :: StringTerm
testLHSTerm = mkFn [mkType "foo", mkType "bar"] 0

testRHSTerm :: StringTerm
testRHSTerm = mkArr (mkType "foo")
                    (mkArr (mkType "bar")
                           (mkType "bar"))
badRHSTerm :: StringTerm
badRHSTerm = mkArr (mkType "cats")
                   (mkArr (mkType "dogs")
                          (mkType "Bool"))

lambdaCalcBenchSuite :: Benchmark
lambdaCalcBenchSuite =
   env (return $ (testLHSTerm, testRHSTerm, typecheck)) $
   \ ~(l, r, p) -> bgroup "Simply Typed Lambda Calculus"
                   [bench "Standard find LHS" $ nf (basicLHS p) r,
                    bench "Standard find 20 LHS" $ nf (stressLHS p) r,
                    bench "Standard find RHS" $ nf (basicRHS p) l,
                    bench "Optimized find LHS"
                       $ nf (basicLHS (normalizeProg p)) r,
                    bench "Optimized find 20 LHS"
                       $ nf (stressLHS (normalizeProg p)) r,
                    bench "Optimized find RHS"
                       $ nf (basicRHS (normalizeProg p)) l]
   where
      basicLHS p rhs = query (Just 1) p (mkHasType
                                         mkNil
                                         (UVar 0)
                                         rhs)
      stressLHS p rhs = query (Just 20) p (mkHasType
                                           mkNil
                                           (UVar 0)
                                           rhs)
      basicRHS p lhs = query (Just 1) p (mkHasType
                                         mkNil
                                         lhs
                                         (UVar 0))

lambdaCalcTestSuite :: TestTree
lambdaCalcTestSuite = testGroup "Simply Typed Lambda Calculus"
                      [testCase "lhs :: rhs typechecks" lhsUnifiesRhs,
                       testCase "?(0) :: rhs typechecks" varUnifiesRhs,
                       testCase "lhs :: ?(0) typechecks" lhsUnifiesVar,
                       testCase "lhs :: badRHS does not typecheck"
                          catsAndDogsLivingTogether,
                       testProperty "∀n numSolns (query n |- [] : ?(0) t) = n"
                          infiniteSolns]
   where
      lhsUnifiesRhs = typecheck `unifies` (mkHasType
                                              mkNil
                                              testLHSTerm
                                              testRHSTerm) @?= True
      varUnifiesRhs = typecheck `unifies` (mkHasType
                                              mkNil
                                              (UVar 0)
                                              testRHSTerm) @?= True
      lhsUnifiesVar = typecheck `unifies` (mkHasType
                                              mkNil
                                              testLHSTerm
                                              (UVar 0)) @?= True
      catsAndDogsLivingTogether = typecheck `unifies`
                                            (mkHasType
                                                mkNil
                                                testLHSTerm
                                                badRHSTerm) @?= False
      infiniteSolns :: Integer -> Property IO
      infiniteSolns a = (>= (0 :: Integer))
                        ==> \n -> numSolns (query
                                               (Just n)
                                               typecheck
                                               (mkHasType
                                                   mkNil
                                                   (UVar 0)
                                                   testRHSTerm)) == n
