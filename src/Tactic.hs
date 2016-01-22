module Tactic where

import Types
import Control.Monad

unify :: (Subst a s, Eq a) => Term a -> Term a -> s -> Maybe s
unify (App f xs) (App g ys) s
   | f == g && length xs == length ys =
      foldM
      (\acc (a, b) -> unify a b acc)
      s
      (zip xs ys)
unify (Var l) (Var r) s
   | l == r = Just s
unify (UVar l) (UVar r) s
   | l == r = Just s
unify (UVar l) r s = inst l r s
unify l (UVar r) s = inst r l s
unify _ _ _ = Nothing
