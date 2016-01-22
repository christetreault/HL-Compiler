module Tactic where

import Types
import Term
import Control.Monad

fnUnify :: (Subst a s, Eq a) => Term a -> Term a -> s -> Maybe s
fnUnify (App f xs) (App g ys) s
   | f == g && length xs == length ys =
      foldM
      (\acc (a, b) -> unify a b acc)
      s
      (zip xs ys)
fnUnify (Var l) (Var r) s
   | l == r = Just s
fnUnify (UVar l) (UVar r) s
   | l == r = Just s
fnUnify (UVar l) r s = inst l r s
fnUnify l (UVar r) s = inst r l s
fnUnify _ _ _ = Nothing

unify :: Term UVar -> Term UVar -> WithSubst Bool
unify a b = do
   state <- get
   case (fnUnify a b state) of
      | Nothing -> return False
      | (Just new) -> do
         put new
         return True

instantiate :: Term UVar -> WithSubst (UVar Term)
instantiate a = do
   state <- get
   let instFn u = case (lookup state u) of
          | Nothing -> UVar u
          | Just t -> t
   return $ instantiateTerm instFn a
