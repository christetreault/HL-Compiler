{-# LANGUAGE FlexibleContexts #-}

module Tactic where

import Term
import Util
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic

fnUnify :: (Subst a (Term v a) s, Eq a, Eq v, Show a,
            MonadPlus m, MonadLogic m)
           => Term v a -> Term v a -> s -> m s
fnUnify (App f xs) (App g ys) s
   | f == g =
      foldM
      (\acc (a, b) -> fnUnify a b acc)
      s
      (zip xs ys)
fnUnify (Var l) (Var r) s
   | l == r = return s
fnUnify (UVar l) (UVar r) s
   | l == r = return s
fnUnify (UVar l) r s =
   case lkup s l of
      Just e -> fnUnify e r s
      Nothing -> inst l r s
fnUnify l (UVar r) s =
   case lkup s r of
      Just e -> fnUnify l e s
      Nothing -> inst r l s
fnUnify _ _ _ = mzero

instantiate :: (MonadState t m, Subst a (Term v a) t)
               => Term v a
               -> m (Term v a)
instantiate a = do
   st <- get
   let instFn u = case (lkup st u) of
          Nothing -> UVar u
          Just t -> t
   return $ instantiateTerm instFn a
