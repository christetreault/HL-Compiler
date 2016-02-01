module Tactic where

import Types
import Term
import Util
import Control.Monad
import Control.Monad.State

fnUnify :: (Subst a s, Eq a) => Term a -> Term a -> s -> Maybe s
fnUnify (App f xs) (App g ys) s
   | f == g && length xs == length ys =
      foldM
      (\acc (a, b) -> fnUnify a b acc)
      s
      (zip xs ys)
fnUnify (Var l) (Var r) s
   | l == r = Just s
fnUnify (UVar l) (UVar r) s
   | l == r = Just s
fnUnify (UVar l) r s = inst l r s
fnUnify l (UVar r) s = inst r l s
fnUnify _ _ _ = Nothing

unify :: (Eq a, MonadState s m, Subst a s)
         => Term a
         -> Term a
         -> m Bool
unify a b = do
   st <- get
   case (fnUnify a b st) of
      Nothing -> return False
      (Just new) -> do
         put new
         return True

instantiate :: (MonadState t m, Subst a t)
               => Term a
               -> m (Term a)
instantiate a = do
   st <- get
   let instFn u = case (lkup st u) of
          Nothing -> UVar u
          Just t -> t
   return $ instantiateTerm instFn a

bindAll :: State t (Maybe [a]) -> [t] -> State t (Maybe [a])
bindAll m states = return $ mconcat $ map (evalState m) states

bindEach :: [State t (Maybe [a])] -> [t] -> State t (Maybe [a])
bindEach ms states
   | length ms /= length states = impossible "ms Length /= states length!"
   | otherwise = return $ mconcat $  map mapper pairs
      where
         pairs = zip ms states
         mapper = uncurry evalState
