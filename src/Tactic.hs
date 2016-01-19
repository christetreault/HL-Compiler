module Tactic where

import Types

unify :: (Subst k s) => Term a -> Term a -> s -> s
unify (App f xs) (App g ys) m
   | f == g && length xs == length ys = undefined
