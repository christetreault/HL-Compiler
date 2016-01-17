module Tactic where

import Types


unify :: Term a -> Term a -> TermMap a -> TermMap a
unify (App f xs) (App g ys) m
   | f == g && length xs == length ys = undefined
