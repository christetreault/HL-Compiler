module Util where

todo :: String -> a
todo m = error $ "TODO: " ++ m

impossible :: String -> a
impossible m = error $ "This should never happen: " ++ m
