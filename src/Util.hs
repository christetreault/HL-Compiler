module Util where

import Text.PrettyPrint.HughesPJClass

todo :: String -> a
todo m = error $ "TODO: " ++ m

impossible :: String -> a
impossible m = error $ "This should never happen: " ++ m

userError :: String -> a
userError m = error $ "User error: " ++ m

divideBar :: Char -> Doc
divideBar c = (text $ take 20 $ repeat c)
