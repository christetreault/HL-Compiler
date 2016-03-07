{-# LANGUAGE FlexibleContexts #-}

module HL.Query where

import Util
import HL.Compile
import HL
import Term
import Control.Monad.State
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Data.Maybe (fromMaybe)

data Query v =
   QueryNo (Term v VarId)
   | QueryYes (Term v VarId) [[(VarId, Term v VarId)]]

instance (Pretty v) => Pretty (Query v) where
   pPrint (QueryNo lhs) =
      text "Unification failed for term:"
      $$ (divideBar '-')
      $$ pPrint lhs
   pPrint (QueryYes lhs rhss) =
      text "Unification succeeded for term:"
      $$ (divideBar '-')
      $$ pPrint lhs
      $$ (divideBar '=')
      $$ (vcat $ fmap solnsMapper numberedList)
      where
         numberedList = zip [1 .. length rhss] rhss
         solnsMapper (n, solns) =
            text ("Solution " ++ (show n) ++ ":")
            $$ nest 2 (vcat $ fmap solnMapper solns)
         solnMapper (v, t) =
            char '?' <> pPrint v
            <+> char '='
            <+> pPrint t

instance (Pretty v) => Show (Query v) where
   show = render . pPrint

query :: (Eq v, Pretty v)
         => Integer
         -> HLProg v VarId
         -> Term v VarId
         -> Query v
query n p t
   | n < 0 = impossible "n must be greater than or equal to 0!"
   | otherwise = case results of
   [] -> QueryNo t
   xs -> QueryYes t xs
   where
      results = do
         let (_, s) = fresh n (empty :: SubstEnv v VarId)
         (_, env) <- runStateT (action p t) s
         let vals = fmap
                    (\k -> (k, fromMaybe
                               (impossible "lkup failure!")
                               $ env `lkup` k))
                    [0 .. n - 1]
         return vals


action :: (Eq v, Pretty v)
          => HLProg v VarId
          -> Term v VarId
          -> StateT (SubstEnv v VarId) [] [Term v VarId]
action p t = compileList p t
