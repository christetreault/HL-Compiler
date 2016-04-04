{-# LANGUAGE FlexibleContexts #-}

module HL.Query where

import Util
import HL.Compile
import HL
import Term
import Control.Monad.State
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Data.Maybe (fromMaybe)
import Control.Monad.Logic

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
         => Maybe Int
         -> Integer
         -> HLProg v VarId
         -> Term v VarId
         -> Query v
query d n p t = case (observer d) results of
   [] -> QueryNo t
   xs -> QueryYes t xs
   where
      observer (Nothing) = observeAll
      observer (Just n') = observeMany n'
      results = do
         let (_, s) = fresh n (empty :: SubstEnv v VarId)
         (_, env) <- runStateT (action p t) s
         let vals = fmap
                    (\k -> (k, instantiateTerm (instFn env)
                                  (fromMaybe
                                  (impossible "lkup failure!")
                                  $ env `lkup` k)))
                    [0 .. n - 1]
         return vals
         where
            instFn :: SubstEnv v VarId -> Integer -> Term v VarId
            instFn env i = fromMaybe (UVar i)
                           $ fmap (instantiateTerm $ instFn env) (env `lkup` i)

action :: (Eq v, Pretty v)
          => HLProg v VarId
          -> Term v VarId
          -> StateT (SubstEnv v VarId) Logic [Term v VarId]
action p t = compile p t
