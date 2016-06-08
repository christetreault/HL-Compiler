{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module HL.Query where

import Util
import HL.Compile
import HL
import Term
import Control.Monad.State
import Text.PrettyPrint.HughesPJClass hiding (empty)
import Data.Maybe (fromMaybe)
import Control.Monad.Logic
import Control.Monad.Identity
import HL.InterleavedTactic
import Control.DeepSeq
import GHC.Generics

numSolns :: Query v -> Int
numSolns (QueryNo _) = 0
numSolns (QueryYes _ rhss) = length rhss

data Query v =
   QueryNo (Term v VarId)
   | QueryYes (Term v VarId) [[(VarId, Term v VarId)]]
     deriving Generic

instance NFData v => NFData (Query v)

instance (Pretty v) => Pretty (Query v) where
   pPrint (QueryNo _) =
      text "No results"
   pPrint (QueryYes lhs rhss) =
      text "Results:"
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

unifies :: (Eq v, Pretty v)
           => HLProg v VarId
           -> Term v VarId
           -> Bool
unifies p t = case result of
   QueryYes _ _ -> True
   _ -> False
   where
      result = query Nothing p t

query :: (Eq v, Pretty v) -- TODO: what if user queries a term with no uvars?
         => Maybe Int
         -> HLProg v VarId
         -> Term v VarId
         -> Query v
query d p t = case results of
   [] -> QueryNo t
   xs -> QueryYes t xs
   where
      observer (Nothing) = observeAllStreamS
      observer (Just n') = observeManyStreamS n'

      n :: Integer
      n = case maxUVar maybeMx Just t Nothing of
            Nothing -> 0
            Just n -> n + 1

      maybeMx Nothing x = x
      maybeMx x Nothing = x
      maybeMx (Just a) (Just b) = Just $ max a b

      -- results :: [[(VarId, Term v VarId)]]
      results = do -- list monad
         let (_, s) = fresh n (empty :: SubstEnv v VarId)
         (_,env) <- (observer d $ action p t) s
         return $ fmap
                    (\k -> (k, instantiateTerm (instFn env)
                                  (fromMaybe
                                  (impossible "lkup failure!")
                                  $ env `lkup` k)))
                    [0 .. n - 1]
         where
            instFn :: SubstEnv v VarId -> Integer -> Term v VarId
            instFn env i = fromMaybe (UVar i)
                           $ fmap (instantiateTerm $ instFn env) (env `lkup` i)

{-
-- This should have a standard definition...
delayer :: StateT (SubstEnv v VarId) (StreamT Identity) a
        -> StateT (SubstEnv v VarId) (StreamT Identity) a
delayer x = StateT $ \ s -> delayT $ runStateT x s
-}

action :: (Eq v, Pretty v)
          => HLProg v VarId
          -> Term v VarId
          -> StreamS (SubstEnv v VarId) [Term v VarId]
action p t = compile delayT p t
