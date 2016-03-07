{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}

module Term where

import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJClass
import Util
import Control.DeepSeq
import GHC.Generics


type VarId = Integer

data Term b a =
   App b [Term b a]
   | Var VarId
   | UVar a
   deriving (Eq, Ord, Generic)

instance (NFData a, NFData b) => NFData (Term a b)

instance (Pretty b, Pretty a) => Show (Term b a) where
   show = render . pPrint

instance (Pretty a, Pretty b) => Pretty (Term b a) where
   pPrint (UVar a) = parens (char '?' <> pPrint a)
   pPrint (Var i) = parens $ pPrint i
   pPrint (App b [UVar a]) = text "App"
                             <+> (quotes (pPrint b))
                             <+> lbrace
                             <+> (brackets $ pPrint a)
                             <+> rbrace
   pPrint (App b [Var i]) = text "App"
                             <+> (quotes (pPrint b))
                             <+> lbrace
                             <+> (parens $ pPrint i)
                             <+> rbrace
   pPrint (App b []) = text "App"
                       <+> (quotes (pPrint b))
                       <+> lbrace
                       <> rbrace
   pPrint (App b ts) = text "App"
                       <+> (quotes (pPrint b))
                       <+> lbrace
                       $$ (nest 1 (vcat $ fmap pPrint ts)
                           $+$ rbrace)

data Rule b a =
   Rule { ruleVars :: VarId,
          rulePrems :: [Term b a],
          ruleConcl :: Term b a}
   deriving (Eq, Ord)

instance (Pretty b, Pretty a) => Show (Rule b a) where
   show = render . pPrint

instance (Pretty a, Pretty b) => Pretty (Rule b a) where
   pPrint (Rule vs pm cl) = renderVars vs
                            $$ (vcat $ fmap renderPrem pm)
                            $$ (divideBar '=')
                            $$ renderConc cl
      where
         renderVars v = text "Variables:" $$ nest 2 (pPrint v)
         renderPrem t = text "Premise:" $$ nest 2 (pPrint t)
         renderConc t = text "Conclusion:" $$ nest 2 (pPrint t)

data SubstEnv b a =
   SubstEnv (Map.Map VarId (Term b a)) Integer
   deriving (Show)

class Subst key val subst | subst -> key , subst -> val where
   fresh :: Integer -> subst -> ([key], subst)
   empty :: subst
   lkup :: subst -> key -> Maybe val
   inst :: key -> val -> subst -> Maybe subst

instance Subst VarId (Term val VarId) (SubstEnv val VarId) where
   fresh n (SubstEnv m i) = ([i .. (i + n)], SubstEnv m (i + n))
   empty = SubstEnv Map.empty 0
   lkup (SubstEnv m _) k = Map.lookup k m
   inst k t (SubstEnv m i) = if k `Map.member` m
                             then Nothing
                             else Just $ SubstEnv (Map.insert k t m) i

instantiateTerm :: (a -> Term v a) -> Term v a -> Term v a
instantiateTerm lf (App f xs) = App f $ fmap (instantiateTerm lf) xs
instantiateTerm lf (UVar u) = lf u
instantiateTerm _ v = v

varsToUVars :: (Integer -> Term v t) -> Term v u -> Term v t
varsToUVars on (App f xs) = App f $ fmap (varsToUVars on) xs
varsToUVars on (Var v) = on v
varsToUVars _ _ = impossible "varsToUVars called on UVar"

divideBar c = (text $ take 20 $ repeat c)
