module HL where

import Term
import Util
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJClass

-- | A high-level program continuation
data HC v b a =
   HCAll (HL v b a) -- ^ Take a given tactic, apply to every subgoal
   | HCEach [HL v b a] -- ^ Zip a list of tactics with a list of subgoals
   | HCIdTacK -- ^ Identity; succeed and do nothing
   | HCFailK -- ^ Fail and do nothing
   deriving (Eq, Ord)

instance (Pretty v, Pretty b, Pretty a) => Show (HC v b a) where
   show = render . pPrint

-- | A high-level program
data HL v b a =
   HLApply (Rule v b) -- ^ Apply a rule
   | HLCall a -- ^ Call a function
   | HLFail -- ^ Fail a tactic
   | HLIdTac -- ^ Identity; succeed and do nothing
   | HLOr (HL v b a) (HL v b a) -- ^ Try LHS, if it fails try RHS
   | HLPlus (HL v b a) (HL v b a) -- ^ Like Or, but if LHS succeeds, then a
     -- subsequent tactic fails, backtracks and tries RHS
   | HLOnce (HL v b a) -- ^ Tries the tactic with no backtracking
   | HLSeq (HL v b a) (HC v b a) -- ^ Try LHS, if it succeeds, produce list of
                                 -- subgoals to be processed by the RHS HC
   | HLAssert (Term v b) (HL v b a) -- ^ Assertion (currently unused)
   | HLK (HC v b a) -- ^ Lift a continuation to a tactic
   deriving (Eq, Ord)

instance (Pretty v, Pretty b, Pretty a) => Show (HL v b a) where
   show = render . pPrint

instance (Pretty v, Pretty a, Pretty b) => Pretty (HL v b a) where
   pPrint (HLApply r) = text "apply"
                        <+> parens (nest 2 (pPrint r))
   pPrint (HLCall f) = text "call"
                       <+> quotes (pPrint f)
   pPrint (HLFail) = brackets (text "fail")
   pPrint (HLIdTac) = brackets (text "idtac")
   pPrint (HLOr lhs rhs) = (pPrint lhs)
                           <+> (text "∨")
                           $$ (pPrint rhs)
   pPrint (HLPlus lhs rhs) = (pPrint lhs)
                           <+> (text "+")
                           $$ (pPrint rhs)
   pPrint (HLOnce hl) = text "once" <> parens (pPrint hl)
   pPrint (HLSeq lhs rhss) = pPrint lhs
                             <+> semi
                             $$ pPrint rhss
   pPrint (HLAssert lhst rhs) = text "assert"
                                <+> pPrint lhst
                                $$ text "as"
                                <+> pPrint rhs
   pPrint (HLK hc) = text "toTactic"
                     <> parens (pPrint hc)

instance (Pretty v, Pretty a, Pretty b) => Pretty (HC v b a) where
   pPrint (HCAll hl) = text "all" <+> pPrint hl
   pPrint (HCEach hls) = text "each"
                         $$ nest 2 (vcat (fmap pPrint hls))
   pPrint (HCIdTacK) = brackets (text "continuation idtac")
   pPrint (HCFailK) = brackets (text "continuation fail")

instance Functor (HL v b) where
   fmap f (HLCall x) = HLCall $ f x
   fmap _ HLFail = HLFail
   fmap _ HLIdTac = HLIdTac
   fmap f (HLOr l r) = HLOr (fmap f l) (fmap f r)
   fmap f (HLPlus l r) = HLPlus (fmap f l) (fmap f r)
   fmap f (HLSeq l r) = HLSeq (fmap f l) (fmap f r)
   fmap f (HLAssert t c) = HLAssert t (fmap f c)
   fmap f (HLOnce h) = HLOnce $ fmap f h
   fmap f (HLK c) = HLK $ fmap f c
   fmap _ (HLApply r) = HLApply r

instance Foldable (HL v b) where
   foldMap f (HLCall x) = f x
   foldMap _ HLFail = mempty
   foldMap _ HLIdTac = mempty
   foldMap f (HLOr l r) = (foldMap f l) `mappend` (foldMap f r)
   foldMap f (HLPlus l r) = (foldMap f l) `mappend` (foldMap f r)
   foldMap f (HLSeq l r) = (foldMap f l) `mappend` (foldMap f r)
   foldMap f (HLAssert _ c) = (foldMap f c)
   foldMap f (HLOnce h) = foldMap f h
   foldMap f (HLK c) = foldMap f c
   foldMap _ (HLApply _) = mempty

instance Traversable (HL v b) where
   traverse f (HLCall x) = HLCall <$> f x
   traverse _ HLFail = pure HLFail
   traverse _ HLIdTac = pure HLIdTac
   traverse f (HLOr l r) = HLOr <$> (traverse f l) <*> (traverse f r)
   traverse f (HLPlus l r) = HLPlus <$> (traverse f l) <*> (traverse f r)
   traverse f (HLSeq l r) = HLSeq <$> (traverse f l) <*> (traverse f r)
   traverse f (HLAssert t c) = HLAssert t <$> (traverse f c)
   traverse f (HLOnce h) = HLOnce <$> traverse f h
   traverse f (HLK c) = HLK <$> traverse f c
   traverse _ (HLApply r) = pure (HLApply r)

instance Monoid (HL v b a) where
   mempty = HLFail
   mappend = HLPlus
   mconcat [] = HLFail
   mconcat [t] = t
   mconcat (t:ts) = HLPlus t $ mconcat ts

instance Functor (HC v b) where
   fmap f (HCAll x) = HCAll $ fmap f x
   fmap f (HCEach x) = HCEach $ map (\a -> fmap f a) x
   fmap _ HCIdTacK = HCIdTacK
   fmap _ HCFailK = HCFailK

instance Traversable (HC v b) where
   traverse f (HCAll x) = HCAll <$> traverse f x
   traverse f (HCEach x) = HCEach <$> traverse (\a -> traverse f a) x
   traverse _ HCIdTacK = pure HCIdTacK
   traverse _ HCFailK = pure HCFailK

instance Foldable (HC v b) where
   foldMap f (HCAll x) = foldMap f x
   foldMap f (HCEach x) = mconcat $ map (\a -> foldMap f a) x
   foldMap _ HCIdTacK = mempty
   foldMap _ HCFailK = mempty

type Tac v a = HL v a String
type TacC v a = HC v a String

data HLProg v a =
   HLProg { progEntry :: String,
            progNames :: Map.Map String Integer,
            progFns :: Map.Map Integer (HL v a Integer)
          }

instance (Pretty v, Pretty a) => Show (HLProg v a) where
   show = render . pPrint


instance (Pretty v, Pretty a) => Pretty (HLProg v a) where
   pPrint hl = (text "Entry point:" <+> text (progEntry hl))
               $$ vcat functions
      where
         functions = fmap (\(name, i) -> text name
                                         <+> lbrace
                                         $$ nest 2 (pPrint (getImpl i))
                                         $$ rbrace)
                          (Map.assocs $ progNames hl)
         getImpl i = Map.findWithDefault
                     (impossible "function declared but not defined!")
                     i
                     (progFns hl)



makeProg :: Map.Map String (Tac v t)
            -> String
            -> Maybe (HLProg v t)
makeProg fns entry = do
   functions <- fns'
   return $ HLProg { progEntry = entry,
                     progNames = names,
                     progFns = functions }
   where
      alist = reverse $ Map.assocs fns
      nexts = [0 .. (fromIntegral $ length alist)]
      names = Map.fromList $ zip (map fst alist) nexts
      fns' = do
         let interm = map snd alist
         let lhss = nexts
         rhss <- sequence
                 $ fmap sequence
                 $ fmap (fmap (`Map.lookup` names)) interm
         return $ Map.fromList $ zip lhss rhss

hlFirst :: [HL v a b] -> HL v a b
hlFirst [] = HLFail
hlFirst (x:xs) = HLOr x $ hlFirst xs

hlAny :: [HL v a b] -> HL v a b
hlAny = mconcat
