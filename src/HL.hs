module HL where

import Types
import qualified Data.Map.Strict as Map

data HC b a =
   HCAll (HL b a)
   | HCEach [HL b a]
   | HCIdTacK
   | HCFailK
   deriving (Eq, Ord, Show)

data HL b a =
   HLApply (Rule b)
   | HLCall a
   | HLFail
   | HLIdTac
   | HLOr (HL b a) (HL b a)
   | HLSeq (HL b a) (HC b a)
   | HLAssert (Term b) (HL b a)
   | HLK (HC b a)
   deriving (Eq, Ord, Show)

instance Functor (HL b) where
   fmap f (HLCall x) = HLCall $ f x
   fmap _ HLFail = HLFail
   fmap _ HLIdTac = HLIdTac
   fmap f (HLOr l r) = HLOr (fmap f l) (fmap f r)
   fmap f (HLSeq l r) = HLSeq (fmap f l) (fmap f r)
   fmap f (HLAssert t c) = HLAssert t (fmap f c)
   fmap f (HLK c) = HLK $ fmap f c
   fmap _ (HLApply r) = HLApply r

instance Foldable (HL b) where
   foldMap f (HLCall x) = f x
   foldMap _ HLFail = mempty
   foldMap _ HLIdTac = mempty
   foldMap f (HLOr l r) = (foldMap f l) `mappend` (foldMap f r)
   foldMap f (HLSeq l r) = (foldMap f l) `mappend` (foldMap f r)
   foldMap f (HLAssert _ c) = (foldMap f c)
   foldMap f (HLK c) = foldMap f c
   foldMap _ (HLApply _) = mempty

instance Traversable (HL b) where
   traverse f (HLCall x) = HLCall <$> f x
   traverse _ HLFail = pure HLFail
   traverse _ HLIdTac = pure HLIdTac
   traverse f (HLOr l r) = HLOr <$> (traverse f l) <*> (traverse f r)
   traverse f (HLSeq l r) = HLSeq <$> (traverse f l) <*> (traverse f r)
   traverse f (HLAssert t c) = HLAssert t <$> (traverse f c)
   traverse f (HLK c) = HLK <$> traverse f c
   traverse _ (HLApply r) = pure (HLApply r)

instance Monoid (HL b a) where
   mempty = HLFail
   mappend = HLOr
   mconcat [] = HLFail
   mconcat [t] = t
   mconcat (t:ts) = HLOr t $ mconcat ts

instance Functor (HC b) where
   fmap f (HCAll x) = HCAll $ fmap f x
   fmap f (HCEach x) = HCEach $ map (\a -> fmap f a) x
   fmap _ HCIdTacK = HCIdTacK
   fmap _ HCFailK = HCFailK

instance Traversable (HC b) where
   traverse f (HCAll x) = HCAll <$> traverse f x
   traverse f (HCEach x) = HCEach <$> traverse (\a -> traverse f a) x
   traverse _ HCIdTacK = pure HCIdTacK
   traverse _ HCFailK = pure HCFailK

instance Foldable (HC b) where
   foldMap f (HCAll x) = foldMap f x
   foldMap f (HCEach x) = mconcat $ map (\a -> foldMap f a) x
   foldMap _ HCIdTacK = mempty
   foldMap _ HCFailK = mempty

type Tac a = HL a String
type TacC a = HC a String

data HLProg a =
   HLProg { progEntry :: String,
            progNames :: Map.Map String Integer,
            progFns :: Map.Map Integer (HL a Integer)
          }
   deriving (Show)

makeProg :: Map.Map String (Tac t)
            -> String
            -> Maybe (HLProg t)
makeProg fns entry = do
   functions <- fns'
   return $ HLProg { progEntry = entry,
                     progNames = names,
                     progFns = functions }
   where
      alist = reverse $ Map.assocs fns
      nexts = [1 .. (fromIntegral $ length alist)]
      names = Map.fromList $ zip (map fst alist) nexts
      fns' = do
         let interm = map snd alist
         let lhss = nexts
         rhss <- sequence
                 $ fmap sequence
                 $ fmap (fmap (`Map.lookup` names)) interm
         return $ Map.fromList $ zip lhss rhss
