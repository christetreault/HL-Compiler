module HL where

import Types
import qualified Data.Map.Strict as Map

data HC b a =
   HCAll (HL b a)
   | HCEach [HL b a]
   | HCIdTacK
   | HCFailK
   deriving (Eq, Ord)

data HL b a =
   HLApply (Rule b)
   | HLCall a
   | HLFail
   | HLIdTac
   | HLOr (HL b a) (HL b a)
   | HLSeq (HL b a) (HC b a)
   | HLAssert (Term b) (HL b a)
   | HLK (HC b a)
   deriving (Eq, Ord)

instance Functor (HL b) where
   fmap f (HLCall x) = HLCall $ f x
   fmap _ HLFail = HLFail
   fmap _ HLIdTac = HLIdTac
   fmap f (HLOr l r) = HLOr (fmap f l) (fmap f r)
   fmap f (HLSeq l r) = HLSeq (fmap f l) (fmap f r)
   fmap f (HLAssert t c) = HLAssert t (fmap f c)
   fmap f (HLK c) = HLK $ fmap f c

instance Functor (HC b) where
   fmap f (HCAll x) = HCAll $ fmap f x
   fmap f (HCEach x) = HCEach $ map (\a -> fmap f a) x
   fmap _ HCIdTacK = HCIdTacK
   fmap _ HCFailK = HCFailK

type Tac a = HL a String
type TacC a = HC a String

data HLProg a =
   HLProg { progEntry :: String,
            progNames :: Map.Map String Int,
            progFns :: Map.Map Int (HL Int a)
          }

makeProg :: Map.Map String (Tac t)
            -> String
            -> HLProg t
makeProg fns entry = HLProg { progEntry = entry,
                              progNames = names,
                              progFns = fns' }
   where
      names = undefined
      fns' = undefined
