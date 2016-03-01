module HL.Optimize where

import HL
import Term

smartK :: HC v a b -> HL v a b
smartK HCIdTacK = HLIdTac
smartK (HCAll x) = x
smartK (HCEach [x]) = x
smartK _ = HLFail

smartSeq :: HL v a b -> HC v a b -> HL v a b
smartSeq HLIdTac r = smartK r
smartSeq HLFail _ = HLFail
smartSeq x HCIdTacK = x
smartSeq x y = HLSeq x y

smartAll :: HL v a b -> HC v a b
smartAll HLIdTac = HCIdTacK
smartAll HLFail = HCFailK
smartAll (HLK k) = k
smartAll x = HCAll x

normalizeHl :: HC v a b -> HL v a b -> HL v a b
normalizeHl k (HLOr l r) = HLOr (normalizeHl k l) (normalizeHl k r)
normalizeHl k (HLPlus l r) = HLPlus (normalizeHl k l) (normalizeHl k r)
normalizeHl _ HLFail = HLFail
normalizeHl k HLIdTac = smartK k
normalizeHl k (HLApply r)
   | length (rulePrems r) == 0 = HLApply r
   | otherwise =
      HLSeq (HLApply r)
      $ HCEach $ fmap (\a -> HLAssert a $ smartK k) $ rulePrems r
normalizeHl k (HLSeq c cc) = normalizeHl (normalizeHc k cc) c
normalizeHl k (HLCall n) = smartSeq (HLCall n) k
normalizeHl k (HLK r) = smartK (normalizeHc k r)
normalizeHl k (HLAssert t c) = HLAssert t $ normalizeHl k c

normalizeHc :: HC v a b -> HC v a b -> HC v a b
normalizeHc k (HCAll t) = smartAll $ normalizeHl k t
normalizeHc k (HCEach ts) = HCEach $ fmap (normalizeHl k) ts
normalizeHc k HCIdTacK = k
normalizeHc _ HCFailK = HCFailK

normalizeProg :: HLProg v a -> HLProg v a
normalizeProg p = p { progFns = fmap (normalizeHl HCIdTacK) (progFns p) }
