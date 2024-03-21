module CoreData.Fr where

import PreCore
import CoreClasses

import CoreData.Search
import CoreData.Tr

data Br h o a = Br {sryBr ∷ o,br ∷ BrI h o a}

data BrI h o a where
  Br1 ∷ ∀ h o a. Tr h o a → BrI h o a
  Br2 ∷ ∀ h o a. Tr h o a → Tr h o a → BrI h o a

data Fr hᴵ hᴼ o a = Fr {sryFr ∷ o,fr ∷ FrI hᴵ hᴼ o a}

data FrI hᴵ hᴼ o a where
  Fr0 ∷ ∀ h o a. FrI h h o a
  Fr1 ∷ ∀ h o a. Tr h o a → FrI h h o a
  Fr2 ∷ ∀ hᴵ hᴼ o a. Br hᴵ o a → Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → FrI hᴵ hᴼ o a

data ExTallFr h o a where
  EqTlFr ∷ ∀ h o a. Fr 'Z h o a → ExTallFr h o a
  SuccFr ∷ ∀ h o a. Fr 'Z ('S h) o a → ExTallFr h o a

data ExShortFr h o a where
  EqShFr ∷ ∀ h o a. Fr 'Z h o a → ExShortFr h o a
  PredFr ∷ ∀ h o a. Fr 'Z h o a → ExShortFr ('S h) o a

data ExFr o a where
  ExFr ∷ ∀ h o a. Fr 'Z h o a → ExFr o a

exFrTall ∷ ExTallFr h o a → ExFr o a
exFrTall f = case f of
  EqTlFr f' → ExFr f'
  SuccFr f' → ExFr f'

exFrShort ∷ ExShortFr h o a → ExFr o a
exFrShort f = case f of
  EqShFr f' → ExFr f'
  PredFr f' → ExFr f'

data FrK h o a where
  TopFr ∷ ∀ o a. FrK 'Z o a
  InFr2 ∷ ∀ h o a. Br h o a → () → Br h o a → FrK h o a → FrK ('S h) o a

data BrInFr hᴵ hᴼ o a where
  InFr2L ∷ ∀ hᴵ hᴼ o a. () → Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → FrK hᴵ o a → BrInFr hᴵ hᴼ o a
  InFr2R ∷ ∀ hᴵ hᴼ o a . Br hᴵ o a → Fr ('S hᴵ) hᴼ o a → () → FrK hᴵ o a → BrInFr hᴵ hᴼ o a

data TrInFr hᴵ hᴼ o a  where
  InFr1 ∷ ∀ h o a. () → FrK h o a → TrInFr h h o a
  InBr1 ∷ ∀ hᴵ hᴼ o a. () → BrInFr hᴵ hᴼ o a → TrInFr hᴵ hᴼ o a
  InBr2L ∷ ∀ hᴵ hᴼ o a. () → Tr hᴵ o a → BrInFr hᴵ hᴼ o a → TrInFr hᴵ hᴼ o a
  InBr2R ∷ ∀ hᴵ hᴼ o a. Tr hᴵ o a → () → BrInFr hᴵ hᴼ o a → TrInFr hᴵ hᴼ o a

data PositionFr h o a where
  PositionFr0 ∷ ∀ h o a. FrK h o a → PositionFr h o a
  PositionFr2 ∷ ∀ hᴵ hᴼ o a. Direction → Fr hᴵ hᴼ o a → FrK hᴵ o a → PositionFr hᴼ o a
  PositionFrBr ∷ ∀ hᴵ hᴼ o a. Direction → Br hᴵ o a → BrInFr hᴵ hᴼ o a → PositionFr hᴼ o a
  PositionFrTr ∷ ∀ hᴵ hᴼ o a. PositionTr hᴵ o a → TrInFr hᴵ hᴼ o a → PositionFr hᴼ o a

data SearchFr h o a where
  NotFoundFr ∷ ∀ h o a. PositionFr h o a → SearchFr h o a
  FoundFr ∷ ∀ hᴵ hᴼ o a. a → TrK 'Z hᴵ o a → TrInFr hᴵ hᴼ o a → SearchFr hᴼ o a

searchFrFromTr ∷ SearchTr hᴵ o a → TrInFr hᴵ hᴼ o a → SearchFr hᴼ o a
searchFrFromTr (NotFoundTr p) cc = NotFoundFr $ PositionFrTr p cc
searchFrFromTr (FoundTr x c) cc = FoundFr x c cc

br1 ∷ Tr h o a → Br h o a
br1 t = Br (sryTr t) $ Br1 t

br2 ∷ (Append o) ⇒ Tr h o a → Tr h o a → Br h o a
br2 tˡ tʳ = Br (sryTr tˡ ⧺ sryTr tʳ) $ Br2 tˡ tʳ

fr0 ∷ (Null o) ⇒ Fr h h o a
fr0 = Fr null Fr0

fr1 ∷ Tr h o a → Fr h h o a
fr1 t = Fr (sryTr t) $ Fr1 t

fr2 ∷ (Append o) ⇒ Br hᴵ o a → Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → Fr hᴵ hᴼ o a
fr2 bˡ f bʳ = Fr (sryBr bˡ ⧺ sryFr f ⧺ sryBr bʳ) $ Fr2 bˡ f bʳ

zipFr ∷ (Append o) ⇒ Fr hᴵ hᴼ o a → FrK hᴵ o a → Fr 'Z hᴼ o a
zipFr f TopFr = f
zipFr f (InFr2 bˡ () bʳ c) = zipFr (fr2 bˡ f bʳ) c

zipFrBr ∷ (Append o) ⇒ Br hᴵ o a → BrInFr hᴵ hᴼ o a → Fr 'Z hᴼ o a
zipFrBr bˡ (InFr2L () f bʳ c) = zipFr (fr2 bˡ f bʳ) c
zipFrBr bʳ (InFr2R bˡ f () c) = zipFr (fr2 bˡ f bʳ) c

zipFrTr ∷ (Append o) ⇒ Tr hᴵ o a → TrInFr hᴵ hᴼ o a → Fr 'Z hᴼ o a
zipFrTr t (InFr1 () c) = zipFr (fr1 t) c
zipFrTr t (InBr1 () c) = zipFrBr (br1  t) c
zipFrTr tˡ (InBr2L () tʳ c) = zipFrBr (br2 tˡ tʳ) c
zipFrTr tʳ (InBr2R tˡ () c) = zipFrBr (br2 tˡ tʳ) c

searchFr ∷ (o → Search) → Fr hᴵ hᴼ o a → FrK hᴵ o a → SearchFr hᴼ o a
searchFr s f c = case s $ sryFr f of
  L → NotFoundFr $ PositionFr2 Left f c
  R → NotFoundFr $ PositionFr2 Right f c
  C → searchFrI s (fr f) c
  N → NotFoundFr $ PositionFr2 Left f c

searchFrI ∷ (o → Search) → FrI hᴵ hᴼ o a → FrK hᴵ o a → SearchFr hᴼ o a
searchFrI _ Fr0 c = NotFoundFr $ PositionFr0 c
searchFrI s (Fr1 t) c = searchFrFromTr (searchTrI s (tr t) TopTr) $ InFr1 () c
searchFrI s (Fr2 bˡ fᵐ bʳ) c = case (s $ sryBr bˡ,s $ sryFr fᵐ,s $ sryBr bʳ) of
  (L,_,_) → NotFoundFr $ PositionFrBr Left bˡ $ InFr2L () fᵐ bʳ c
  (C,_,_) → searchBrI s (br bˡ) $ InFr2L () fᵐ bʳ c
  (R,L,_) → NotFoundFr $ PositionFr2 Left fᵐ $ InFr2 bˡ () bʳ c
  (R,N,L) → NotFoundFr $ PositionFr2 Left fᵐ $ InFr2 bˡ () bʳ c
  (_,C,_) → searchFrI s (fr fᵐ) $ InFr2 bˡ () bʳ c
  (_,R,L) → NotFoundFr $ PositionFrBr  Left bʳ $ InFr2R bˡ fᵐ () c
  (_,_,C) → searchBrI s (br bʳ) $ InFr2R bˡ fᵐ () c
  (_,_,R) → NotFoundFr $ PositionFrBr Right bʳ $ InFr2R bˡ fᵐ () c
  (N,_,_) → error "search direction should never be N"
  (_,_,N) → error "search direction should never be N"

searchBrI ∷ (o → Search) → BrI hᴵ o a → BrInFr hᴵ hᴼ o a → SearchFr hᴼ o a
searchBrI s (Br1 t) c = searchFrFromTr (searchTrI s (tr t) TopTr) $ InBr1 () c
searchBrI s (Br2 tˡ tʳ) c = case (s $ sryTr tˡ,s $ sryTr tʳ) of
  (L,_) → NotFoundFr $ PositionFrTr (PositionTr Left tˡ TopTr) $ InBr2L () tʳ c
  (C,_) → searchFrFromTr (searchTrI s (tr tˡ) TopTr) $ InBr2L () tʳ c
  (R,L) → NotFoundFr $ PositionFrTr (PositionTr Left tʳ TopTr) $ InBr2R tˡ () c
  (_,C) → searchFrFromTr (searchTrI s (tr tʳ) TopTr) $ InBr2R tˡ () c
  (_,R) → NotFoundFr $ PositionFrTr (PositionTr Right tʳ TopTr) $ InBr2R tˡ () c
  (N,_) → error "search direction should never be N"
  (_,N) → error "search direction should never be N"

frT ∷ (Append o) ⇒ Tr h o a → FrK h o a → Fr 'Z h o a
frT t c = zipFr (fr1 t) c

frTT ∷ (Monoid o) ⇒ Tr h o a → Tr h o a → FrK h o a → Fr 'Z ('S h) o a
frTT tˡ tʳ c = zipFr (fr2 (br1 tˡ) fr0 (br1 tʳ)) c

frTTT ∷ (Monoid o) ⇒ Tr h o a → Tr h o a → Tr h o a → FrK h o a → Fr 'Z ('S h) o a
frTTT tˡ tᵐ tʳ c = zipFr (fr2 (br2 tˡ tᵐ) fr0 (br1 tʳ)) c

frTTTT ∷ (Append o) ⇒ Tr h o a → Tr h o a → Tr h o a → Tr h o a → FrK h o a → Fr 'Z ('S h) o a
frTTTT tᴸ tˡ tʳ tᴿ c = zipFr (fr2 (br1 tᴸ) (fr1 (tr2 tˡ tʳ)) (br1 tᴿ)) c

frTTFB ∷ (Append o) ⇒ Tr hᴵ o a → Tr hᴵ o a → Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → FrK hᴵ o a → Fr 'Z hᴼ o a
frTTFB tˡ tᵐ f bʳ c = zipFr (fr2 (br2 tˡ tᵐ) f bʳ) c

frBFTT ∷ (Append o) ⇒ Br hᴵ o a → Fr ('S hᴵ) hᴼ o a → Tr hᴵ o a → Tr hᴵ o a → FrK hᴵ o a → Fr 'Z hᴼ o a
frBFTT bˡ f tᵐ tʳ c = zipFr (fr2 bˡ f (br2 tᵐ tʳ)) c

balExtraFrL ∷ (Monoid o) ⇒ Tr hᴵ o a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrL t (Fr _ Fr0) c = EqTlFr $ frT t c
balExtraFrL tˡ (Fr _ (Fr1 tʳ)) c = SuccFr $ frTT tˡ tʳ c
balExtraFrL tˡ (Fr _ (Fr2 (Br _ (Br1 tᵐ)) f bʳ)) c = EqTlFr $ frTTFB tˡ tᵐ f bʳ c
balExtraFrL tᴸ (Fr _ (Fr2 (Br _ (Br2 tˡ tʳ)) f bᴿ)) c = balExtraFrL (tr2 tˡ tʳ) f $ InFr2 (br1 tᴸ) () bᴿ c

balExtraFrLL ∷ (Monoid o) ⇒ Tr hᴵ o a → Tr hᴵ o a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrLL tˡ tʳ (Fr _ Fr0) c = SuccFr $ frTT tˡ tʳ c
balExtraFrLL tˡ tᵐ (Fr _ (Fr1 tʳ)) c = SuccFr $ frTTT tˡ tᵐ tʳ c
balExtraFrLL tᴸ tˡ (Fr _ (Fr2 (Br _ (Br1 tʳ)) f bᴿ)) c = balExtraFrL (tr2 tˡ tʳ) f $ InFr2 (br1 tᴸ) () bᴿ c
balExtraFrLL tᴸ tˡ (Fr _ (Fr2 (Br _ (Br2 tᵐ tʳ)) f bᴿ)) c = balExtraFrL (tr3 tˡ tᵐ tʳ) f $ InFr2 (br1 tᴸ) () bᴿ c

balExtraFrLLL ∷ (Monoid o) ⇒ Tr hᴵ o a → Tr hᴵ o a → Tr hᴵ o a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrLLL tˡ tᵐ tʳ (Fr _ Fr0) c = SuccFr $ frTTT tˡ tᵐ tʳ c
balExtraFrLLL tᴸ tˡ tʳ (Fr _ (Fr1 tᴿ)) c = SuccFr $ frTTTT tᴸ tˡ tʳ tᴿ c
balExtraFrLLL tᴸ tˡ tᵐ (Fr _ (Fr2 (Br _ (Br1 tʳ)) f bᴿ)) c = balExtraFrL (tr3 tˡ tᵐ tʳ) f $ InFr2 (br1 tᴸ) () bᴿ c
balExtraFrLLL tᴸ tˡ tᵐ (Fr _ (Fr2 (Br _ (Br2 tⁿ tʳ)) f bᴿ)) c = balExtraFrL (tr3 tᵐ tⁿ tʳ) f $ InFr2 (br2 tᴸ tˡ) () bᴿ c

balExtraFrBL ∷ (Monoid o) ⇒ Br hᴵ o a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrBL (Br _ (Br1 t)) f c = balExtraFrL t f c
balExtraFrBL (Br _ (Br2 tˡ tʳ)) f c = balExtraFrLL tˡ tʳ f c

balExtraFrTBL ∷ (Monoid o) ⇒ Tr hᴵ o a → Br hᴵ o a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrTBL tˡ (Br _ (Br1 tʳ)) f c = balExtraFrLL tˡ tʳ f c
balExtraFrTBL tˡ (Br _ (Br2 tᵐ tʳ)) f c = balExtraFrLLL tˡ tᵐ tʳ f c

balExtraFrR ∷ (Monoid o) ⇒ Fr hᴵ hᴼ o a → Tr hᴵ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrR (Fr _ Fr0) t c = EqTlFr $ frT t c
balExtraFrR (Fr _ (Fr1 tˡ)) tʳ c = SuccFr $ frTT tˡ tʳ c
balExtraFrR (Fr _ (Fr2 bˡ f (Br _ (Br1 tᵐ)))) tʳ c = EqTlFr $ frBFTT bˡ f tᵐ tʳ c
balExtraFrR (Fr _ (Fr2 bᴸ f (Br _ (Br2 tˡ tʳ)))) tᴿ c = balExtraFrR f (tr2 tˡ tʳ) $ InFr2 bᴸ () (br1 tᴿ) c

balExtraFrRR ∷ (Monoid o) ⇒ Fr hᴵ hᴼ o a → Tr hᴵ o a → Tr hᴵ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrRR (Fr _ Fr0) tˡ tʳ c = SuccFr $ frTT tˡ tʳ c
balExtraFrRR (Fr _ (Fr1 tˡ)) tᵐ tʳ c = SuccFr $ frTTT tˡ tᵐ tʳ c
balExtraFrRR (Fr _ (Fr2 bᴸ f (Br _ (Br1 tˡ)))) tʳ tᴿ c = balExtraFrR f (tr2 tˡ tʳ) $ InFr2 bᴸ () (br1 tᴿ) c
balExtraFrRR (Fr _ (Fr2 bᴸ f (Br _ (Br2 tˡ tᵐ)))) tʳ tᴿ c = balExtraFrR f (tr3 tˡ tᵐ tʳ) $ InFr2 bᴸ () (br1 tᴿ) c

balExtraFrRRR ∷ (Monoid o) ⇒ Fr hᴵ hᴼ o a → Tr hᴵ o a → Tr hᴵ o a → Tr hᴵ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrRRR (Fr _ Fr0) tˡ tᵐ tʳ c = SuccFr $ frTTT tˡ tᵐ tʳ c
balExtraFrRRR (Fr _ (Fr1 tᴸ)) tˡ tʳ tᴿ c = SuccFr $ frTTTT tᴸ tˡ tʳ tᴿ c
balExtraFrRRR (Fr _ (Fr2 bᴸ f (Br _ (Br1 tˡ)))) tᵐ tʳ tᴿ c = balExtraFrR f (tr3 tˡ tᵐ tʳ) $ InFr2 bᴸ () (br1 tᴿ) c
balExtraFrRRR (Fr _ (Fr2 bᴸ f (Br _ (Br2 tˡ tᵐ)))) tⁿ tʳ tᴿ c = balExtraFrR f (tr3 tˡ tᵐ tⁿ) $ InFr2 bᴸ () (br2 tʳ tᴿ) c

balExtraFrBR ∷ (Monoid o) ⇒ Fr hᴵ hᴼ o a → Br hᴵ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrBR f (Br _ (Br1 t)) c = balExtraFrR f t c
balExtraFrBR f (Br _ (Br2 tˡ tʳ)) c = balExtraFrRR f tˡ tʳ c

balExtraFrBTR ∷ (Monoid o) ⇒ Fr hᴵ hᴼ o a → Br hᴵ o a → Tr hᴵ o a → FrK hᴵ o a → ExTallFr hᴼ o a
balExtraFrBTR f (Br _ (Br1 tˡ)) tʳ c = balExtraFrRR f tˡ tʳ c
balExtraFrBTR f (Br _ (Br2 tˡ tᵐ)) tʳ c = balExtraFrRRR f tˡ tᵐ tʳ c

balTallTrFr ∷ (Monoid o) ⇒ Tr hᴵ o a → Tr hᴵ o a → TrInFr hᴵ hᴼ o a → ExTallFr hᴼ o a
balTallTrFr tˡ tʳ (InFr1 () c) = SuccFr $ frTT tˡ tʳ c
balTallTrFr tˡ tʳ (InBr1 () c) = EqTlFr $ zipFrBr (br2 tˡ tʳ) c
balTallTrFr tᴸ tˡ (InBr2L () tʳ (InFr2L () f bᴿ c)) = balExtraFrL tᴸ (fr2 (br2 tˡ tʳ) f bᴿ) c
balTallTrFr tˡ tʳ (InBr2L () tᴿ (InFr2R bᴸ f () c)) = balExtraFrR (fr2 bᴸ f (br2 tˡ tʳ)) tᴿ c
balTallTrFr tˡ tʳ (InBr2R tᴸ () (InFr2L () f bᴿ c)) = balExtraFrL tᴸ (fr2 (br2 tˡ tʳ) f bᴿ) c
balTallTrFr tʳ tᴿ (InBr2R tˡ () (InFr2R bᴸ f () c)) = balExtraFrR (fr2 bᴸ f (br2 tˡ tʳ)) tᴿ c

balExTallTrFr ∷ (Monoid o) ⇒ ExTallTr hᴵ o a → TrInFr hᴵ hᴼ o a → ExTallFr hᴼ o a
balExTallTrFr (EqTlTr t) c = EqTlFr $ zipFrTr t c
balExTallTrFr (SuccTr tˡ tʳ) c = balTallTrFr tˡ tʳ c

balHoleFrL ∷ (Monoid o) ⇒ Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → FrK hᴵ o a → ExShortFr hᴼ o a
balHoleFrL (Fr _ Fr0) (Br _ (Br1 t)) c = PredFr $ zipFr (fr1 t) c
balHoleFrL (Fr _ Fr0) (Br _ (Br2 tˡ tʳ)) c = EqShFr $ zipFr (fr2 (br1 tˡ) fr0 (br1 tʳ)) c
balHoleFrL (Fr _ (Fr1 (Tr _ (Tr2 tˡ tᵐ)))) bʳ c = EqShFr $ zipFr (fr2 (br2 tˡ tᵐ) fr0 bʳ) c
balHoleFrL (Fr _ (Fr1 (Tr _ (Tr3 tᴸ tˡ tʳ)))) bᴿ c = EqShFr $ zipFr (fr2 (br1 tᴸ) (fr1 (tr2 tˡ tʳ)) bᴿ) c
balHoleFrL (Fr _ (Fr2 (Br _ (Br1 (Tr _ (Tr2 tᴸ tˡ)))) f bʳ)) bᴿ c = balHoleFrL f bʳ $ InFr2 (br2 tᴸ tˡ) () bᴿ c
balHoleFrL (Fr _ (Fr2 (Br _ (Br1 (Tr _ (Tr3 tᴸ tˡ tᵐ)))) f bʳ)) bᴿ c = EqShFr $ zipFr (fr2 (br1 tᴸ) (fr2 (br1 (tr2 tˡ tᵐ)) f bʳ) bᴿ) c
balHoleFrL (Fr _ (Fr2 (Br _ (Br2 (Tr _ (Tr2 tᴸ tˡ)) tᵐ)) f bʳ)) bᴿ c = EqShFr $ zipFr (fr2 (br2 tᴸ tˡ) (fr2 (br1 tᵐ) f bʳ) bᴿ) c
balHoleFrL (Fr _ (Fr2 (Br _ (Br2 (Tr _ (Tr3 tᴸ tˡ tᵐ)) tⁿ)) f bʳ)) bᴿ c = EqShFr $ zipFr (fr2 (br1 tᴸ) (fr2 (br2 (tr2 tˡ tᵐ) tⁿ) f bʳ) bᴿ) c

balHoleFrR ∷ (Monoid o) ⇒ Br hᴵ o a → Fr ('S hᴵ) hᴼ o a → FrK hᴵ o a → ExShortFr hᴼ o a
balHoleFrR (Br _ (Br1 t)) (Fr _ Fr0) c = PredFr $ zipFr (fr1 t) c
balHoleFrR (Br _ (Br2 tˡ tʳ)) (Fr _ Fr0) c = EqShFr $ zipFr (fr2 (br1 tˡ) fr0 (br1 tʳ)) c
balHoleFrR bˡ (Fr _ (Fr1 (Tr _ (Tr2 tᵐ tʳ)))) c = EqShFr $ zipFr (fr2 bˡ fr0 (br2 tᵐ tʳ)) c
balHoleFrR bᴸ (Fr _ (Fr1 (Tr _ (Tr3 tˡ tʳ tᴿ)))) c = EqShFr $ zipFr (fr2 bᴸ (fr1 (tr2 tˡ tʳ)) (br1 tᴿ)) c
balHoleFrR bᴸ (Fr _ (Fr2 bˡ f (Br _ (Br1 (Tr _ (Tr2 tʳ tᴿ)))))) c = balHoleFrR bˡ f $ InFr2 bᴸ () (br2 tʳ tᴿ) c
balHoleFrR bᴸ (Fr _ (Fr2 bˡ f (Br _ (Br1 (Tr _ (Tr3 tᵐ tʳ tᴿ)))))) c = EqShFr $ zipFr (fr2 bᴸ (fr2 bˡ f (br1 (tr2 tᵐ tʳ))) (br1 tᴿ)) c
balHoleFrR bᴸ (Fr _ (Fr2 bˡ f (Br _ (Br2 tᵐ (Tr _ (Tr2 tʳ tᴿ)))))) c = EqShFr $ zipFr (fr2 bᴸ (fr2 bˡ f (br1 tᵐ)) (br2 tʳ tᴿ)) c
balHoleFrR bᴸ (Fr _ (Fr2 bˡ f (Br _ (Br2 tᵐ (Tr _ (Tr3 tⁿ tʳ tᴿ)))))) c = EqShFr $ zipFr (fr2 bᴸ (fr2 bˡ f (br2 tᵐ (tr2 tⁿ tʳ))) (br1 tᴿ)) c

balShortBrFr ∷ (Monoid o) ⇒ Tr hᴵ o a → BrInFr ('S hᴵ) hᴼ o a → ExShortFr hᴼ o a
balShortBrFr tˡ (InFr2L () f bʳ (InFr2 (Br _ (Br1 tᴸ)) () bᴿ c)) = balHoleFrL (fr2 (br1 (tr2 tᴸ tˡ)) f bʳ) bᴿ c
balShortBrFr tᵐ (InFr2L () f bʳ (InFr2 (Br _ (Br2 tᴸ tˡ)) () bᴿ c)) = balHoleFrL (fr2 (br1 (tr3 tᴸ tˡ tᵐ)) f bʳ) bᴿ c
balShortBrFr tʳ (InFr2R bˡ f () (InFr2 bᴸ () (Br _ (Br1 tᴿ)) c)) = balHoleFrR bᴸ (fr2 bˡ f (br1 (tr2 tʳ tᴿ))) c
balShortBrFr tᵐ (InFr2R bˡ f () (InFr2 bᴸ () (Br _ (Br2 tʳ tᴿ)) c)) = balHoleFrR bᴸ (fr2 bˡ f (br1 (tr3 tᵐ tʳ tᴿ))) c

balShortTrFr ∷ (Monoid o) ⇒ Tr hᴵ o a → TrInFr ('S hᴵ) hᴼ o a → ExShortFr hᴼ o a
balShortTrFr tᵐ (InFr1 () (InFr2 (Br _ (Br1 tˡ)) () bʳ c)) = balHoleFrL (fr1 (tr2 tˡ tᵐ)) bʳ c
balShortTrFr tʳ (InFr1 () (InFr2 (Br _ (Br2 tᴸ tˡ)) () bᴿ c)) = balHoleFrL (fr1 (tr3 tᴸ tˡ tʳ)) bᴿ c
balShortTrFr t (InBr1 () c) = balShortBrFr t c
balShortTrFr tˡ (InBr2L () (Tr _ (Tr2 tᵐ tʳ)) c) = EqShFr $ zipFrBr (br1 (tr3 tˡ tᵐ tʳ)) c
balShortTrFr tᴸ (InBr2L () (Tr _ (Tr3 tˡ tʳ tᴿ)) c) = EqShFr $ zipFrBr (br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)) c
balShortTrFr tʳ (InBr2R (Tr _ (Tr2 tˡ tᵐ)) () c) = EqShFr $ zipFrBr (br1 (tr3 tˡ tᵐ tʳ)) c
balShortTrFr tᴿ (InBr2R (Tr _ (Tr3 tᴸ tˡ tʳ)) () c) = EqShFr $ zipFrBr (br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)) c

balHoleTrFr ∷ (Monoid o) ⇒ TrInFr 'Z h o a → ExShortFr h o a
balHoleTrFr (InFr1 () TopFr) = EqShFr fr0
balHoleTrFr (InBr1 () (InFr2L () f bʳ c)) = balHoleFrL f bʳ c
balHoleTrFr (InBr1 () (InFr2R bˡ f () c)) = balHoleFrR bˡ f c
balHoleTrFr (InBr2L () b c) = EqShFr $ zipFrBr (br1 b) c
balHoleTrFr (InBr2R b () c) = EqShFr $ zipFrBr (br1 b) c

balExShortTrFr ∷ (Monoid o) ⇒ ExShortTr hᴵ o a → TrInFr hᴵ hᴼ o a → ExShortFr hᴼ o a
balExShortTrFr ZeroTr c = balHoleTrFr c
balExShortTrFr (EqShTr t) c = EqShFr $ zipFrTr t c
balExShortTrFr (PredTr t) c = balShortTrFr t c

brBB ∷ (Append o) ⇒ Br h o a → Br h o a → Br ('S h) o a
brBB (Br _ (Br1 tˡ)) (Br _ (Br1 tʳ)) = br1 (tr2 tˡ tʳ)
brBB (Br _ (Br1 tˡ)) (Br _ (Br2 tᵐ tʳ)) = br1 (tr3 tˡ tᵐ tʳ)
brBB (Br _ (Br2 tˡ tᵐ)) (Br _ (Br1 tʳ)) = br1 (tr3 tˡ tᵐ tʳ)
brBB (Br _ (Br2 tᴸ tˡ)) (Br _ (Br2 tʳ tᴿ)) = br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)

brBBB ∷ (Append o) ⇒ Br h o a → Br h o a → Br h o a → Br ('S h) o a
brBBB (Br _ (Br1 tˡ)) (Br _ (Br1 tᵐ)) (Br _ (Br1 tʳ)) = br1 (tr3 tˡ tᵐ tʳ)
brBBB (Br _ (Br1 tᴸ)) (Br _ (Br1 tˡ)) (Br _ (Br2 tʳ tᴿ)) = br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br1 tᴸ)) (Br _ (Br2 tˡ tʳ)) (Br _ (Br1 tᴿ)) = br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br2 tᴸ tˡ)) (Br _ (Br1 tʳ)) (Br _ (Br1 tᴿ)) = br2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br1 tᴸ)) (Br _ (Br2 tˡ tᵐ)) (Br _ (Br2 tʳ tᴿ)) = br2 (tr3 tᴸ tˡ tᵐ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br2 tᴸ tˡ)) (Br _ (Br1 tᵐ)) (Br _ (Br2 tʳ tᴿ)) = br2 (tr3 tᴸ tˡ tᵐ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br2 tᴸ tˡ)) (Br _ (Br2 tᵐ tʳ)) (Br _ (Br1 tᴿ)) = br2 (tr3 tᴸ tˡ tᵐ) (tr2 tʳ tᴿ)
brBBB (Br _ (Br2 tᴸ tˡ)) (Br _ (Br2 tᵐ tⁿ)) (Br _ (Br2 tʳ tᴿ)) = br2 (tr3 tᴸ tˡ tᵐ) (tr3 tⁿ tʳ tᴿ)

consFrBrK ∷ (Monoid o,Summary o a) ⇒ a → Br hᴵ o a → BrInFr hᴵ hᴼ o a → ExTallFr hᴼ o a
consFrBrK x (Br _ (Br1 t)) c = balExTallTrFr (consTr x t) $ InBr1 () c
consFrBrK x (Br _ (Br2 tˡ tʳ)) c = balExTallTrFr (consTr x tˡ) $ InBr2L () tʳ c

snocFrBrK ∷ (Monoid o,Summary o a) ⇒ Br hᴵ o a → a → BrInFr hᴵ hᴼ o a → ExTallFr hᴼ o a
snocFrBrK (Br _ (Br1 t)) x c = balExTallTrFr (snocTr t x) $ InBr1 () c
snocFrBrK (Br _ (Br2 tˡ tʳ)) x c = balExTallTrFr (snocTr tʳ x) $ InBr2R tˡ () c

consFrK ∷ (Monoid o,Summary o a) ⇒ a → Fr hᴵ hᴼ o a → FrK hᴵ o a → ExTallFr hᴼ o a
consFrK x (Fr _ Fr0) TopFr = EqTlFr $ fr1 (tr0 x)
consFrK x (Fr _ (Fr1 t)) TopFr = balExTallTrFr (consTr x t) $ InFr1 () TopFr
consFrK x (Fr _ (Fr2 bˡ f bʳ)) TopFr = consFrBrK x bˡ $ InFr2L () f bʳ TopFr
consFrK x f (InFr2 bˡ () bʳ c) = snocFrBrK bˡ x $ InFr2L () f bʳ c

snocFrK ∷ (Monoid o,Summary o a) ⇒ Fr hᴵ hᴼ o a → a → FrK hᴵ o a → ExTallFr hᴼ o a
snocFrK (Fr _ Fr0) x TopFr = EqTlFr $ fr1 (tr0 x)
snocFrK (Fr _ (Fr1 t)) x TopFr = balExTallTrFr (snocTr t x) $ InFr1 () TopFr
snocFrK (Fr _ (Fr2 bˡ f bʳ)) x c = snocFrBrK bʳ x $ InFr2R bˡ f () c
snocFrK f x (InFr2 bˡ () bʳ c) = consFrBrK x bʳ $ InFr2R bˡ f () c

insertPosFr ∷ (Monoid o,Summary o a) ⇒ a → PositionFr h o a → ExTallFr h o a
insertPosFr x (PositionFr0 c) = consFrK x fr0 c
insertPosFr x (PositionFr2 d f c) = case d of
  Left → consFrK x f c
  Right → snocFrK f x c
insertPosFr x (PositionFrBr d b c) = case d of
  Left → consFrBrK x b c
  Right → snocFrBrK b x c
insertPosFr x (PositionFrTr p c) = balExTallTrFr (insertPosTr x p) c

-- sequence operations

consFr ∷ (Monoid o,Summary o a) ⇒ a → Fr 'Z h o a → ExTallFr h o a
consFr x f = balExtraFrL (tr0 x) f TopFr

snocFr ∷ (Monoid o,Summary o a) ⇒ Fr 'Z h o a → a → ExTallFr h o a
snocFr f x = balExtraFrR f (tr0 x) TopFr

unconsFr ∷ (Monoid o) ⇒ Fr 'Z h o a → 𝑂 (a ∧ ExShortFr h o a)
unconsFr (Fr _ Fr0) = None
unconsFr (Fr _ (Fr1 (Tr _ (Tr0 x)))) = Some (x :* EqShFr fr0)
unconsFr (Fr _ (Fr2 (Br _ (Br1 (Tr _ (Tr0 x)))) f bʳ)) = Some (x :* balHoleFrL f bʳ TopFr)
unconsFr (Fr _ (Fr2 (Br _ (Br2 (Tr _ (Tr0 x)) tˡ)) f bʳ)) = Some (x :* EqShFr (fr2 (br1 tˡ) f bʳ))

unsnocFr ∷ (Monoid o) ⇒ Fr 'Z h o a → 𝑂 (ExShortFr h o a ∧ a)
unsnocFr (Fr _ Fr0) = None
unsnocFr (Fr _ (Fr1 (Tr _ (Tr0 x)))) = Some (EqShFr fr0 :* x)
unsnocFr (Fr _ (Fr2 bˡ f (Br _ (Br1 (Tr _ (Tr0 x)))))) = Some (balHoleFrR bˡ f TopFr :* x)
unsnocFr (Fr _ (Fr2 bˡ f (Br _ (Br2 tʳ (Tr _ (Tr0 x)))))) = Some (EqShFr (fr2 bˡ f (br1 tʳ)) :* x)

appendFrN ∷ (Monoid o) ⇒ Fr hᴵ hᴼ₁ o a → Br hᴵ o a → Fr hᴵ hᴼ₂ o a → FrK hᴵ o a → ExFr o a
appendFrN (Fr _ Fr0) bˡ fʳ c = exFrTall $ balExtraFrBL bˡ fʳ c
appendFrN fˡ bʳ (Fr _ Fr0) c = exFrTall $ balExtraFrBR fˡ bʳ c
appendFrN (Fr _ (Fr1 tˡ)) bᵐ fʳ c = exFrTall $ balExtraFrTBL tˡ bᵐ fʳ c
appendFrN fˡ bᵐ (Fr _ (Fr1 tʳ)) c = exFrTall $ balExtraFrBTR fˡ bᵐ tʳ c
appendFrN (Fr _ (Fr2 bᴸ fˡ bᵐ)) bⁿ (Fr _ (Fr2 bᵒ fʳ bᴿ)) c = appendFrN fˡ (brBBB bᵐ bⁿ bᵒ) fʳ $ InFr2 bᴸ () bᴿ c

appendFr ∷ (Monoid o) ⇒ Fr 'Z hᴼ₁ o a → Fr 'Z hᴼ₂ o a → ExFr o a
appendFr (Fr _ Fr0) f₂ = ExFr f₂
appendFr f₁ (Fr _ Fr0) = ExFr f₁
appendFr (Fr _ (Fr1 t)) f₂ = exFrTall $ balExtraFrL t f₂ TopFr
appendFr f₁ (Fr _ (Fr1 t)) = exFrTall $ balExtraFrR f₁ t TopFr
appendFr (Fr _ (Fr2 bᴸ fˡ bᵐ)) (Fr _ (Fr2 bⁿ fʳ bᴿ)) = appendFrN fˡ (brBB bᵐ bⁿ) fʳ $ InFr2 bᴸ () bᴿ TopFr

-- search operations

insertFr ∷ (Monoid o,Summary o a) ⇒ (o → Search) → (a → a → a) → a → Fr 'Z h o a → ExTallFr h o a
insertFr s u x f = case searchFr s f TopFr of
  NotFoundFr p → insertPosFr x p
  FoundFr x' c cc → EqTlFr $ zipFrTr (zipTr (tr0 (u x x')) c) cc

lookupFr ∷ (o → Search) → Fr 'Z h o a → 𝑂 a
lookupFr s f = case searchFr s f TopFr of
  NotFoundFr _ → None
  FoundFr x _ _ → Some x

removeFr ∷ (Monoid o) ⇒ (o → Search) → Fr 'Z h o a → 𝑂 (a ∧ ExShortFr h o a)
removeFr s t = case searchFr s t TopFr of
  NotFoundFr _ → None
  FoundFr x c cc → Some (x :* balExShortTrFr (balHoleTr c) cc)

-- mapping

mapBr ∷ (a → b) → Br h o a → Br h o b
mapBr g (Br o b) = Br o (mapBrI g b)

mapBrI ∷ (a → b) → BrI h o a → BrI h o b
mapBrI g (Br1 t) = Br1 (mapTr g t)
mapBrI g (Br2 tˡ tʳ) = Br2 (mapTr g tˡ) (mapTr g tʳ)

mapFr ∷ (a → b) → Fr hᴵ hᴼ o a → Fr hᴵ hᴼ o b
mapFr g (Fr o f) = Fr o (mapFrI g f)

mapFrI ∷ (a → b) → FrI hᴵ hᴼ o a → FrI hᴵ hᴼ o b
mapFrI g = \case
  Fr0 → Fr0
  Fr1 t → Fr1 (mapTr g t)
  Fr2 bˡ f bʳ → Fr2 (mapBr g bˡ) (mapFr g f) (mapBr g bʳ)

-- Iter

iterBr ∷ Br h o a → (a → b → b) → b → b
iterBr (Br _ (Br1 t)) g = iterTr t g
iterBr (Br _ (Br2 tˡ tʳ)) g = iterTr tʳ g ∘ iterTr tˡ g

iterFr ∷ Fr hᴵ hᴼ o a → (a → b → b) → b → b
iterFr (Fr _ Fr0) _ = id
iterFr (Fr _ (Fr1 t)) g = iterTr t g
iterFr (Fr _ (Fr2 bˡ f bʳ)) g = iterBr bʳ g ∘ iterFr f g ∘ iterBr bˡ g

instance ToIter a (Fr hᴵ hᴼ o a) where iter xs = 𝐼 $ iterFr xs

-- Stream

data FrK𝑆 o a where
  TopFr𝑆 ∷ ∀ o a. FrK𝑆 o a
  InFr2𝑆 ∷ ∀ h o a. () → Br h o a → FrK𝑆 o a → FrK𝑆 o a

data BrInFr𝑆 o a where
  InFr2L𝑆 ∷ ∀ hᴵ hᴼ o a. () → Fr ('S hᴵ) hᴼ o a → Br hᴵ o a → FrK𝑆 o a → BrInFr𝑆 o a
  InFr2R𝑆 ∷ ∀ o a . () → FrK𝑆 o a → BrInFr𝑆 o a

data TrInFr𝑆 o a  where
  InFr1𝑆 ∷ ∀ o a. () → FrK𝑆 o a → TrInFr𝑆 o a
  InBr1𝑆 ∷ ∀ o a. () → BrInFr𝑆 o a → TrInFr𝑆 o a
  InBr2L𝑆 ∷ ∀ h o a. () → Tr h o a → BrInFr𝑆 o a → TrInFr𝑆 o a

locFstTrFr𝑆 ∷ Tr h o a → TrInFr𝑆 o a → a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a
locFstTrFr𝑆 t cc = locFstTr𝑆 t TopTr𝑆 :* cc

locFstBr𝑆 ∷ Br h o a → BrInFr𝑆 o a → a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a
locFstBr𝑆 (Br _ (Br1 t)) cc = locFstTrFr𝑆 t $ InBr1𝑆 () cc
locFstBr𝑆 (Br _ (Br2 tˡ tʳ)) cc = locFstTrFr𝑆 tˡ $ InBr2L𝑆 () tʳ cc

locFstFrK𝑆 ∷ FrK𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a)
locFstFrK𝑆 TopFr𝑆 = None
locFstFrK𝑆 (InFr2𝑆 () b cc) = Some $ locFstBr𝑆 b $ InFr2R𝑆 () cc

locFstFr𝑆 ∷ Fr hᴵ hᴼ o a → FrK𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a)
locFstFr𝑆 (Fr _ Fr0) cc = locFstFrK𝑆 cc
locFstFr𝑆 (Fr _ (Fr1 t)) cc = Some $ locFstTrFr𝑆 t $ InFr1𝑆 () cc
locFstFr𝑆 (Fr _ (Fr2 bˡ f bʳ)) cc = Some $ locFstBr𝑆 bˡ $ InFr2L𝑆 () f bʳ cc

locFstBrK𝑆 ∷ BrInFr𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a)
locFstBrK𝑆 (InFr2L𝑆 () f b cc) = locFstFr𝑆 f $ InFr2𝑆 () b cc
locFstBrK𝑆 (InFr2R𝑆 () cc) = locFstFrK𝑆 cc

locFstTrFrK𝑆 ∷ TrInFr𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a)
locFstTrFrK𝑆 (InFr1𝑆 () cc) = locFstFrK𝑆 cc
locFstTrFrK𝑆 (InBr1𝑆 () cc) = locFstBrK𝑆 cc
locFstTrFrK𝑆 (InBr2L𝑆 () t cc) = Some $ locFstTrFr𝑆 t $ InBr1𝑆 () cc

nextFr𝑆 ∷ TrK𝑆 o a → TrInFr𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a ∧ TrInFr𝑆 o a)
nextFr𝑆 c cc = case nextTr𝑆 c of
  None → locFstTrFrK𝑆 cc
  Some (x :* c') → Some (x :* c' :* cc)

streamFr ∷ Fr hᴵ hᴼ o a → 𝑆 a
streamFr f  = 𝑆 (locFstFr𝑆 f TopFr𝑆) $ \case
  None → None
  Some (x :* c :* cc) → Some (x :* nextFr𝑆 c cc)

instance ToStream a (Fr hᴵ hᴼ o a) where stream = streamFr
