module CoreData.Tr where

import PreCore
import CoreClasses

import CoreData.Search

data Tr h o a = Tr {sryTr ∷ o,tr ∷ TrI h o a}

data TrI h o a where
  Tr0 ∷ ∀ o a. a → TrI 'Z o a
  Tr2 ∷ ∀ h o a. Tr h o a → Tr h o a → TrI ('S h) o a
  Tr3 ∷ ∀ h o a. Tr h o a → Tr h o a → Tr h o a → TrI ('S h) o a

data ExTr o a where
  ExTr ∷ ∀ h o a. Tr h o a → ExTr o a

data ExTallTr h o a where 
  EqTlTr ∷ ∀ h o a. Tr h o a → ExTallTr h o a
  SuccTr ∷ ∀ h o a. Tr h o a → Tr h o a → ExTallTr h o a

data ExShortTr h o a where
  ZeroTr ∷ ExShortTr 'Z o a
  EqShTr ∷ ∀ h o a. Tr h o a → ExShortTr h o a
  PredTr ∷ ∀ h o a. Tr h o a → ExShortTr ('S h) o a

data TrK hᴵ hᴼ o a where
  TopTr ∷ ∀ h o a. TrK h h o a
  InTr2L ∷ ∀ hᴵ hᴼ o a. () → Tr hᴵ o a → TrK ('S hᴵ) hᴼ o a → TrK hᴵ hᴼ o a
  InTr2R ∷ ∀ hᴵ hᴼ o a. Tr hᴵ o a → () → TrK ('S hᴵ) hᴼ o a → TrK hᴵ hᴼ o a
  InTr3L ∷ ∀ hᴵ hᴼ o a. () → Tr hᴵ o a → Tr hᴵ o a → TrK ('S hᴵ) hᴼ o a → TrK hᴵ hᴼ o a
  InTr3M ∷ ∀ hᴵ hᴼ o a. Tr hᴵ o a → () → Tr hᴵ o a → TrK ('S hᴵ) hᴼ o a → TrK hᴵ hᴼ o a
  InTr3R ∷ ∀ hᴵ hᴼ o a. Tr hᴵ o a → Tr hᴵ o a → () → TrK ('S hᴵ) hᴼ o a → TrK hᴵ hᴼ o a

data PositionTr h o a where
  PositionTr ∷ ∀ hᴵ hᴼ o a. Direction → Tr hᴵ o a → TrK hᴵ hᴼ o a → PositionTr hᴼ o a

data SearchTr h o a where
  NotFoundTr ∷ ∀ h o a. PositionTr h o a → SearchTr h o a
  FoundTr ∷ ∀ h o a. a → TrK 'Z h o a → SearchTr h o a

tr0 ∷ (Summary o a) ⇒ a → Tr 'Z o a
tr0 x = Tr (initSummary x) $ Tr0 x

tr2 ∷ (Append o) ⇒ Tr h o a → Tr h o a → Tr ('S h) o a
tr2 tˡ tʳ = Tr (sryTr tˡ ⧺ sryTr tʳ) $ Tr2 tˡ tʳ

tr3 ∷ (Append o) ⇒ Tr h o a → Tr h o a → Tr h o a → Tr ('S h) o a
tr3 tˡ tᵐ tʳ = Tr (sryTr tˡ ⧺ sryTr tᵐ ⧺ sryTr tʳ) $ Tr3 tˡ tᵐ tʳ

exTrTall ∷ (Append o) ⇒ ExTallTr h o a → ExTr o a
exTrTall (EqTlTr t) = ExTr t
exTrTall (SuccTr tˡ tʳ) = ExTr $ tr2 tˡ tʳ

exTrShort ∷ ExShortTr h o a → 𝑂 (ExTr o a)
exTrShort ZeroTr = None
exTrShort (EqShTr t) = Some $ ExTr t
exTrShort (PredTr t) = Some $ ExTr t

zipTr ∷ (Append o) ⇒ Tr hᴵ o a → TrK hᴵ hᴼ o a → Tr hᴼ o a
zipTr t TopTr = t
zipTr tˡ (InTr2L () tʳ c) = zipTr (tr2 tˡ tʳ) c
zipTr tʳ (InTr2R tˡ () c) = zipTr (tr2 tˡ tʳ) c
zipTr tˡ (InTr3L () tᵐ tʳ c) = zipTr (tr3 tˡ tᵐ tʳ) c
zipTr tᵐ (InTr3M tˡ () tʳ c) = zipTr (tr3 tˡ tᵐ tʳ) c
zipTr tʳ (InTr3R tˡ tᵐ () c) = zipTr (tr3 tˡ tᵐ tʳ) c

fuseTr ∷ TrK h₁ h₂ o a → TrK h₂ h₃ o a → TrK h₁ h₃ o a
fuseTr TopTr c = c
fuseTr (InTr2L () tʳ c₁) c₂ = InTr2L () tʳ $ fuseTr c₁ c₂
fuseTr (InTr2R tˡ () c₁) c₂ = InTr2R tˡ () $ fuseTr c₁ c₂
fuseTr (InTr3L () tᵐ tʳ c₁) c₂ = InTr3L () tᵐ tʳ $ fuseTr c₁ c₂
fuseTr (InTr3M tˡ () tʳ c₁) c₂ = InTr3M tˡ () tʳ $ fuseTr c₁ c₂
fuseTr (InTr3R tˡ tᵐ () c₁) c₂ = InTr3R tˡ tᵐ () $ fuseTr c₁ c₂

locFstTr ∷ Tr hᴵ o a → TrK hᴵ hᴼ o a → a ∧ TrK 'Z hᴼ o a
locFstTr (Tr _ (Tr0 x)) c = x :* c
locFstTr (Tr _ (Tr2 tˡ tʳ)) c = locFstTr tˡ $ InTr2L () tʳ c
locFstTr (Tr _ (Tr3 tˡ tᵐ tʳ)) c = locFstTr tˡ $ InTr3L () tᵐ tʳ c

locLstTr ∷ Tr hᴵ o a → TrK hᴵ hᴼ o a → a ∧ TrK 'Z hᴼ o a
locLstTr (Tr _ (Tr0 x)) c = x :* c
locLstTr (Tr _ (Tr2 tˡ tʳ)) c = locLstTr tʳ $ InTr2R tˡ () c
locLstTr (Tr _ (Tr3 tˡ tᵐ tʳ)) c = locLstTr tʳ $ InTr3R tˡ tᵐ () c

searchTr ∷ (o → Search) → Tr hᴵ o a → TrK hᴵ hᴼ o a → SearchTr hᴼ o a
searchTr s t c = case s $ sryTr t of
  L → NotFoundTr $ PositionTr Left t c
  R → NotFoundTr $ PositionTr Right t c
  C → searchTrI s (tr t) c
  N → error "search direction should never be N"
  
searchTrI ∷ (o → Search) → TrI hᴵ o a → TrK hᴵ hᴼ o a → SearchTr hᴼ o a
searchTrI _ (Tr0 x) c = FoundTr x c
searchTrI s (Tr2 tˡ tʳ) c = case (s $ sryTr tˡ,s $ sryTr tʳ) of
  (L,_) → NotFoundTr $ PositionTr Left tˡ $ InTr2L () tʳ c
  (C,_) → searchTrI s (tr tˡ) $ InTr2L () tʳ c
  (R,L) → NotFoundTr $ PositionTr Left tʳ $ InTr2R tˡ () c
  (_,C) → searchTrI s (tr tʳ) $ InTr2R tˡ () c
  (_,R) → NotFoundTr $ PositionTr Right tʳ $ InTr2R tˡ () c
  (N,_) → error "search direction should never be N"
  (_,N) → error "search direction should never be N"
searchTrI s (Tr3 tˡ tᵐ tʳ) c = case (s $ sryTr tˡ,s $ sryTr tᵐ,s $ sryTr tʳ) of
  (L,_,_) → NotFoundTr $ PositionTr Left tˡ $ InTr3L () tᵐ tʳ c
  (C,_,_) → searchTrI s (tr tˡ) $ InTr3L () tᵐ tʳ c
  (R,L,_) → NotFoundTr $ PositionTr Left tᵐ $ InTr3M tˡ () tʳ c
  (_,C,_) → searchTrI s (tr tᵐ) $ InTr3M tˡ () tʳ c
  (_,R,L) → NotFoundTr $ PositionTr Left tʳ $ InTr3R tˡ tᵐ () c
  (_,_,C) → searchTrI s (tr tʳ) $ InTr3R tˡ tᵐ () c
  (_,_,R) → NotFoundTr $ PositionTr Right tʳ $ InTr3R tˡ tᵐ () c
  (N,_,_) → error "search direction should never be N"
  (_,N,_) → error "search direction should never be N"
  (_,_,N) → error "search direction should never be N"

balTallTr ∷ (Append o) ⇒ Tr hᴵ o a → Tr hᴵ o a → TrK hᴵ hᴼ o a → ExTallTr hᴼ o a
balTallTr tˡ tʳ c = case c of
  TopTr → SuccTr tˡ tʳ
  InTr2L () tʳ' c' → EqTlTr $ zipTr (tr3 tˡ tʳ tʳ') c'
  InTr2R tˡ' () c' → EqTlTr $ zipTr (tr3 tˡ' tˡ tʳ) c'
  InTr3L () tᵐ' tʳ' c' → balTallTr (tr2 tˡ tʳ) (tr2 tᵐ' tʳ') c'
  InTr3M tˡ' () tʳ' c' → balTallTr (tr2 tˡ' tˡ) (tr2 tʳ tʳ') c'
  InTr3R tˡ' tᵐ' () c' → balTallTr (tr2 tˡ' tᵐ') (tr2 tˡ tʳ) c'

balShortTr ∷ (Append o) ⇒ Tr hᴵ o a → TrK ('S hᴵ) hᴼ o a → ExShortTr hᴼ o a
balShortTr t TopTr = PredTr t
balShortTr tˡ (InTr2L () (Tr _ (Tr2 tᵐ tʳ)) c) = balShortTr (tr3 tˡ tᵐ tʳ) c
balShortTr tʳ (InTr2R (Tr _ (Tr2 tˡ tᵐ)) () c) = balShortTr (tr3 tˡ tᵐ tʳ) c
balShortTr tᴸ (InTr2L () (Tr _ (Tr3 tˡ tʳ tᴿ)) c) = EqShTr $ zipTr (tr2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)) c
balShortTr tᴿ (InTr2R (Tr _ (Tr3 tᴸ tˡ tʳ)) () c) = EqShTr $ zipTr (tr2 (tr2 tᴸ tˡ) (tr2 tʳ tᴿ)) c
balShortTr tᴸ (InTr3L () (Tr _ (Tr2 tˡ tʳ)) tᴿ c) = EqShTr $ zipTr (tr2 (tr3 tᴸ tˡ tʳ) tᴿ) c
balShortTr tʳ (InTr3M (Tr _ (Tr2 tᴸ tˡ)) () tᴿ c) = EqShTr $ zipTr (tr2 (tr3 tᴸ tˡ tʳ) tᴿ) c
balShortTr tᴿ (InTr3R tᴸ (Tr _ (Tr2 tˡ tʳ)) () c) = EqShTr $ zipTr (tr2 tᴸ (tr3 tˡ tʳ tᴿ)) c
balShortTr tᴸ (InTr3L () (Tr _ (Tr3 tˡ tᵐ tʳ)) tᴿ c) = EqShTr $ zipTr (tr3 (tr2 tᴸ tˡ) (tr2 tᵐ tʳ) tᴿ) c
balShortTr tʳ (InTr3M (Tr _ (Tr3 tᴸ tˡ tᵐ)) () tᴿ c) = EqShTr $ zipTr (tr3 (tr2 tᴸ tˡ) (tr2 tᵐ tʳ) tᴿ) c
balShortTr tᴿ (InTr3R tᴸ (Tr _ (Tr3 tˡ tᵐ tʳ)) () c) = EqShTr $ zipTr (tr3 tᴸ (tr2 tˡ tᵐ) (tr2 tʳ tᴿ)) c

balHoleTr ∷ (Append o) ⇒ TrK 'Z h o a → ExShortTr h o a
balHoleTr TopTr = ZeroTr
balHoleTr (InTr2L () t c) = balShortTr t c
balHoleTr (InTr2R t () c) = balShortTr t c
balHoleTr (InTr3L () tˡ tʳ c) = EqShTr $ zipTr (tr2 tˡ tʳ) c
balHoleTr (InTr3M tˡ () tʳ c) = EqShTr $ zipTr (tr2 tˡ tʳ) c
balHoleTr (InTr3R tˡ tʳ () c) = EqShTr $ zipTr (tr2 tˡ tʳ) c

-- mid-level operations

consTrK ∷ (Append o,Summary o a) ⇒ a → Tr hᴵ o a → TrK hᴵ hᴼ o a → ExTallTr hᴼ o a
consTrK xˡ t c =
  let xʳ :* c' = locFstTr t c
  in balTallTr (tr0 xˡ) (tr0 xʳ) c'

snocTrK ∷ (Append o,Summary o a) ⇒ Tr hᴵ o a → a → TrK hᴵ hᴼ o a → ExTallTr hᴼ o a
snocTrK t xʳ c =
  let xˡ :* c' = locLstTr t c
  in balTallTr (tr0 xˡ) (tr0 xʳ) c'

insertPosTr ∷ (Append o,Summary o a) ⇒ a → PositionTr h o a → ExTallTr h o a
insertPosTr x (PositionTr d t c) = case d of {Left → consTrK x t c; Right → snocTrK t x c}

-- sequence operations

consTr ∷ (Append o,Summary o a) ⇒ a → Tr h o a → ExTallTr h o a
consTr x t = consTrK x t TopTr

snocTr ∷ (Append o,Summary o a) ⇒ Tr h o a → a → ExTallTr h o a
snocTr t x = snocTrK t x TopTr

unconsTr ∷ (Append o) ⇒ Tr h o a → a ∧ ExShortTr h o a
unconsTr t = 
  let x :* c = locFstTr t TopTr
  in x :* balHoleTr c

unsnocTr ∷ (Append o) ⇒ Tr h o a → ExShortTr h o a ∧ a
unsnocTr t =
  let x :* c = locLstTr t TopTr
  in balHoleTr c :* x

appendTrN ∷ (Append o) ⇒ TrK hᴵ hᴼ₁ o a → Tr hᴵ o a → Tr hᴵ o a → TrK hᴵ hᴼ₂ o a → ExTr o a
appendTrN TopTr tˡ tʳ cᴿ = exTrTall $ balTallTr tˡ tʳ cᴿ
appendTrN cᴸ tˡ tʳ TopTr = exTrTall $ balTallTr tˡ tʳ cᴸ
appendTrN (InTr2R tᴸ () cᴸ) tˡ tʳ (InTr2L () tᴿ cᴿ) = appendTrN cᴸ (tr2 tᴸ tˡ) (tr2 tʳ tᴿ) cᴿ
appendTrN (InTr3R tᴸ tˡ () cᴸ) tᵐˡ tʳ (InTr2L () tᴿ cᴿ) = appendTrN cᴸ (tr3 tᴸ tˡ tᵐˡ) (tr2 tʳ tᴿ) cᴿ
appendTrN (InTr2R tᴸ () cᴸ) tˡ tᵐʳ (InTr3L () tʳ tᴿ cᴿ) = appendTrN cᴸ (tr2 tᴸ tˡ) (tr3 tᵐʳ tʳ tᴿ) cᴿ
appendTrN (InTr3R tᴸ tˡ () cᴸ) tᵐˡ tᵐʳ (InTr3L () tʳ tᴿ cᴿ) = appendTrN cᴸ (tr3 tᴸ tˡ tᵐˡ) (tr3 tᵐʳ tʳ tᴿ) cᴿ
appendTrN (InTr2L () _ _) _ _ _ = error "impossible"
appendTrN (InTr3L () _ _ _) _ _ _ = error "impossible"
appendTrN (InTr3M _ () _ _) _ _ _ = error "impossible"
appendTrN _ _ _ (InTr2R _ () _) = error "impossible"
appendTrN _ _ _ (InTr3R _ _ () _) = error "impossible"
appendTrN _ _ _ (InTr3M _ () _ _) = error "impossible"

appendTr ∷ (Append o,Summary o a) ⇒ Tr h₁ o a → Tr h₂ o a → ExTr o a
appendTr tˡ tʳ =
  let xˡ :* cˡ = locLstTr tˡ TopTr
      xʳ :* cʳ = locFstTr tʳ TopTr
  in appendTrN cˡ (tr0 xˡ) (tr0 xʳ) cʳ

-- search operations

insertTr ∷ (Append o,Summary o a) ⇒ (o → Search) → (a → a → a) → a → Tr h o a → ExTallTr h o a
insertTr s u x t = case searchTr s t TopTr of
  NotFoundTr p → insertPosTr x p
  FoundTr x' c → EqTlTr $ zipTr (tr0 (u x x')) c

lookupTr ∷ (o → Search) → Tr h o a → 𝑂 a
lookupTr s t = case searchTr s t TopTr of
  NotFoundTr _ → None
  FoundTr x _ → Some x

removeTr ∷ (Append o) ⇒ (o → Search) → Tr h o a → 𝑂 (a ∧ ExShortTr h o a)
removeTr s t = case searchTr s t TopTr of
  NotFoundTr _ → None
  FoundTr x c → Some (x :* balHoleTr c)

-- mapping

mapTr ∷ (a → b) → Tr h o a → Tr h o b
mapTr f (Tr u t) = Tr u (mapTrI f t)

mapTrI ∷ (a → b) → TrI h o a → TrI h o b
mapTrI f = \case
  Tr0 x → Tr0 $ f x
  Tr2 tˡ tʳ → Tr2 (mapTr f tˡ) (mapTr f tʳ)
  Tr3 tˡ tᵐ tʳ → Tr3 (mapTr f tˡ) (mapTr f tᵐ) (mapTr f tʳ)

-- Iter

iterTr ∷ Tr h o a → (a → b → b) → b → b
iterTr (Tr _ (Tr0 x)) f = f x
iterTr (Tr _ (Tr2 tˡ tʳ)) f = iterTr tʳ f ∘ iterTr tˡ f
iterTr (Tr _ (Tr3 tˡ tᵐ tʳ)) f = iterTr tʳ f ∘ iterTr tᵐ f ∘ iterTr tˡ f

instance ToIter a (Tr h o a) where iter xs = 𝐼 $ iterTr xs

-- Stream

data TrK𝑆 o a where
  TopTr𝑆 ∷ ∀ o a. TrK𝑆 o a
  InTr2L𝑆 ∷ ∀ h o a. () → Tr h o a → TrK𝑆 o a → TrK𝑆 o a
  InTr3L𝑆 ∷ ∀ h o a. () → Tr h o a → Tr h o a → TrK𝑆 o a → TrK𝑆 o a

locFstTr𝑆 ∷ Tr h o a → TrK𝑆 o a → a ∧ TrK𝑆 o a
locFstTr𝑆 (Tr _ (Tr0 x)) c = x :* c
locFstTr𝑆 (Tr _ (Tr2 tˡ tʳ)) c = locFstTr𝑆 tˡ $ InTr2L𝑆 () tʳ c
locFstTr𝑆 (Tr _ (Tr3 tˡ tᵐ tʳ)) c = locFstTr𝑆 tˡ $ InTr3L𝑆 () tᵐ tʳ c

nextTr𝑆 ∷ TrK𝑆 o a → 𝑂 (a ∧ TrK𝑆 o a)
nextTr𝑆 TopTr𝑆 = None
nextTr𝑆 (InTr2L𝑆 () tʳ c) =
  let x :* c' = locFstTr𝑆 tʳ c
  in Some (x :* c')
nextTr𝑆 (InTr3L𝑆 () tᵐ tʳ c) =
  let x :* c' = locFstTr𝑆 tᵐ $ InTr2L𝑆 () tʳ c
  in Some (x :* c')

streamTr ∷ Tr h o a → 𝑆 a
streamTr t = 𝑆 (Some $ locFstTr𝑆 t TopTr𝑆) $ \case
  None → None
  Some (x :* c) → Some (x :* nextTr𝑆 c)

instance ToStream a (Tr h o a) where stream = streamTr

