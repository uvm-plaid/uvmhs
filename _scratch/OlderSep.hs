module UVMHS.Lib.Parser.Sep where

import UVMHS.Core
import UVMHS.Lib.Pretty

-- data Sep i a =
--     SepE a
--   | SepS a i (𝐼 (a ∧ i)) a
--
-- sepI ∷ (Null a) ⇒ i → Sep i a
-- sepI i = SepS null i null null
--
-- instance (Null a) ⇒ Null (Sep i a) where null = Sep null
-- instance (Append a) ⇒ Append (Sep i a) where
--   Sep x₁ ⧺ Sep x₂ = Sep $ x₁ ⧺ x₂
--   Sep x₁ ⧺ Sep x₂₁ i₂ xis₂ x₂₂ = Sep (x₁ ⧺ x₂₁) i₂ xis₂ x₂₂
--   Sep x₁₁ i₁ xis₁ x₁₂ ⧺ Sep x₂ = Sep x₁₁ i₁ xis₁ $ x₁₂ ⧺ x₂
--   Sep x₁₁ i₁ xis₁ x₁₂ ⧺ Sep x₂₁ i₂ xis₂ x₂₂ =
--     let xis' = xis₁ ⧺ single ((x₁₁ ⧺ x₂₁) :* i₂) ⧺ xis₂
--     in Sep x₁₁ i₁ xis' x₂₂
-- instance (Monoid a) ⇒ Monoid (Sep i a)

data SepL i a = SepL
  { sepLHead ∷ a
  , sepLTail ∷ 𝑄 (i ∧ a)
  } deriving (Eq,Ord,Show)
instance (Null a) ⇒ Null (SepL i a) where null = SepL null null
instance (Append a) ⇒ Append (SepL i a) where
  SepL x₁ sxs₁ ⧺ SepL x₂ sxs₂ = case unsnoc𝑄 sxs₁ of
    None → SepL (x₁ ⧺ x₂) sxs₂
    Some (sxs₁' :* (s₁ :* x₁')) → SepL x₁ (sxs₁' ⧺ single (s₁ :* (x₁' ⧺ x₂)) ⧺ sxs₂)
instance (Monoid a) ⇒ Monoid (SepL i a)
instance ToStream a (SepL a a) where
  stream (SepL x₀ sxs₀) = concat
    [ single x₀
    , concat $ mapOn sxs₀ $ \ (x :* y) → stream [x,y]
    ]
instance ToIter a (SepL a a) where
  iter (SepL x₀ sxs₀) = concat
    [ single x₀
    , concat $ mapOn sxs₀ $ \ (x :* y) → iter [x,y]
    ]
instance Functor (SepL i) where map = mapSepL id

mapSepL ∷ (i₁ → i₂) → (a₁ → a₂) → SepL i₁ a₁ → SepL i₂ a₂
mapSepL fⁱ fᵃ (SepL x sxs) = SepL (fᵃ x) $ map (mapPair fⁱ fᵃ) sxs

eSepL ∷ a → SepL i a
eSepL x = SepL x null

iSepL ∷ (Null a) ⇒ i → SepL i a
iSepL s = SepL null $ single (s :* null)

firstNSepL ∷ ℕ64 → SepL i a → SepL i a
firstNSepL n (SepL x sxs) = SepL x $ seq $ firstN (nat n) sxs

data SepR i a = SepR
  { sepRHead ∷ 𝑄 (a ∧ i)
  , sepRTail ∷ a
  } deriving (Eq,Ord,Show)
instance (Null a) ⇒ Null (SepR i a) where null = SepR null null
instance (Append a) ⇒ Append (SepR i a) where
  SepR xss₁ x₁ ⧺ SepR xss₂ x₂ = case uncons𝑄 xss₂ of
    None → SepR xss₁ (x₁ ⧺ x₂)
    Some ((x₂' :* s₂) :* xss₂') → SepR (xss₁ ⧺ single ((x₁ ⧺ x₂') :* s₂) ⧺ xss₂') x₂
instance (Monoid a) ⇒ Monoid (SepR i a)

instance ToStream a (SepR a a) where
  stream (SepR xss₀ x₀) =
    mjoin
    $ flip (⧺) (single (single x₀))
    $ map (\ (x :* y) → stream [x,y])
    $ stream xss₀
instance ToIter a (SepR a a) where
  iter (SepR xss₀ x₀) =
    mjoin
    $ flip (⧺) (single (single x₀))
    $ map (\ (x :* y) → iter [x,y])
    $ iter xss₀
instance Functor (SepR i) where map f (SepR xss x) = SepR (map (mapFst f) xss) $ f x
instance (Pretty a) ⇒ Pretty (SepR a a) where pretty = pretty ∘ stream

mapSepR ∷ (i₁ → i₂) → (a₁ → a₂) → SepR i₁ a₁ → SepR i₂ a₂
mapSepR fⁱ fᵃ (SepR xss x) = SepR (map (mapPair fᵃ fⁱ) xss) $ fᵃ x

eSepR ∷ a → SepR i a
eSepR x = SepR null x

iSepR ∷ (Null a) ⇒ i → SepR i a
iSepR s = SepR (single (null :* s)) null

lastNSepR ∷ ℕ64 → SepR i a → SepR i a
lastNSepR n (SepR xss x) = SepR (seq $ lastN (nat n) xss) x

-- Converting

sepRL ∷ SepR i a → SepL i a
sepRL (SepR xss₀ x₀) = let (x₀' :* sxs₀') = loop xss₀ x₀ in SepL x₀' sxs₀'
  where
    loop ∷ 𝑄 (a ∧ i) → a → (a ∧ 𝑄 (i ∧ a))
    loop xss x = case unsnoc𝑄 xss of
      None → (x :* null)
      Some (xss' :* (x' :* s)) →
        let (y :* sys) = loop xss' x'
        in (y :* snoc𝑄 sys (s :* x))

sepLR ∷ SepL i a → SepR i a
sepLR (SepL x₀ sxs₀) = let (xss₀' :* x₀') = loop x₀ sxs₀ in SepR xss₀' x₀'
  where
    loop ∷ a → 𝑄 (i ∧ a) → 𝑄 (a ∧ i) ∧ a
    loop x sxs = case uncons𝑄 sxs of
      None → (null :* x)
      Some ((s :* x') :* sxs') →
        let (yss :* y) = loop x' sxs'
        in (cons𝑄 (x :* s) yss :* y)
