module UVMHS.Lib.Parser.NewSep where

import UVMHS.Core
import UVMHS.Lib.NewPretty

------------
-- Swivel --
------------

swivelL ∷ a → 𝐿 a → 𝐿 a ∧ a
swivelL x Nil = Nil :* x
swivelL x (y :& xs) =
  let xs' :* x' = swivelL y xs
  in (x :& xs') :* x'

swivelR ∷ 𝐿 a → a → a ∧ 𝐿 a
swivelR Nil x = x :* Nil
swivelR (x :& xs) y =
  let x' :* xs' = swivelR xs y
  in x :* (x' :& xs')

----------
-- SepL --
----------

data SepL i a =
    SepLSingle a
  | SepLMulti ℕ64 a (𝐼 (i ∧ a)) i a
  deriving (Show)

-- {-# INLINE eSepL #-}
eSepL ∷ a → SepL i a
eSepL x = SepLSingle x

-- {-# INLINE iSepL #-}
iSepL ∷ (Null a) ⇒ i → SepL i a
iSepL i = SepLMulti one null null i null

instance (Null a) ⇒ Null (SepL i a) where
  -- {-# INLINE null #-}
  null = SepLSingle null
instance (Append a) ⇒ Append (SepL i a) where
  -- {-# INLINE (⧺) #-}
  SepLSingle x₁ ⧺ SepLSingle x₂ = SepLSingle $ x₁ ⧺ x₂
  SepLSingle x₁ ⧺ SepLMulti n x₂ ixs₂ i₂ y₂ = SepLMulti n (x₁ ⧺ x₂) ixs₂ i₂ y₂
  SepLMulti n x₁ ixs₁ i₁ y₁ ⧺ SepLSingle x₂ = SepLMulti n x₁ ixs₁ i₁ (y₁ ⧺ x₂)
  SepLMulti n₁ x₁ ixs₁ i₁ y₁ ⧺ SepLMulti n₂ x₂ ixs₂ i₂ y₂ =
    SepLMulti (n₁ + n₂) x₁ (ixs₁ ⧺ single (i₁ :* (y₁ ⧺ x₂)) ⧺ ixs₂) i₂ y₂
instance (Monoid a) ⇒ Monoid (SepL i a)

instance ToIter a (SepL a a) where
  -- {-# INLINE iter #-}
  iter (SepLSingle x) = single x
  iter (SepLMulti _ x ixs i y) = concat
    [ single x
    , concat $ mapOn ixs $ \ (i' :* x') → iter [i',x']
    , iter [i,y]
    ]

instance (Pretty a) ⇒ Pretty (SepL a a) where
  -- {-# INLINE pretty #-}
  pretty = concat ∘ map pretty ∘ iter

firstNSepL ∷ ℕ64 → SepL i a → SepL i a
firstNSepL _ (SepLSingle x) = SepLSingle x
firstNSepL n (SepLMulti n' x ixs i y)
  | n ≡ zero = SepLSingle x
  | n' ≤ n = SepLMulti n' x ixs i y
  | otherwise =
      let iy' :* ixs' = swivelR (list ixs) (i :* y)
          ixs'' = list $ firstN (nat $ n - one) ixs'
          ixs''' :* (i''' :* y''') = swivelL iy' ixs''
      in SepLMulti n x (iter ixs''') i''' y'''

mapSepL ∷ (i → j) → (a → b) → SepL i a → SepL j b
mapSepL _ f (SepLSingle x) = SepLSingle $ f x
mapSepL g f (SepLMulti n x ixs i y) = SepLMulti n (f x) (map (\ (i' :* x') → g i' :* f x') ixs) (g i) (f y)

sepsCountL ∷ SepL i a → ℕ64
sepsCountL (SepLSingle _) = zero
sepsCountL (SepLMulti n _ _ _ _) = n

----------
-- SepR --
----------

data SepR i a =
    SepRSingle a
  | SepRMulti ℕ64 a i (𝐼 (a ∧ i)) a
  deriving (Show)

-- {-# INLINE eSepR #-}
eSepR ∷ a → SepR i a
eSepR x = SepRSingle x

-- {-# INLINE iSepR #-}
iSepR ∷ (Null a) ⇒ i → SepR i a
iSepR i = SepRMulti one null i null null

instance (Null a) ⇒ Null (SepR i a) where
  -- {-# INLINE null #-}
  null = SepRSingle null
instance (Append a) ⇒ Append (SepR i a) where
  -- {-# INLINE (⧺) #-}
  SepRSingle x₁ ⧺ SepRSingle x₂ = SepRSingle $ x₁ ⧺ x₂
  SepRSingle x₁ ⧺ SepRMulti n x₂ i₂ xis₂ y₂ = SepRMulti n (x₁ ⧺ x₂) i₂ xis₂ y₂
  SepRMulti n x₁ i₁ xis₁ y₁ ⧺ SepRSingle x₂ = SepRMulti n x₁ i₁ xis₁ (y₁ ⧺ x₂)
  SepRMulti n₁ x₁ i₁ xis₁ y₁ ⧺ SepRMulti n₂ x₂ i₂ xis₂ y₂ =
    SepRMulti (n₁ + n₂) x₁ i₁ (xis₁ ⧺ single ((y₁ ⧺ x₂) :* i₂) ⧺ xis₂) y₂
instance (Monoid a) ⇒ Monoid (SepR i a)

instance ToIter a (SepR a a) where
  -- {-# INLINE iter #-}
  iter (SepRSingle x) = single x
  iter (SepRMulti _ x i xis y) = concat
    [ iter [x,i]
    , concat $ mapOn xis $ \ (x' :* i') → iter [x',i']
    , single y
    ]

instance (Pretty a) ⇒ Pretty (SepR a a) where
  -- {-# INLINE pretty #-}
  pretty = concat ∘ map pretty ∘ iter

lastNSepR ∷ ℕ64 → SepR i a → SepR i a
lastNSepR _ (SepRSingle x) = SepRSingle x
lastNSepR n (SepRMulti n' x i xis y)
  | n ≡ zero = SepRSingle y
  | n' ≤ n = SepRMulti n' x i xis y
  | otherwise =
      let xis' :* xi' = swivelL (x :* i) (list xis)
          xis'' = list $ lastN (nat $ n - one) xis'
          (x''' :* i''') :* xis''' = swivelR xis'' xi'
      in SepRMulti n x''' i''' (iter xis''') y

mapSepR ∷ (i → j) → (a → b) → SepR i a → SepR j b
mapSepR _ f (SepRSingle x) = SepRSingle $ f x
mapSepR g f (SepRMulti n x i xis y) = SepRMulti n (f x) (g i) (map (\ (x' :* i') → f x' :* g i') xis) (f y)

sepsCountR ∷ SepR i a → ℕ64
sepsCountR (SepRSingle _) = zero
sepsCountR (SepRMulti n _ _ _ _) = n
