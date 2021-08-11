module UVMHS.Lib.Sep where

import UVMHS.Core

data Sep i a = 
    SepE a
  | SepN a i (𝐼C (a ∧ i)) a

sepI ∷ (Null a) ⇒ i → Sep i a
sepI i = SepN null i null null

instance (Null a) ⇒ Null (Sep i a) where null = SepE null
instance (Append a) ⇒ Append (Sep i a) where
  SepE x₁ ⧺ SepE x₂ = SepE $ x₁ ⧺ x₂
  SepE x₁ ⧺ SepN x₂₁ i₂ xis₂ x₂₂ = SepN (x₁ ⧺ x₂₁) i₂ xis₂ x₂₂
  SepN x₁₁ i₁ xis₁ x₁₂ ⧺ SepE x₂ = SepN x₁₁ i₁ xis₁ $ x₁₂ ⧺ x₂
  SepN x₁₁ i₁ xis₁ x₁₂ ⧺ SepN x₂₁ i₂ xis₂ x₂₂ = 
    let xis' = xis₁ ⧺ single ((x₁₂ ⧺ x₂₁) :* i₂) ⧺ xis₂
    in SepN x₁₁ i₁ xis' x₂₂
instance (Monoid a) ⇒ Monoid (Sep i a)

instance ToIter a (Sep a a) where
  iter = \case
    SepE x → single x
    SepN x₁ i xis x₂ → concat
      [ single x₁
      , single i
      , do x' :* i' ← iter xis  
           iter [x',i']
      , single x₂
      ]

instance Functor (Sep i) where map = mapSep id

instance CSized (Sep i a) where
  csize = \case
    SepE _ → zero
    SepN _ _ xis _ → one + csize xis

mapSep ∷ (i → j) → (a → b) → Sep i a → Sep j b
mapSep f g = \case
  SepE x → SepE $ g x
  SepN x₁ i xis x₂ → SepN (g x₁) (f i) (map (mapPair g f) xis) $ g x₂
  
mapSepI ∷ (i → j) → Sep i a → Sep j a
mapSepI f = mapSep f id

