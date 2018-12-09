-- replaced by Lib.Parser.Sep???
module Core.Lines where

import Init
import Core.Classes
import Core.Data

data LinesPre a = LinesPre
  { linesHead ∷ a
  , linesTails ∷ 𝑄 (a ∧ a)
  } deriving (Eq,Ord)
instance (Null a) ⇒ Null (LinesPre a) where null = LinesPre null null
instance (Append a) ⇒ Append (LinesPre a) where
  LinesPre x₁ sxs₁ ⧺ LinesPre x₂ sxs₂ = case unsnoc𝑄 sxs₁ of
    None → LinesPre (x₁ ⧺ x₂) sxs₂
    Some (sxs₁' :꘍ (s₁ :꘍ x₁')) → LinesPre x₁ (sxs₁' ⧺ single (s₁ :꘍ x₁' ⧺ x₂) ⧺ sxs₂)
instance (Monoid a) ⇒ Monoid (LinesPre a)
instance ToStream a (LinesPre a) where 
  stream (LinesPre x₀ sxs₀) = case stream sxs₀ of
    𝑆 s₀ g → 𝑆 (Inl (x₀ :꘍ s₀)) $ \case
      Inl (x :꘍ s) → Some (x :꘍ Inr s)
      Inr s → do
        ((x₁ :꘍ x₂) :꘍ s') ← g s
        Some (x₁ :꘍ Inl (x₂ :꘍ s'))
instance ToIter a (LinesPre a) where iter = iter ∘ stream
instance Functor LinesPre where map f (LinesPre x sxs) = LinesPre (f x) $ map (mapPair f f) sxs

linesPreChunk ∷ a → LinesPre a
linesPreChunk x = LinesPre x null

linesPreSep ∷ (Null a) ⇒ a → LinesPre a
linesPreSep s = LinesPre null $ single (s :꘍ null)

firstNLinesPre ∷ ℕ → LinesPre a → LinesPre a
firstNLinesPre n (LinesPre x sxs) = LinesPre x $ seq $ firstN n $ stream sxs

data LinesPost a = LinesPost
  { linesHeads ∷ 𝑄 (a ∧ a)
  , linesTail ∷ a
  } deriving (Eq,Ord)
instance (Null a) ⇒ Null (LinesPost a) where null = LinesPost null null
instance (Append a) ⇒ Append (LinesPost a) where
  LinesPost xss₁ x₁ ⧺ LinesPost xss₂ x₂ = case uncons𝑄 xss₂ of
    None → LinesPost xss₁ (x₁ ⧺ x₂)
    Some ((x₂' :꘍ s₂) :꘍ xss₂') → LinesPost (xss₁ ⧺ single (x₁ ⧺ x₂' :꘍ s₂) ⧺ xss₂') x₂
instance (Monoid a) ⇒ Monoid (LinesPost a)
instance ToStream a (LinesPost a) where
  stream (LinesPost xss₀ x₀) = case stream xss₀ of
    𝑆 s₀ g → 𝑆 (Some (Inl (s₀ :꘍ x₀))) $ \case
      Some (Inl (s :꘍ x)) → case g s of
        None → Some (x :꘍ None)
        Some ((x₁ :꘍ x₂) :꘍ s') → Some (x₁ :꘍ Some (Inr (x₂ :꘍ s' :꘍ x)))
      Some (Inr (x₁ :꘍ s :꘍ x₂)) → Some (x₁ :꘍ Some (Inl (s :꘍ x₂)))
      None → None
instance ToIter a (LinesPost a) where iter = iter ∘ stream
instance Functor LinesPost where map f (LinesPost xss x) = LinesPost (map (mapPair f f) xss) $ f x

linesPostChunk ∷ a → LinesPost a
linesPostChunk x = LinesPost null x

linesPostSep ∷ (Null a) ⇒ a → LinesPost a
linesPostSep s = LinesPost (single (null :꘍ s)) null

lastNLinesPost ∷ ℕ → LinesPost a → LinesPost a
lastNLinesPost n (LinesPost xss x) = LinesPost (seq $ firstN n $ list $ reverse $ list xss) x

-- Converting

linesPreFromPost ∷ LinesPost a → LinesPre a
linesPreFromPost (LinesPost xss₀ x₀) = let (x₀' :꘍ sxs₀') = loop xss₀ x₀ in LinesPre x₀' sxs₀'
  where
    loop ∷ 𝑄 (a ∧ a) → a → (a ∧ 𝑄 (a ∧ a))
    loop xss x = case unsnoc𝑄 xss of
      None → (x :꘍ null)
      Some (xss' :꘍ (x' :꘍ s)) → 
        let (y :꘍ sys) = loop xss' x'
        in (y :꘍ snoc𝑄 sys (s :꘍ x))

linesPostFromPre ∷ LinesPre a → LinesPost a
linesPostFromPre (LinesPre x₀ sxs₀) = let (xss₀' :꘍ x₀') = loop x₀ sxs₀ in LinesPost xss₀' x₀'
  where
    loop ∷ a → 𝑄 (a ∧ a) → 𝑄 (a ∧ a) ∧ a
    loop x sxs = case uncons𝑄 sxs of
      None → (null :꘍ x)
      Some ((s :꘍ x') :꘍ sxs') →
        let (yss :꘍ y) = loop x' sxs'
        in (cons𝑄 (x :꘍ s) yss :꘍ y)

