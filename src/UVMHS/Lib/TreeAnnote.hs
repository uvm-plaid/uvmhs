module UVMHS.Lib.TreeAnnote where

import UVMHS.Core

import qualified Prelude as HS

-- This file contains two datastructures for annotated trees. The first is
-- straightforward to understand based on its datatype definition. The second
-- “virtual” one never constructs the full tree, and is equivalent to the first
-- “vanilla” one.

class Annote i a | a → i where
  annote ∷ i → a → a

-------------
-- VANILLA --
-------------

data 𝑇 i a =
    N𝑇
  | B𝑇 (𝑇 i a) (𝑇 i a)
  | L𝑇 a
  | A𝑇 i (𝑇 i a)
  deriving (Eq,Ord,Show)

fold𝑇With ∷ (Monoid b) ⇒ (a → b) → (i → b → b) → 𝑇 i a → b
fold𝑇With fₗ fₐ = loop
  where
    loop = \case
      N𝑇 → null
      B𝑇 xs ys → loop xs ⧺ loop ys
      L𝑇 x → fₗ x
      A𝑇 i xs → fₐ i $ loop xs

fold𝑇On ∷ (Monoid b) ⇒ 𝑇 i a → (a → b) → (i → b → b) → b
fold𝑇On = rotateR fold𝑇With

instance Null (𝑇 i a) where null = N𝑇
instance Append (𝑇 i a) where (⧺) = B𝑇
instance Monoid (𝑇 i a)

instance Single a (𝑇 i a) where single = L𝑇
instance Annote i (𝑇 i a) where annote = A𝑇

instance Functor (𝑇 i) where map f = fold𝑇With (L𝑇 ∘ f) annote

instance ToIter a (𝑇 i a) where
  iter t₀ = 𝐼 HS.$ \ yield →
    let loop t i continue = case t of
          N𝑇 → continue i
          B𝑇 t₁ t₂ →
            loop t₁ i $ \ i' →
            loop t₂ i' continue
          L𝑇 x → yield x i continue
          A𝑇 _ t' → loop t' i continue
    in loop t₀

-------------
-- VIRTUAL --
-------------

data 𝑇V i a = 𝑇V
  { un𝑇V ∷ ∀ b. (Monoid b)
              ⇒ (a → b)
              → (i → b → b)
              → b
  }

fold𝑇VOn ∷ (Monoid b) ⇒ 𝑇V i a → (a → b) → (i → b → b) → b
fold𝑇VOn xs = un𝑇V xs

fold𝑇VWith ∷ (Monoid b) ⇒ (a → b) → (i → b → b) → 𝑇V i a → b
fold𝑇VWith = rotateL fold𝑇VOn

null𝑇V ∷ 𝑇V i a
null𝑇V = 𝑇V HS.$ \ _fₑ _fₐ → null

append𝑇V ∷ 𝑇V i a → 𝑇V i a → 𝑇V i a
append𝑇V (𝑇V g₁) (𝑇V g₂) = 𝑇V HS.$ \ fₑ fₐ →
  g₁ fₑ fₐ ⧺ g₂ fₑ fₐ

single𝑇V ∷ a → 𝑇V i a
single𝑇V e = 𝑇V HS.$ \ fₑ _fₐ → fₑ e

annote𝑇V ∷ i → 𝑇V i a → 𝑇V i a
annote𝑇V i (𝑇V g) = 𝑇V HS.$ \ fₑ fₐ → fₐ i $ g fₑ fₐ

map𝑇V ∷ (i → j) → (a → b) → 𝑇V i a → 𝑇V j b
map𝑇V f g (𝑇V h) = 𝑇V HS.$ \ fₑ fₐ → h (fₑ ∘ g) $ fₐ ∘ f

realize𝑇 ∷ 𝑇V i a → 𝑇 i a
realize𝑇 = fold𝑇VWith L𝑇 A𝑇

virtualize𝑇 ∷ 𝑇 i a → 𝑇V i a
virtualize𝑇 t₀ = 𝑇V HS.$ \ leaf anno → 
  let loop = \case
        N𝑇 → null
        B𝑇 t₁ t₂ → loop t₁ ⧺ loop t₂
        L𝑇 x → leaf x
        A𝑇 i t → anno i $ loop t
  in loop t₀

instance Null (𝑇V i a) where null = null𝑇V
instance Append (𝑇V i a) where (⧺) = append𝑇V
instance Monoid (𝑇V i a)

instance Single a (𝑇V i a) where single = single𝑇V
instance Annote i (𝑇V i a) where annote = annote𝑇V

instance Functor (𝑇V i) where map = map𝑇V id

instance (Eq a,Eq i) ⇒ Eq (𝑇V i a) where (==) = (≡) `on` realize𝑇
instance (Ord a,Ord i) ⇒ Ord (𝑇V i a) where compare = (⋚) `on` realize𝑇
instance (Show a,Show i) ⇒ Show (𝑇V i a) where show = show ∘ realize𝑇

instance ToIter a (𝑇V i a) where iter = iter ∘ realize𝑇
