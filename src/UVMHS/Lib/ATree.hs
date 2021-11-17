module UVMHS.Lib.ATree where

import UVMHS.Core

import qualified Prelude as HS

data 𝑇V i a = 𝑇V 
  { un𝑇V ∷ ∀ b. (Monoid b) 
              ⇒ (a → b) 
              → (i → b → b) 
              → b 
  }

fold𝑇VOnLeafNode ∷ (Monoid b) ⇒ 𝑇V i a → (a → b) → (i → b → b) → b
fold𝑇VOnLeafNode = un𝑇V

fold𝑇VLeafNodeOn ∷ (Monoid b) ⇒ (a → b) → (i → b → b) → 𝑇V i a → b
fold𝑇VLeafNodeOn fₗ fₙ xs = un𝑇V xs fₗ fₙ

null𝑇V ∷ 𝑇V i a
null𝑇V = 𝑇V HS.$ \ _fₑ _fₐ → null

append𝑇V ∷ 𝑇V i a → 𝑇V i a → 𝑇V i a
append𝑇V (𝑇V g₁) (𝑇V g₂) = 𝑇V HS.$ \ fₑ fₐ →
  g₁ fₑ fₐ ⧺ g₂ fₑ fₐ

single𝑇V ∷ a → 𝑇V i a
single𝑇V e = 𝑇V HS.$ \ fₑ _fₐ → fₑ e

annot ∷ i → 𝑇V i a → 𝑇V i a
annot i (𝑇V g) = 𝑇V HS.$ \ fₑ fₐ → fₐ i $ g fₑ fₐ


instance Null (𝑇V i a) where null = null𝑇V
instance Append (𝑇V i a) where (⧺) = append𝑇V
instance Monoid (𝑇V i a)
instance Single a (𝑇V i a) where single = single𝑇V

map𝑇V ∷ (i → j) → (a → b) → 𝑇V i a → 𝑇V j b
map𝑇V f g (𝑇V h) = 𝑇V HS.$ \ fₑ fₐ → h (fₑ ∘ g) $ fₐ ∘ f

instance Functor (𝑇V i) where map = map𝑇V id

-- -------
-- -- 𝐴 --
-- -------
-- 
-- data 𝐴 o i j a =
--     Leaf𝐴 o i j a
--   | Append𝐴 o i j (𝐴 o i j a) (𝐼 (𝐴 o i j a)) (𝐴 o i j a)
--   deriving (Show)
-- 
-- instance (Null o,Null i,Null j,Null a) ⇒ Null (𝐴 o i j a) where 
--   null = Leaf𝐴 null null null null
-- instance (Append o,Eq i,Null i,Append i,Eq j,Null j,Append j,Append a) ⇒ Append (𝐴 o i j a) where
--   t₁@(Leaf𝐴 o₁ i₁ j₁ x₁) ⧺ t₂@(Leaf𝐴 o₂ i₂ j₂ x₂)
--     | (i₁ ≡ i₂) ⩓ (j₁ ≡ null) ⩓ (j₂ ≡ null) = Leaf𝐴 (o₁ ⧺ o₂) i₁ j₁ (x₁ ⧺ x₂)
--     | otherwise = Append𝐴 (o₁ ⧺ o₂) null null t₁ null t₂
--   t₁@(Leaf𝐴 o₁ i₁ j₁ _) ⧺ t₂@(Append𝐴 o₂ i₂ j₂ t₂₁ ts₂₂ t₂₃)
--     | (i₁ ≡ i₂) ⩓ (j₁ ≡ null) ⩓ (j₂ ≡ null) = 
--         let t' :* ts' = case t₁ ⧺ t₂₁ of
--               Append𝐴 _ i₃ j₃ t₃₁ ts₃₂ t₃₃ | ((i₁ ⧺ i₃) ≡ i₁) ⩓ (j₃ ≡ null) → t₃₁ :* (ts₃₂ ⧺ single t₃₃ ⧺ ts₂₂)
--               t₃ → t₃ :* ts₂₂
--         in Append𝐴 (o₁ ⧺ o₂) i₁ j₁ t' ts' t₂₃
--     | otherwise = Append𝐴 (o₁ ⧺ o₂) null null t₁ null t₂
--   t₁@(Append𝐴 o₁ i₁ j₁ t₁₁ ts₁₂ t₁₃) ⧺ t₂@(Leaf𝐴 o₂ i₂ j₂ _)
--     | (i₁ ≡ i₂) ⩓ (j₁ ≡ null) ⩓ (j₂ ≡ null) =
--         let ts' :* t' = case t₁₃ ⧺ t₂ of
--               Append𝐴 _ i₃ j₃ t₃₁ ts₃₂ t₃₃ | ((i₁ ⧺ i₃) ≡ i₁) ⩓ (j₃ ≡ null) → (ts₁₂ ⧺ single t₃₁ ⧺ ts₃₂) :* t₃₃
--               t₃ → ts₁₂ :* t₃
--         in Append𝐴 (o₁ ⧺ o₂) i₁ j₁ t₁₁ ts' t'
--     | otherwise = Append𝐴 (o₁ ⧺ o₂) null null t₁ null t₂
--   t₁@(Append𝐴 o₁ i₁ j₁ x₁₁ xs₁₂ xs₁₃) ⧺ t₂@(Append𝐴 o₂ i₂ j₂ x₂₁ xs₂₂ xs₂₃)
--     | (i₁ ≡ i₂) ⩓ (j₁ ≡ null) ⩓ (j₂ ≡ null)  = 
--         let xs₂' = case xs₁₃ ⧺ x₂₁ of
--               Append𝐴 _ i₃ j₃ x₃₁ xs₃₂ x₃₃ | ((i₁ ⧺ i₃) ≡ i₁) ⩓ (j₃ ≡ null) → single x₃₁ ⧺ xs₃₂ ⧺ single x₃₃
--               t₃ → single t₃
--         in Append𝐴 (o₁ ⧺ o₂) i₁ j₁ x₁₁ (xs₁₂ ⧺ xs₂' ⧺ xs₂₂) xs₂₃
--     | otherwise = Append𝐴 (o₁ ⧺ o₂) null null  t₁ null t₂
-- instance (Monoid o,Eq i,Monoid i,Eq j,Monoid j,Monoid a) ⇒ Monoid (𝐴 o i j a)
-- 
-- annoi ∷ (Append i) ⇒ i → 𝐴 o i j a → 𝐴 o i j a
-- annoi i (Leaf𝐴 o i' j x) = Leaf𝐴 o (i ⧺ i') j x
-- annoi i (Append𝐴 o i' j x₁ xs₂ x₃) = Append𝐴 o (i ⧺ i') j x₁ xs₂ x₃
-- 
-- annoj ∷ (Append j) ⇒ j → 𝐴 o i j a → 𝐴 o i j a
-- annoj j (Leaf𝐴 o i j' x) = Leaf𝐴 o i (j ⧺ j') x
-- annoj j (Append𝐴 o i j' x₁ xs₂ x₃) = Append𝐴 o i (j ⧺ j') x₁ xs₂ x₃
-- 
-- summary ∷ 𝐴 o i j a → o
-- summary (Leaf𝐴 o _ _ _) = o
-- summary (Append𝐴 o _ _ _ _ _) = o
-- 
-- mapSummary ∷ (o → o') → 𝐴 o i j a → 𝐴 o' i j a
-- mapSummary f (Leaf𝐴 o i j x) = Leaf𝐴 (f o) i j x
-- mapSummary f (Append𝐴 o i j x₁ xs₂ x₃) = 
--   Append𝐴 (f o) i j (mapSummary f x₁) (map (mapSummary f) xs₂) (mapSummary f x₃)
-- 
-- resummary ∷ (Monoid o) ⇒ (i → j → a → o) → 𝐴 o' i j a → 𝐴 o i j a
-- resummary f (Leaf𝐴 _ i j x) = Leaf𝐴 (f i j x) i j x
-- resummary f (Append𝐴 _ i j x₁ xs₂ x₃) = 
--   let x₁' = resummary f x₁
--       xs₂' = map (resummary f) xs₂
--       x₃' = resummary f x₃
--       o' = concat
--         [ summary x₁'
--         , concat $ map summary $ iter xs₂'
--         , summary x₃'
--         ] 
--   in Append𝐴 o' i j x₁' xs₂' x₃'
-- 
-- homMap𝐴 ∷ (o → o') → (a → b) → 𝐴 o i j a → 𝐴 o' i j b
-- homMap𝐴 f g (Leaf𝐴 o i j x) = Leaf𝐴 (f o) i j $ g x
-- homMap𝐴 f g (Append𝐴 o i j t₁ ts₂ t₃) = 
--   Append𝐴 (f o) i j (homMap𝐴 f g t₁) (map (homMap𝐴 f g) ts₂) (homMap𝐴 f g t₃)
-- 
-- instance Functor (𝐴 () i j) where map = homMap𝐴 id
-- 
