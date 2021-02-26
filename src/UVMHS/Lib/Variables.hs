module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty

---------------
-- VARIABLES --
---------------

data 𝕏 = 𝕏
  { 𝕩name ∷ 𝕊
  , 𝕩Gen ∷ 𝑂 ℕ64
  } deriving (Eq,Ord,Show)

var ∷ 𝕊 → 𝕏
var x = 𝕏 x None

makeLenses ''𝕏

instance Pretty 𝕏 where
  pretty (𝕏 x nO) = concat
    [ ppString x
    , case nO of
        None → null
        Some n → concat [ppPun "#",ppPun $ show𝕊 n]
    ]

-------------------
-- VARIABLE SETS --
-------------------

data 𝔛 = 𝔛
  { 𝔵lexicals ∷ 𝑃 𝕏
  , 𝔵metas ∷ 𝑃 𝕏
  }
  deriving 
  (Eq,Ord,Show
  )

instance Pretty 𝔛 where
  pretty (𝔛 𝓍ˡ 𝓍ᵐ) 
    | 𝓍ˡ ≡ null , 𝓍ᵐ ≡ null = ppLit "∅"
    | 𝓍ˡ ≡ null = ppApp (ppString "meta") $ single𝐼 $ pretty 𝓍ᵐ
    | 𝓍ᵐ ≡ null = ppApp (ppString "lexi") $ single𝐼 $ pretty 𝓍ˡ
    | otherwise = ppRecord  (ppPun "↦")
        [ ppString "lexi" :* pretty 𝓍ˡ
        , ppString "meta" :* pretty 𝓍ᵐ
        ]

instance Bot 𝔛 where bot = 𝔛 bot bot
instance Join 𝔛 where 𝔛 𝓍ˢ₁ 𝓍ᵐ₁ ⊔ 𝔛 𝓍ˢ₂ 𝓍ᵐ₂ = 𝔛 (𝓍ˢ₁ ⊔ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊔ 𝓍ᵐ₂
instance Meet 𝔛 where 𝔛 𝓍ˢ₁ 𝓍ᵐ₁ ⊓ 𝔛 𝓍ˢ₂ 𝓍ᵐ₂ = 𝔛 (𝓍ˢ₁ ⊓ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊓ 𝓍ᵐ₂
instance Difference 𝔛 where 𝔛 𝓍ˢ₁ 𝓍ᵐ₁ ⊟ 𝔛 𝓍ˢ₂ 𝓍ᵐ₂ = 𝔛 (𝓍ˢ₁ ⊟ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊟ 𝓍ᵐ₂

instance JoinLattice 𝔛

𝔵lexical ∷ 𝑃 𝕏 → 𝔛
𝔵lexical 𝓍 = 𝔛 𝓍 bot

𝔵meta ∷ 𝑃 𝕏 → 𝔛
𝔵meta 𝓍 = 𝔛 bot 𝓍

-------------------
-- SUBSTITUTIONS --
-------------------

data 𝔖 a = 𝔖
  { 𝔰lexicals ∷ 𝕏 ⇰ a
  , 𝔰metas ∷ 𝕏 ⇰ a
  } deriving (Eq,Ord,Show)

instance (Eq a,Pretty a) ⇒ Pretty (𝔖 a) where
  pretty (𝔖 𝓈ˡ 𝓈ᵐ)
    | 𝓈ˡ ≡ null , 𝓈ᵐ ≡ null = ppLit "∅"
    | 𝓈ˡ ≡ null = ppApp (ppString "meta") $ single𝐼 $ pretty 𝓈ᵐ
    | 𝓈ᵐ ≡ null = ppApp (ppString "lexi") $ single𝐼 $ pretty 𝓈ˡ
    | otherwise = ppRecord  (ppPun "↦")
        [ ppString "lexi" :* pretty 𝓈ˡ
        , ppString "meta" :* pretty 𝓈ᵐ
        ]

𝔰lexical ∷ 𝕏 ⇰ a → 𝔖 a
𝔰lexical 𝓈ˡ = 𝔖 𝓈ˡ null

𝔰meta ∷ 𝕏 ⇰ a → 𝔖 a
𝔰meta 𝓈ᵐ = 𝔖 null 𝓈ᵐ 

instance Null (𝔖 a) where null = 𝔖 null null
instance Append (𝔖 a) where 𝔖 𝓈ˡ₁ 𝓈ᵐ₁ ⧺ 𝔖 𝓈ˡ₂ 𝓈ᵐ₂ = 𝔖 (𝓈ˡ₁ ⩌ 𝓈ˡ₂) $ 𝓈ᵐ₁ ⩌ 𝓈ᵐ₂
instance Monoid (𝔖 a)

instance ToStream (𝕏 ∧ a) (𝔖 a) where stream (𝔖 𝓈ˡ 𝓈ᵐ) = stream 𝓈ˡ ⧺ stream 𝓈ᵐ

𝔰restrict ∷ 𝔛 → 𝔖 a → 𝔖 a
𝔰restrict (𝔛 𝓍ˡ 𝓍ᵐ) (𝔖 𝓈ˡ 𝓈ᵐ) = 𝔖 (restrict 𝓍ˡ 𝓈ˡ) $ restrict 𝓍ᵐ 𝓈ᵐ

𝔰without ∷ 𝔛 → 𝔖 a → 𝔖 a
𝔰without (𝔛 𝓍ˡ 𝓍ᵐ) (𝔖 𝓈ˡ 𝓈ᵐ) = 𝔖 (without 𝓍ˡ 𝓈ˡ) $ without 𝓍ᵐ 𝓈ᵐ

𝔰support ∷ (Eq a) ⇒ 𝔖 a → 𝔛
𝔰support (𝔖 𝓈ˡ 𝓈ᵐ) = 𝔛 (keys 𝓈ˡ) $ keys 𝓈ᵐ

𝔰values ∷ 𝔖 a → 𝐿 a
𝔰values (𝔖 𝓈ˡ 𝓈ᵐ) = list $ iter (values 𝓈ˡ) ⧺ iter (values 𝓈ᵐ)

substVarLexical ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → 𝔖 a → 𝕏 → m b
substVarLexical mkvar 𝒸 𝓈 x = case 𝔰lexicals 𝓈 ⋕? x of
  None → return $ mkvar x
  Some e → 𝒸 e

substVarMeta ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → 𝔖 a → 𝕏 → m b
substVarMeta mkvar 𝒸 𝓈 x = case 𝔰metas 𝓈 ⋕? x of
  None → return $ mkvar x
  Some e → 𝒸 e

instance FunctorM 𝔖 where
  mapM ∷ (Monad m) ⇒ (a → m b) → 𝔖 a → m (𝔖 b)
  mapM f (𝔖 𝓈ˡ 𝓈ᵐ) = do
    𝓈ˡ' ← dict ^$ mapMOn (iter 𝓈ˡ) $ \ (x :* a) → do
      b ← f a
      return $ x ↦ b
    𝓈ᵐ' ← dict ^$ mapMOn (iter 𝓈ᵐ) $ \ (χ :* a) → do
      b ← f a
      return $ χ ↦ b
    return $ 𝔖 𝓈ˡ' 𝓈ᵐ'
