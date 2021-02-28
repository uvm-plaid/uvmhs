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

data FV = FV
  { fvLexis ∷ 𝑃 𝕏
  , fvMetas ∷ 𝑃 𝕏
  }
  deriving 
  (Eq,Ord,Show
  )

instance Pretty FV where
  pretty (FV 𝓍ˡ 𝓍ᵐ) 
    | 𝓍ˡ ≡ null , 𝓍ᵐ ≡ null = ppLit "∅"
    | 𝓍ˡ ≡ null = ppApp (ppPrim "meta") $ single𝐼 $ pretty 𝓍ᵐ
    | 𝓍ᵐ ≡ null = ppApp (ppPrim "lexi") $ single𝐼 $ pretty 𝓍ˡ
    | otherwise = ppRecord  (ppPun "↦")
        [ ppPrim "lexis" :* pretty 𝓍ˡ
        , ppPrim "metas" :* pretty 𝓍ᵐ
        ]

instance Bot FV where bot = FV bot bot
instance Join FV where FV 𝓍ˢ₁ 𝓍ᵐ₁ ⊔ FV 𝓍ˢ₂ 𝓍ᵐ₂ = FV (𝓍ˢ₁ ⊔ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊔ 𝓍ᵐ₂
instance Meet FV where FV 𝓍ˢ₁ 𝓍ᵐ₁ ⊓ FV 𝓍ˢ₂ 𝓍ᵐ₂ = FV (𝓍ˢ₁ ⊓ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊓ 𝓍ᵐ₂
instance Difference FV where FV 𝓍ˢ₁ 𝓍ᵐ₁ ⊟ FV 𝓍ˢ₂ 𝓍ᵐ₂ = FV (𝓍ˢ₁ ⊟ 𝓍ˢ₂) $ 𝓍ᵐ₁ ⊟ 𝓍ᵐ₂

instance JoinLattice FV

fvLexi ∷ 𝑃 𝕏 → FV
fvLexi 𝓍 = FV 𝓍 bot

fvMeta ∷ 𝑃 𝕏 → FV
fvMeta 𝓍 = FV bot 𝓍

class HasFV a where
  fv ∷ a → FV

-------------------
-- SUBSTITUTIONS --
-------------------

data Sub a = Sub
  { subLexis ∷ 𝕏 ⇰ a
  , subMetas ∷ 𝕏 ⇰ a
  , subFvars ∷ FV
  } deriving (Eq,Ord,Show)

mkSub ∷ (HasFV a) ⇒ 𝕏 ⇰ a → 𝕏 ⇰ a → Sub a
mkSub 𝓈ˡ 𝓈ᵐ =
  let 𝓍 = joins $ map fv (values 𝓈ˡ) ⧺ map fv (values 𝓈ᵐ)
  in Sub 𝓈ˡ 𝓈ᵐ 𝓍

instance (Eq a,Pretty a) ⇒ Pretty (Sub a) where
  pretty (Sub 𝓈ˡ 𝓈ᵐ 𝓍)
    | 𝓈ˡ ≡ null , 𝓈ᵐ ≡ null = ppLit "∅"
    | 𝓈ˡ ≡ null = ppApp (ppString "meta") $ [pretty 𝓈ᵐ,pretty 𝓍]
    | 𝓈ᵐ ≡ null = ppApp (ppString "lexi") $ [pretty 𝓈ˡ,pretty 𝓍]
    | otherwise = ppRecord  (ppPun "↦")
        [ ppString "lexis" :* pretty 𝓈ˡ
        , ppString "metas" :* pretty 𝓈ᵐ
        , ppString "fvars" :* pretty 𝓍
        ]

subLexical ∷ (HasFV a) ⇒ 𝕏 ⇰ a → Sub a
subLexical 𝓈ˡ = mkSub 𝓈ˡ null

subMeta ∷ (HasFV a) ⇒ 𝕏 ⇰ a → Sub a
subMeta 𝓈ᵐ = mkSub null 𝓈ᵐ 

instance Null (Sub a) where null = Sub null null bot
instance Append (Sub a) where Sub 𝓈ˡ₁ 𝓈ᵐ₁ 𝓍₁ ⧺ Sub 𝓈ˡ₂ 𝓈ᵐ₂ 𝓍₂ = Sub (𝓈ˡ₁ ⩌ 𝓈ˡ₂) (𝓈ᵐ₁ ⩌ 𝓈ᵐ₂) $ 𝓍₁ ⊔ 𝓍₂
instance Monoid (Sub a)

instance ToStream (𝕏 ∧ a) (Sub a) where stream (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = stream 𝓈ˡ ⧺ stream 𝓈ᵐ

subRestrict ∷ (HasFV a) ⇒ FV → Sub a → Sub a
subRestrict (FV 𝓍ˡ 𝓍ᵐ) (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = mkSub (restrict 𝓍ˡ 𝓈ˡ) $ restrict 𝓍ᵐ 𝓈ᵐ

subWithout ∷ (HasFV a) ⇒ FV → Sub a → Sub a
subWithout (FV 𝓍ˡ 𝓍ᵐ) (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = mkSub (without 𝓍ˡ 𝓈ˡ) $ without 𝓍ᵐ 𝓈ᵐ

subSupport ∷ (Eq a) ⇒ Sub a → FV
subSupport (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = FV (keys 𝓈ˡ) $ keys 𝓈ᵐ

subValues ∷ Sub a → 𝐿 a
subValues (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = list $ iter (values 𝓈ˡ) ⧺ iter (values 𝓈ᵐ)

substVarLexical ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → Sub a → 𝕏 → m b
substVarLexical mkvar 𝒸 𝓈 x = case subLexis 𝓈 ⋕? x of
  None → return $ mkvar x
  Some e → 𝒸 e

substVarMeta ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → Sub a → 𝕏 → m b
substVarMeta mkvar 𝒸 𝓈 x = case subMetas 𝓈 ⋕? x of
  None → return $ mkvar x
  Some e → 𝒸 e

mapMSub ∷ (Monad m,HasFV b) ⇒ (a → m b) → Sub a → m (Sub b)
mapMSub f (Sub 𝓈ˡ 𝓈ᵐ _𝓍) = do
  𝓈ˡ' ← dict ^$ mapMOn (iter 𝓈ˡ) $ \ (x :* a) → do
    b ← f a
    return $ x ↦ b
  𝓈ᵐ' ← dict ^$ mapMOn (iter 𝓈ᵐ) $ \ (χ :* a) → do
    b ← f a
    return $ χ ↦ b
  return $ mkSub 𝓈ˡ' 𝓈ᵐ'
