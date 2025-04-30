module UVMHS.Lib.Substitution.GSubst where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand

import UVMHS.Lib.Substitution.DSubst
import UVMHS.Lib.Substitution.Var

-------------------------------
-- GENERIC SCOPED SUBSTITUTION --
-------------------------------

-- A "named" variable will still use GSubst. Substitutions for named variables
-- are seen as maps from variable names (i.e., 𝕎 things, or just strings
-- conceptually) to a DSubst. In order to perform substitutions on DSubst, you
-- need to also have the GVar and MVar substitution environments lying around.
-- So the GSubst type is used for both named and de-bruijn substitutions.
--
-- Put another way, you can think of `DVar` substitutions using `DSubst` and `NVar`
-- substitutions using `𝕎 ⇰ DSubst`. When you keep around the GVar and MVar
-- subsitution environments, you end up with `GSubst` and `𝕎 ⇰ GSubst` as the
-- DVar and NVar substitution structures.

data GSubst s₁ s₂ e = GSubst
  { gsubstGVars ∷ s₁ ⇰ SubstElem s₂ e
  -- , gsubstMetas ∷ s₁ ⇰ SubstElem s₂ e
  , gsubstSubst ∷ s₂ ⇰ DSubst s₂ e
  }
  deriving (Eq,Ord,Show)
makeLenses ''GSubst

instance (Pretty a, Pretty b, Pretty c) ⇒ Pretty (GSubst a b c) where
  pretty (GSubst g s)
    | csize g ≡ 0 ⩓ csize s ≡ 0 = ppString "⊘"
    | otherwise =
        ppGA $ ppCollection (ppPun "⟨") (ppPun "⟩") (ppPun ",")
          [ concat [ppString "𝐆:", ppGA $ pretty g]
          -- , concat [ppString "𝐌:", ppGA $ pretty m]
          , concat [ppString "𝐒:", ppGA $ pretty s]
          ]

instance Functor (GSubst s₁ s₂) where
  map f (GSubst a c) = GSubst (map (map f) a) (map (map f) c)

-- generates random substitutions for property based testing
instance (Ord s₁,Ord s₂,Fuzzy s₁,Fuzzy s₂,Fuzzy e) ⇒ Fuzzy (GSubst s₁ s₂ e) where
  fuzzy = return GSubst ⊡ fuzzy ⊡ fuzzy

-- alter a substitution to "protect" the first n de bruijn indices
-- 0 ↦ 1
-- 1 ↦ 2
-- 2 ↦ 3
-- ⇒ shift 1
-- 0 ↦ 0
-- 1 ↦ 2
-- 2 ↦ 3
-- 3 ↦ 4
𝓈shiftG ∷ (Ord s₂) ⇒ s₂ ⇰ ℕ64 → GSubst s₁ s₂ e → GSubst s₁ s₂ e
𝓈shiftG 𝑠 (GSubst esᴳ 𝓈s) =
  let esᴳ' = map (introSubstElem 𝑠) esᴳ
      𝓈s' = kmapOn 𝓈s $ \ s (DSubst ρ es ι) →
        let ρ'  = ρ + ifNone 0 (𝑠 ⋕? s)
            es' = mapOn es $ introSSubstElem s 𝑠
        in DSubst ρ' es' ι
  in GSubst esᴳ' 𝓈s'

-- the substitution that introduces de bruijn variable 0, and shifts everything
-- else up by one
-- 0 ↦ 1
-- 1 ↦ 2
-- etc.
𝓈introG ∷ s₂ ⇰ ℕ64 → GSubst s₁ s₂ e
𝓈introG 𝑠 = GSubst null $ mapOn 𝑠 $ DSubst 0 null ∘ intΩ64

𝓈sbindsG ∷ s₂ ⇰ 𝕍 e → GSubst s₁ s₂ e
𝓈sbindsG ess = GSubst null $ mapOn ess $ \ es →
  let ℯs = map (Trm_SSE ∘ SubstElem null ∘ const ∘ return) es
      ι  = neg $ intΩ64 $ csize es
  in DSubst zero ℯs ι

𝓈sgbindsG ∷ s₁ ⇰ e → GSubst s₁ s₂ e
𝓈sgbindsG esᴳ = GSubst (map (SubstElem null ∘ const ∘ return) esᴳ) null

-- 𝓈smbindsG ∷ s₁ ⇰ e → GSubst s₁ s₂ e
-- 𝓈smbindsG esᴳ = GSubst null (map (SubstElem null ∘ const ∘ return) esᴳ) null

-- 𝓈₁ ≜ ⟨ρ₁,es₁,ι₁⟩
-- 𝓈₂ ≜ ⟨ρ₂,es₂,ι₂⟩
-- 𝔰₁ = |es₁|
-- 𝔰₂ = |es₂|
-- (𝓈₂⧺𝓈₁)(i)
-- ==
-- 𝓈₂(𝓈₁(i))
-- ==
-- cases (sequential):
--   | i < ρ₁    ⇒ 𝓈₂(i)
--   | i < ρ₁+𝔰₁ ⇒ 𝓈₂(es₁[i-ρ₁])
--   | ⊤         ⇒ 𝓈₂(i+ι₁)
-- ==
-- cases (sequential):
--   | i < ρ₁    ⇒ cases (sequential):
--                    | i < ρ₂    ⇒ i
--                    | i < ρ₂+𝔰₂ ⇒ es₂[i-ρ₂]
--                    | ⊤         ⇒ i+ι₂
--   | i < ρ₁+𝔰₁ ⇒ 𝓈₂(es₁[i-ρ₁])
--   | ⊤         ⇒ cases (sequential):
--                    | i < ρ₂-ι₁    ⇒ i+ι₁
--                    | i < ρ₂+𝔰₂-ι₁ ⇒ es₂[i+ι₁-ρ₂]
--                    | ⊤            ⇒ i+ι₁+ι₂
-- ==
-- cases (sequential):
--   | i < ρ₁⊓ρ₂      ⇒ i
--   ---------------------------------
--   | i < ρ₁⊓(ρ₂+𝔰₂) ⇒ es₂[i-ρ₂]
--   | i < ρ₁         ⇒ i+ι₂
--   | i < ρ₁+𝔰₁      ⇒ 𝓈₂(es₁[i-ρ₁])
--   | i < ρ₂-ι₁      ⇒ i+ι₁
--   | i < ρ₂+𝔰₂-ι₁   ⇒ es₂[i+ι₁-ρ₂]
--   ---------------------------------
--   | ⊤              ⇒ i+ι₁+ι₂
-- == ⟨ρ,es,ι⟩(i)
-- where
--     ρ = ρ₁⊓ρ₂
--     ι = ι₁+ι₂
--     𝔰 ≜ |es|
--   ρ+𝔰 = (ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁)
--     𝔰 = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
appendGSubst ∷
  (Ord s₁,Ord s₂)
  ⇒ (GSubst s₁ s₂ e → e → 𝑂 e)
  → GSubst s₁ s₂ e
  → GSubst s₁ s₂ e
  → GSubst s₁ s₂ e
appendGSubst esubst 𝓈̂₂ 𝓈̂₁ =
  let GSubst esᴳ₁ 𝓈s₁ = 𝓈̂₁
      GSubst esᴳ₂ 𝓈s₂ = 𝓈̂₂
      esub 𝓈 𝑠 = esubst $ appendGSubst esubst 𝓈 $ 𝓈introG 𝑠
      ℯsub s 𝓈 = subSSubstElem (elim𝑂 (const Var_SSE) dsubstVar $ gsubstSubst 𝓈 ⋕? s) $ esub 𝓈
      esᴳ₁' = map (subSubstElem $ esub 𝓈̂₂) esᴳ₁
      -- esᴹ₁' = map (subSubstElem $ esub 𝓈̂₂) esᴹ₁
      𝓈s₁' = kmapOn 𝓈s₁ $ \ s (DSubst ρ̇₁ es₁ ι₁) → DSubst ρ̇₁ (mapOn es₁ $ ℯsub s 𝓈̂₂) ι₁
      esᴳ = esᴳ₁' ⩌ esᴳ₂
      -- esᴹ = esᴹ₁' ⩌ esᴹ₂
      𝓈s = dunionByOn 𝓈s₂ 𝓈s₁' $ \ 𝓈₂@(DSubst ρ̇₂ es₂ ι₂) 𝓈₁@(DSubst ρ̇₁ es₁ ι₁) →
        if
        | isNullDSubst 𝓈₁ → 𝓈₂
        | isNullDSubst 𝓈₂ → 𝓈₁
        | otherwise →
            let 𝔰₁ = intΩ64 $ csize es₁
                𝔰₂ = intΩ64 $ csize es₂
                ρ₁ = intΩ64 ρ̇₁
                ρ₂ = intΩ64 ρ̇₂
                ρ̇  = ρ̇₁⊓ρ̇₂
                ρ  = intΩ64 ρ̇
                ι  = ι₁+ι₂
                𝔰  = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
                δ  = ρ
                es = vecF (natΩ64 𝔰) $ \ ṅ →
                  let n = intΩ64 ṅ + δ in
                  if
                  | n < ρ₁⊓(ρ₂+𝔰₂) → es₂ ⋕! natΩ64 (n-ρ₂)
                  | n < ρ₁         → Var_SSE $ natΩ64 $ n+ι₂
                  | n < ρ₁+𝔰₁      → es₁ ⋕! natΩ64 (n-ρ₁)
                  | n < ρ₂-ι₁      → Var_SSE $ natΩ64 $ n+ι₁
                  | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
                  | otherwise      → error "bad"
            in
            DSubst ρ̇ es ι
  in GSubst esᴳ 𝓈s


-- FYI there is no Substy instance for Subst, which would be "applying a
-- substitution to a substition". The way to achieve that is just through
-- append, or `⧺`, via the Append type class for which Subst has an instance.
newtype Subst s e = Subst {
  unSubst ∷
    GSubst
      (s ∧ 𝕎)   -- domain for global variables: scope + gvar name
      (s ∧ 𝑂 𝕎) -- domain for scoped variables: scope + either name or None for de Bruijn substitution
      e
  }
  deriving (Eq,Ord,Show,Pretty,Fuzzy)
makeLenses ''Subst

newtype MetaSubst s e = MetaSubst { unMetaSubst ∷ (s ∧ 𝕎) ⇰ SubstElem (s ∧ 𝑂 𝕎) e }
  deriving (Eq,Ord,Show,Pretty,Fuzzy)
makeLenses ''MetaSubst

instance Functor (Subst s) where
  map f (Subst s) = Subst (map f s)
