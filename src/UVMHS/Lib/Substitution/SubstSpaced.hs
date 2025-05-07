module UVMHS.Lib.Substitution.SubstSpaced where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped


-- =================================================================== --
-- (NAME)SPACED SUBSTITUTION (NAMED AND NAMELESS, SCOPED AND UNSCOPED) --
-- =================================================================== --

--------------------------------------------------------------------------------
-- `SubstSpaced` builds on `SubstScoped` and adds support for:
-- - namespaced scoped   nameless variables
-- - scoped   named variables
-- - unscoped named variables
--
-- The namespace parameters can be instantiated to easily recover named
-- variables. Conceptually a nameless substitution is a map `ℕ ⇰ Expr`. A well
-- behaved named substitution will also track a notion of nameless indices (see
-- example below), and so a named substitution is a map from `𝕎 ∧ ℕ ⇰ Expr`.
-- This can be restructued as `ℕ ⇰ 𝕎 ⇰ Expr`, and because `SubstSpaced` has a
-- generic structure `ℕ ⇰ s ⇰ Expr` for any `s`, we can just instantiate `s` to
-- be `𝕎` to recover named substitutions.
--
-- [aside]
-- The reason to want nameless indices for named substitutions is so you can do
-- substitutions of the form:
-- 
--     λx.λy.[x↦y](λy.x) ≡ λx.λy.λy.y↑1
--
-- where the final `y↑1` is the name `y` paired with the nameless index `1` to
-- indicate that it points to the outer `y` binding, not the inner `y` binding.
-- This structure allows you to do capture avoiding substitution very cleanly
-- without the need to gensym unique variable names, or rename/rebind lambdas
-- during substitution. □
--
-- Say you have two scope namespaces: one for expressions and one for types,
-- and we have a simple enum type `data Scope = Exp | Type`.
-- Then, you could instantiate `SubstSpaced sU sS e` with:
-- - `sU = Scope ∧ 𝕎`  
--   - i.e., an unscoped substitution for each (global) raw variable name and
--   `Scope`
-- - `sS = Scope ∧ 𝑂 𝕏` 
--   - i.e., a scoped for each for each: (non-global) raw variable name, and
--    `Scope`, plus one additional for nameless variables
--  In this way, the generic `SubstSpaced` type is instantiated to recover
--  three conceptual substitution maps:
--  - unscoped substitutions for global named expression variables and global
--    named type variables
--  - scoped substitutions for named expression variables and named type
--    variables
--  - scoped substitutions for nameless expression variables and nameless type
--    variables
--------------------------------------------------------------------------------

-- 𝓈 = ⟨𝓈U,𝓈S⟩
data SubstSpaced sU sS e = SubstSpaced
  { substSpacedUnscoped ∷ sU ⇰ SubstElem sS e
  , substSpacedScoped   ∷ sS ⇰ SubstScoped sS e
  }
  deriving (Eq,Ord,Show)
makeLenses ''SubstSpaced

canonSubstSpaced ∷ (Eq sS,Eq e) ⇒ e ⌲ ℕ64 → (sS ⇰ ℕ64 → e → 𝑂 e) → SubstSpaced sU sS e → SubstSpaced sU sS e
canonSubstSpaced ℓvar substE (SubstSpaced 𝓈U 𝓈S) = 
  let 𝓈U' = map (canonSubstElem substE) 𝓈U
      𝓈S' = map (canonSubstScoped ℓvar substE) 𝓈S
  in SubstSpaced 𝓈U' 𝓈S'

-- Alter a substitution to "protect" the first n nameless indices. This
-- commonly occurs when moving a substitution underneath a binder.
-- E.g.,
--
--     shift 1
--     [ 0 ↦ 1
--     , 1 ↦ 2
--     , 2 ↦ 3
--     ]
--     ≡
--     [ 0 ↦ 0
--     , 1 ↦ 2
--     , 2 ↦ 3
--     , 3 ↦ 4
--     ]
shiftSubstSpaced ∷ (Ord sS) ⇒ sS ⇰ ℕ64 → SubstSpaced sU sS e → SubstSpaced sU sS e
shiftSubstSpaced ιs (SubstSpaced 𝓈U 𝓈S) =
  let 𝓈U' = map (introSubstElem ιs) 𝓈U
      𝓈S' = kmap (shiftSubstScoped ιs) 𝓈S
  in SubstSpaced 𝓈U' 𝓈S'

-- The substitution that introduces de bruijn variable 0, and shifts everything
-- else up by one.
-- E.g.,
--
--     intro 1
--     ≡
--     [ 0 ↦ 1
--     , 1 ↦ 2
--     , 2 ↦ 3 
--     , …
--     ]
introSubstSpaced ∷ sS ⇰ ℕ64 → SubstSpaced sU sS e
introSubstSpaced = SubstSpaced null ∘ map introSubstScoped

sbindsSubstSpaced ∷ sS ⇰ 𝕍 e → SubstSpaced sU sS e
sbindsSubstSpaced ess = SubstSpaced null $ mapOn ess $ \ es →
  let ℯs = map (Trm_SSE ∘ SubstElem null ∘ Some) es
      ι  = neg $ intΩ64 $ csize es
  in SubstScoped zero ℯs ι

ubindsSubstSpaced ∷ sU ⇰ e → SubstSpaced sU sS e
ubindsSubstSpaced es = SubstSpaced (map (SubstElem null ∘ Some) es) null

-- 𝓈smbindsG ∷ sU ⇰ e → SubstSpaced sU sS e
-- 𝓈smbindsG esᴳ = SubstSpaced null (map (SubstElem null ∘ const ∘ return) esᴳ) null

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


-- substSpacedExtended _ _ 𝓈 ιs e ≈ 𝓈(e⇈ιs)
substSpacedExtended ∷ (Ord sU,Ord sS) ⇒ (sS → e ⌲ ℕ64) → (SubstSpaced sU sS e → e → 𝑂 e) → SubstSpaced sU sS e → sS ⇰ ℕ64 → e → 𝑂 e
substSpacedExtended ℓvar substE 𝓈P ιs = substE $ appendSubstSpaced ℓvar substE 𝓈P $ introSubstSpaced ιs

substSubstElemSpacedE ∷ (Ord sU,Ord sS) ⇒ (sS → e ⌲ ℕ64) → (SubstSpaced sU sS e → e → 𝑂 e) → SubstSpaced sU sS e → SubstElem sS e → 𝑂 e
substSubstElemSpacedE ℓvar substE 𝓈P = substSubstElemE $ substSpacedExtended ℓvar substE 𝓈P

substSubstElemSpaced ∷ (Ord sU,Ord sS) ⇒ (sS → e ⌲ ℕ64) → (SubstSpaced sU sS e → e → 𝑂 e) → SubstSpaced sU sS e → SubstElem sS e → SubstElem sS e
substSubstElemSpaced ℓvars substE 𝓈P = substSubstElem $ substSpacedExtended ℓvars substE 𝓈P

substSSubstElemSpaced ∷ (Ord sU,Ord sS) ⇒ (sS → e ⌲ ℕ64) → (SubstSpaced sU sS e → e → 𝑂 e) → SubstSpaced sU sS e → sS → SSubstElem sS e → SSubstElem sS e
substSSubstElemSpaced ℓvars substE 𝓈P s = substSSubstElem (ℓvars s) $ substSpacedExtended ℓvars substE 𝓈P

appendSubstSpaced ∷
  ∀ sU sS e. (Ord sU,Ord sS)
  ⇒ (sS → e ⌲ ℕ64)
  → (SubstSpaced sU sS e → e → 𝑂 e)
  → SubstSpaced sU sS e
  → SubstSpaced sU sS e
  → SubstSpaced sU sS e
appendSubstSpaced ℓvars substE 𝓈P₂ 𝓈P₁ =
  let SubstSpaced 𝓈U₁ 𝓈S₁ = 𝓈P₁
      SubstSpaced 𝓈U₂ 𝓈S₂ = 𝓈P₂
      𝓈Uᵣ = map (substSubstElemSpaced ℓvars substE 𝓈P₂) 𝓈U₁ ⩌ 𝓈U₂
      𝓈Sᵣ= dkunionByOn 𝓈S₁ 𝓈S₂ $ \ s 𝓈₁ 𝓈₂ →
        if
        | isNullSubstScoped 𝓈₁ → 𝓈₂
        | isNullSubstScoped 𝓈₂ → 𝓈₁
        | otherwise →
            let SubstScoped ρ̇₁ es₁ ι₁ = 𝓈₁
                SubstScoped ρ̇₂ es₂ ι₂ = 𝓈₂
                𝔰₁ = intΩ64 $ csize es₁
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
                  | n < ρ₁+𝔰₁      → substSSubstElemSpaced ℓvars substE 𝓈P₂ s $ es₁ ⋕! natΩ64 (n-ρ₁)
                  | n < ρ₂-ι₁      → Var_SSE $ natΩ64 $ n+ι₁
                  | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
                  | otherwise      → error "bad"
            in
            SubstScoped ρ̇ es ι
  in SubstSpaced 𝓈Uᵣ 𝓈Sᵣ

-------------
-- FUNCTOR --
-------------

instance Functor (SubstSpaced sU sS) where
  map f (SubstSpaced 𝓈U 𝓈S) = SubstSpaced (mapp f 𝓈U) $ mapp f 𝓈S

---------------------
-- PRETTY PRINTING --
---------------------

instance (Pretty s₁,Pretty s₂,Pretty e) ⇒ Pretty (SubstSpaced s₁ s₂ e) where
  pretty (SubstSpaced 𝓈U 𝓈S) = ppDict $ concat
    [ if csize 𝓈U ≡ 0 then null𝐼 else single $ ppCon "𝐔" :* pretty 𝓈U
    , if csize 𝓈S ≡ 0 then null𝐼 else single $ ppCon "𝐒" :* pretty 𝓈S
    ]

-------------
-- FUZZING --
-------------

-- generates random substitutions for property based testing
instance (Ord sU,Ord sS,Fuzzy sU,Fuzzy sS,Fuzzy e) ⇒ Fuzzy (SubstSpaced sU sS e) where
  fuzzy = return SubstSpaced ⊡ fuzzy ⊡ fuzzy

---------------
-- SHRINKING --
---------------

instance (Ord sU,Ord sS,Shrinky e) ⇒ Shrinky (SubstSpaced sU sS e) where
  shrink (SubstSpaced 𝓈U 𝓈S) = do
    𝓈U' ← shrink 𝓈U
    𝓈S' ← shrink 𝓈S
    return $ SubstSpaced 𝓈U' 𝓈S'
