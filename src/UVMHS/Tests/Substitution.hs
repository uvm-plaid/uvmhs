module UVMHS.Tests.Substitution (g__TESTS__UVMHS__Tests__Substitution) where

import UVMHS.Core

import UVMHS.Lib.Substitution
import UVMHS.Lib.Testing
import UVMHS.Lib.Variables
import UVMHS.Lib.Rand

import UVMHS.Lang.ULCD

-- basic --

𝔱 "subst:id" [| subst null [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 1   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 2   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 2 |] |]

𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 2   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 3 |] |]

𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 4   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 4 |] |]

𝔱 "subst:bind" [| subst (𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → λ → 2 |] |]

𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 2 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 2 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 1 |]) [ulcd| λ → 2 |] |] 
                 [| Some [ulcd| λ → λ → 3 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulcd| λ → 2 |]) [ulcd| λ → 2 |] |] 
                 [| Some [ulcd| λ → λ → 4 |] |]

-- append --

𝔱 "subst:⧺" [| subst null            [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)   [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 2 null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| subst null          [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1)               [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dintro 1 ⧺ null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1)               [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 2 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dintro 1 ⧺ null) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 2 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dbind [ulcd| λ → 0 |] ⧺ null) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 2) [ulcd| λ → 1 |] |]            [| Some [ulcd| λ → 3 |] |]
𝔱 "subst:⧺" [| subst (𝓈dintro 1 ⧺ 𝓈dintro 1) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 3 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulcd| λ → 0 |]) ⧺ 𝓈dintro 1) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1 ⧺ 𝓈dbind [ulcd| 1 |]) [ulcd| 0 (λ → 2) |] |] 
            [| Some [ulcd| 2 (λ → 2) |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulcd| 1 |]) ⧺ 𝓈dintro 1) [ulcd| 0 (λ → 2) |] |] 
            [| Some [ulcd| 2 (λ → 2) |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1) *$ subst (𝓈dshift 1 null) [ulcd| 0 |] |]
            [| subst (𝓈dintro 1 ⧺ 𝓈dshift 1 null) [ulcd| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulcd| 1 |]) *$ subst (𝓈dshift 1 (𝓈dintro 1)) [ulcd| 0 |] |]
            [| subst (𝓈dbind [ulcd| 1 |] ⧺ 𝓈dshift 1 (𝓈dintro 1)) [ulcd| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulcd| 1 |])) *$ subst (𝓈dshift 1 null) [ulcd| 1 |] |]
            [| subst (𝓈dshift 1 (𝓈dbind [ulcd| 1 |]) ⧺ 𝓈dshift 1 null) [ulcd| 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulcd| 3 |]) ⧺ null) [ulcd| 0 |] |]
            [| subst (𝓈dshift 1 (𝓈dbind [ulcd| 3 |])) [ulcd| 0 |] |]

-- fuzzing --

𝔣 "zzz:subst:hom:refl" 100 
  [| do e ← randSml @ULCDExpRaw
        return e
  |]
  [| \ e → 
       subst null e ≡ Some e
  |]

𝔣 "zzz:subst:hom:⧺" 1000
  [| do 𝓈₁ ← rand @(Subst 𝕏 (𝔖 ()) ULCDExpRaw) 1 1
        𝓈₂ ← rand @(Subst 𝕏 (𝔖 ()) ULCDExpRaw) 1 1
        e ← rand @ULCDExpRaw 0 0
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → 
       subst (𝓈₁ ⧺ 𝓈₂) e ≡ (subst 𝓈₁ *$ subst 𝓈₂ e)
  |]

-- -- -- -- -- 𝔣 "zzz:subst:lunit:⧺" 100 
-- -- -- -- --   [| do 𝓈 ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         e ← randSml @ULCDExpR
-- -- -- -- --         return $ 𝓈 :* e
-- -- -- -- --   |]
-- -- -- -- --   [| \ (𝓈 :* e) → 
-- -- -- -- --        subst (null ⧺ 𝓈) e ≡ subst 𝓈 e
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:runit:⧺" 100 
-- -- -- -- --   [| do 𝓈 ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         e ← randSml @ULCDExpR
-- -- -- -- --         return $ 𝓈 :* e
-- -- -- -- --   |]
-- -- -- -- --   [| \ (𝓈 :* e) → 
-- -- -- -- --        subst (𝓈 ⧺ null) e ≡ subst 𝓈 e
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:trans:⧺" 100 
-- -- -- -- --   [| do 𝓈₁ ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         𝓈₂ ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         𝓈₃ ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         e ← randSml @ULCDExpR
-- -- -- -- --         return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
-- -- -- -- --   |]
-- -- -- -- --   [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
-- -- -- -- --        subst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ subst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:unit:shift" 100
-- -- -- -- --   [| do i ← randSml @ℕ64
-- -- -- -- --         e ← randSml @ULCDExpR
-- -- -- -- --         return $ i :* e
-- -- -- -- --   |]
-- -- -- -- --   [| \ (i :* e) → subst (𝓈dshift i null) e ≡ Some e 
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:unit:bind∘intro" 100
-- -- -- -- --   [| do e₁ ← randSml @ULCDExpR
-- -- -- -- --         e₂ ← randSml @ULCDExpR
-- -- -- -- --         return $ e₁ :* e₂
-- -- -- -- --   |]
-- -- -- -- --   [| \ (e₁ :* e₂) → (subst (𝓈dbind e₁) *$ subst (𝓈dintro 1) e₂) ≡ Some e₂
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:commute:intro∘bind" 100
-- -- -- -- --   [| do e₁ ← randSml @ULCDExpR
-- -- -- -- --         e₂ ← randSml @ULCDExpR
-- -- -- -- --         return $ e₁ :* e₂
-- -- -- -- --   |]
-- -- -- -- --   [| \ (e₁ :* e₂) → 
-- -- -- -- --        (subst (𝓈dintro 1) *$ subst (𝓈dbind e₁) e₂)
-- -- -- -- --        ≡ 
-- -- -- -- --        (subst (𝓈dshift 1 $ 𝓈dbind e₁) *$ subst (𝓈dintro 1) e₂)
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- 𝔣 "zzz:subst:dist:shift/⧺" 100 
-- -- -- -- --   [| do n  ← randSml @ℕ64
-- -- -- -- --         𝓈₁ ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         𝓈₂ ← randSml @(GDSubst ULCDExpR)
-- -- -- -- --         e  ← randSml @ULCDExpR
-- -- -- -- --         return $ n :* 𝓈₁ :* 𝓈₂ :* e
-- -- -- -- --   |]
-- -- -- -- --   [| \ (n :* 𝓈₁ :* 𝓈₂ :* e) → 
-- -- -- -- --        subst (𝓈dshift n (𝓈₁ ⧺ 𝓈₂)) e ≡ subst (𝓈dshift n 𝓈₁ ⧺ 𝓈dshift n 𝓈₂) e 
-- -- -- -- --   |]
-- -- -- -- -- 
-- -- -- -- -- buildTests

buildTests
