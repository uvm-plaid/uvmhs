module UVMHS.Tests.Substitution (g__TESTS__UVMHS__Tests__Substitution) where

import UVMHS.Core

import UVMHS.Lib.Rand
import UVMHS.Lib.Substitution
import UVMHS.Lib.Testing

import UVMHS.Lang.ULC

-- -- basic --

𝔱 "subst:id" [| subst null [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 1   |] |] [| Some [ulc| λ → 1   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 2   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 2 |] |]

𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulc| λ → 1   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulc| λ → 2   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 1) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 3 |] |]

𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulc| λ → 1   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulc| λ → 2   |] |] [| Some [ulc| λ → 4   |] |]
𝔱 "subst:intro" [| subst (𝓈dintro 2) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 4 |] |]

𝔱 "subst:bind" [| subst (𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulc| λ → 1 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:bind" [| subst (𝓈dbind [ulc| λ → 1 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 2 |] |]

𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 0 |] |]
                 [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 1 |]) [ulc| λ → 0 |] |]
                 [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
                 [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 1 |]) [ulc| λ → 1 |] |]
                 [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 2 |]) [ulc| λ → 0 |] |]
                 [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 2 |]) [ulc| λ → 1 |] |]
                 [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 1 |]) [ulc| λ → 2 |] |]
                 [| Some [ulc| λ → λ → 3 |] |]
𝔱 "subst:shift" [| subst (𝓈dshift 1 $ 𝓈dbind [ulc| λ → 2 |]) [ulc| λ → 2 |] |]
                 [| Some [ulc| λ → λ → 4 |] |]

-- append --

𝔱 "subst:⧺" [| subst null                      [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)             [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 2 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst null                      [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)             [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1)               [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dintro 1 ⧺ null) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1)               [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dintro 1 ⧺ null) [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈dbind [ulc| λ → 0 |] ⧺ null) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 2)             [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]
𝔱 "subst:⧺" [| subst (𝓈dintro 1 ⧺ 𝓈dintro 1) [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulc| λ → 0 |]) ⧺ 𝓈dintro 1) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1 ⧺ 𝓈dbind [ulc| 1 |]) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]
𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulc| 1 |]) ⧺ 𝓈dintro 1) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]

𝔱 "subst:⧺" [| subst (𝓈dintro 1) *$ subst (𝓈dshift 1 null) [ulc| 0 |] |]
            [| subst (𝓈dintro 1 ⧺ 𝓈dshift 1 null) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dbind [ulc| 1 |]) *$ subst (𝓈dshift 1 (𝓈dintro 1)) [ulc| 0 |] |]
            [| subst (𝓈dbind [ulc| 1 |] ⧺ 𝓈dshift 1 (𝓈dintro 1)) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulc| 1 |])) *$ subst (𝓈dshift 1 null) [ulc| 1 |] |]
            [| subst (𝓈dshift 1 (𝓈dbind [ulc| 1 |]) ⧺ 𝓈dshift 1 null) [ulc| 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈dshift 1 (𝓈dbind [ulc| 3 |]) ⧺ null) [ulc| 0 |] |]
            [| subst (𝓈dshift 1 (𝓈dbind [ulc| 3 |])) [ulc| 0 |] |]

-- de bruijn conversion --


-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → x             |] |] [| Some [ulc| λ x → 0             |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → 0             |] |] [| Some [ulc| λ x → 0             |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0           |] |] [| Some [ulc| λ x → 0 0           |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 1         |] |] [| Some [ulc| λ x → 0 0 1         |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 y         |] |] [| Some [ulc| λ x → 0 0 y         |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 1 y       |] |] [| Some [ulc| λ x → 0 0 1 y       |] |]

-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x       |] |] [| Some [ulc| λ y → λ x → 0       |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → 0       |] |] [| Some [ulc| λ y → λ x → 0       |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0     |] |] [| Some [ulc| λ y → λ x → 0 0     |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 1   |] |] [| Some [ulc| λ y → λ x → 0 0 1   |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 y   |] |] [| Some [ulc| λ y → λ x → 0 0 1   |] |]
-- 𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 1 y |] |] [| Some [ulc| λ y → λ x → 0 0 1 1 |] |]

-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x             |] |] [| Some [ulc| λ x → x             |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → 0             |] |] [| Some [ulc| λ x → x             |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0           |] |] [| Some [ulc| λ x → x x           |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 1         |] |] [| Some [ulc| λ x → x x 1         |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 y         |] |] [| Some [ulc| λ x → x x y         |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 1 y       |] |] [| Some [ulc| λ x → x x 1 y       |] |]

-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x       |] |] [| Some [ulc| λ y → λ x → x       |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → 0       |] |] [| Some [ulc| λ y → λ x → x       |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0     |] |] [| Some [ulc| λ y → λ x → x x     |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 1   |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 y   |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
-- 𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 1 y |] |] [| Some [ulc| λ y → λ x → x x y y |] |]

-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → 0           |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → x           |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → 1 0   |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → x 0   |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → 1 y   |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → x y   |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 0) 0 |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → y) 0 |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 0) x |] |] [| null |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → y) x |] |] [| null |]

-- 𝔱 "subst:fvs" [| fvs [ulc| 0                   |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0]             |]
-- 𝔱 "subst:fvs" [| fvs [ulc| 0 1                 |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0,1]           |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → 0 1           |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0]             |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 2) 0   |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0]             |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 1) 1   |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0]             |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 2) 1   |] |] [| (↦♭) () $ pow𝑃 $ map DVar [0]             |]
-- 𝔱 "subst:fvs" [| fvs [ulc| x                   |] |] [| (↦♭) () $ pow𝑃 $ map (nvar∘var) ["x"]     |]
-- 𝔱 "subst:fvs" [| fvs [ulc| x y                 |] |] [| (↦♭) () $ pow𝑃 $ map (nvar∘var) ["x","y"] |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → y             |] |] [| (↦♭) () $ pow𝑃 $ map (nvar∘var) ["y"]     |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → x) y   |] |] [| (↦♭) () $ pow𝑃 $ map (nvar∘var) ["y"]     |]
-- 𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → x) x y |] |] [| (↦♭) () $ pow𝑃 $ map (nvar∘var) ["y"]     |]

𝔱 "subst:metas" [| subst (𝓈nbind (var "x") [ulc| y |]) [ulc| x |] |] [| Some [ulc| y |] |]
𝔱 "subst:metas" [| subst (𝓈nbind (var "x") [ulc| y |]) [ulc| λ y → x |] |] [| Some [ulc| λ y → y↑1 |] |]
𝔱 "subst:metas" [| msubst (𝓈mbind (var "x") [ulc| y |]) [ulc| 𝔪:x |] |] [| Some [ulc| y |] |]
𝔱 "subst:metas" [| msubst (𝓈mbind (var "x") [ulc| y |]) [ulc| λ y → 𝔪:x |] |] [| Some [ulc| λ y → y |] |]

𝔱 "subst:metas:delayed-subst (the one that solves our problem!!!)"
  [| msubst (𝓈mbind (var "x") [ulc| 0 |]) [ulc| 𝔪:x (λ y → 𝔪:[1]x) |] |]
  [| Some [ulc| 0 (λ y → 1) |] |]

--   -- {m:x ↦ int}      m:x, m:y, m:z
--   --                  int, (m:y){m:x↦int}, (m:x){m:x↦int}    don't want
--   -- {⌊0⌋ ↦ int}      m:x, m:y, m:z
--   --                  (m:x){⌊0⌋ ↦ int}
--
-- -- metavariables
--
-- -- fuzzing --

-- 𝔣 "zzz:subst:hom:refl" 1
--   [| do e ← randSml @ULCExpRaw
--         return e
--   |]
--   [| \ e → subst null e ≡ Some e |]

-- 𝔣 "zzz:subst:hom:⧺:nometa" 100
--   -- generate things to test (100 things)
--   [| do 𝓈₁ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
--         𝓈₂ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
--         e ← randSml @ULCExpRaw
--         return $ 𝓈₁ :* 𝓈₂ :* e
--   |]
--   -- test one of the things that was generated
--   [| \ (𝓈₁ :* 𝓈₂ :* e) → subst (𝓈₁ ⧺ 𝓈₂) e ≡ (subst 𝓈₁ *$ subst 𝓈₂ e) |]

-- 𝔣 "zzz:subst:lunit:⧺" 100
--   [| do 𝓈 ← randSml @(Subst () ULCExpRaw)
--         e ← randSml @ULCExpRaw
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → subst (null ⧺ 𝓈) e ≡ subst 𝓈 e |]

-- 𝔣 "zzz:subst:runit:⧺" 100
--   [| do 𝓈 ← randSml @(Subst () ULCExpRaw)
--         e ← randSml @ULCExpRaw
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → subst (𝓈 ⧺ null) e ≡ subst 𝓈 e |]

-- 𝔣 "zzz:subst:assoc:⧺" 10
--   [| do 𝓈₁ ← randSml @(Subst () ULCExpRaw)
--         𝓈₂ ← randSml @(Subst () ULCExpRaw)
--         𝓈₃ ← randSml @(Subst () ULCExpRaw)
--         e ← randSml @ULCExpRaw
--         return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
--   |]
--   [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → subst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ subst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e |]

-- 𝔣 "zzz:subst:unit:shift" 100
--   [| do i ← randSml @ℕ64
--         e ← randSml @ULCExpRaw
--         return $ i :* e
--   |]
--   [| \ (i :* e) → subst (𝓈dshift i null) e ≡ Some e |]

-- 𝔣 "zzz:subst:unit:bind∘intro" 100
--   [| do e₁ ← randSml @ULCExpRaw
--         e₂ ← randSml @ULCExpRaw
--         return $ e₁ :* e₂
--   |]
--   [| \ (e₁ :* e₂) → (subst (𝓈dbind e₁) *$ subst (𝓈dintro 1) e₂) ≡ Some e₂ |]

-- 𝔣 "zzz:subst:commute:intro∘bind" 100
--   [| do e₁ ← randSml @ULCExpRaw
--         e₂ ← randSml @ULCExpRaw
--         return $ e₁ :* e₂
--   |]
--   [| \ (e₁ :* e₂) →
--          (subst (𝓈dintro 1) *$ subst (𝓈dbind e₁) e₂)
--          ≡
--          (subst (𝓈dshift 1 $ 𝓈dbind e₁) *$ subst (𝓈dintro 1) e₂)
--   |]

-- 𝔣 "zzz:subst:dist:shift/⧺:nometa" 100
--   [| do n  ← randSml @ℕ64
--         𝓈₁ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
--         𝓈₂ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
--         e  ← randSml @ULCExpRaw
--         return $ n :* 𝓈₁ :* 𝓈₂ :* e
--   |]
--   [| \ (n :* 𝓈₁ :* 𝓈₂ :* e) → subst (𝓈dshift n (𝓈₁ ⧺ 𝓈₂)) e ≡ subst (𝓈dshift n 𝓈₁ ⧺ 𝓈dshift n 𝓈₂) e |]

𝔣 "zzz:subst:todbr:idemp" 100
  [| do randSml @ULCExpRaw |]
  [| \ e → todbr e ≡ (todbr *$ todbr e)  |]

𝔣 "zzz:subst:todbr:∘tonmd" 100
  [| do randSml @ULCExpRaw |]
  [| \ e → todbr e ≡ (todbr *$ tonmd e)  |]

𝔣 "zzz:subst:tonmd:idemp" 100
  [| do randSml @ULCExpRaw |]
  [| \ e → tonmd e ≡ (tonmd *$ tonmd e)  |]

𝔣 "zzz:subst:tonmd:∘todbr" 100
  [| do randSml @ULCExpRaw |]
  [| \ e → tonmd e ≡ (tonmd *$ todbr e)  |]

buildTests
