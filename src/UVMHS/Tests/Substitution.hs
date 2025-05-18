module UVMHS.Tests.Substitution (g__TESTS__UVMHS__Tests__Substitution) where

import UVMHS.Core

import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Substitution
import UVMHS.Lib.Testing
import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

import UVMHS.Lang.ULC

-- substitutions --

𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
                 [| "{}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ SubstScoped 1 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
                 [| "{:0…:0↦[≡]}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ SubstScoped 2 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
                 [| "{:0…:1↦[≡]}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) 1 |] 
                 [| "{:0…:∞↦[+1]}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) $ neg 1 |] 
                 [| "{:0…:∞↦[-1]}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ 
                      SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ vec [Trm_SSE $ SubstElem null $ Some ()]) 0 
                  |] 
                 [| "{:0↦()}" |]
𝔱 "subst:pretty" [| ppRenderNoFmtWide $ pretty $ 
                      SubstScoped 1 (id @(𝕍 (SSubstElem () ())) $ vec [Trm_SSE $ SubstElem null $ Some ()]) 3 
                 |] 
                 [| "{:0…:0↦[≡],:1↦(),:2…:∞↦[+3]}" |]

𝔱 "subst:parse" [| [ulc| χ:m{} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") null |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:0↦[≡]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ SubstScoped 1 null 0 
                |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:1↦[≡]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ SubstScoped 2 null 0 
                |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0↦0} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ 
                       let es = vec [Trm_SSE $ SubstElem null $ Some [ulc|0|]]
                       in SubstScoped 0 es 0 
                |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:∞↦[≡]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ SubstScoped 0 null 0 
                |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:∞↦[+1]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ SubstScoped 0 null 1
                |]
𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:0↦[≡],x:1↦0,x:2…x:∞↦[+1]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* Some (var "x")) $ 
                       let es = vec [Trm_SSE $ SubstElem null $ Some [ulc|0|]]
                       in SubstScoped 1 es 1 
                |]

-- 𝔱 "subst:parse" [| [ulc| χ:m{x:0…x:1↦[≡],x:1↦0,x:2↦1,x:3…x:∞↦[-2]} |] |] 
--                 [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
--                      (↦) (() :* Some (var "x")) $ 
--                        let es = vec 
--                              [ Trm_SSE $ SubstElem null $ Some [ulc|0|]
--                              , Trm_SSE $ SubstElem null $ Some [ulc|1|]
--                              ]
--                        in SubstScoped 0 es (-1)
--                 |]

-- 𝔱 "subst:subst" [| [ulc| χ:m{} |] |] [| [ulc| 0 |] |]
𝔱 "subst:subst" [| concat 
                     [ dshiftSubst 1 $ dbindSubst [ulc| 0 |] 
                     , nshiftSubst (var "x" ↦ 1) $ nbindSubst (var "x") [ulc| 1 |]
                     ] |] [| null |]

-- basic --

𝔱 "subst:id" [| subst null [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 1   |] |] [| Some [ulc| λ → 1   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 2   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 2 |] |]

𝔱 "subst:intro" [| subst (dintroSubst 1) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 1) [ulc| λ → 1   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 1) [ulc| λ → 2   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 1) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 3 |] |]

𝔱 "subst:intro" [| subst (dintroSubst 2) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 2) [ulc| λ → 1   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 2) [ulc| λ → 2   |] |] [| Some [ulc| λ → 4   |] |]
𝔱 "subst:intro" [| subst (dintroSubst 2) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 4 |] |]

𝔱 "subst:bind" [| subst (dbindSubst [ulc| λ → 0 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (dbindSubst [ulc| λ → 1 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (dbindSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:bind" [| subst (dbindSubst [ulc| λ → 1 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 2 |] |]

𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 0 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 1 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 1 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 2 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 2 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 1 |]) [ulc| λ → 2 |] |]
                [| Some [ulc| λ → λ → 3 |] |]
𝔱 "subst:shift" [| subst (dshiftSubst 1 $ dbindSubst [ulc| λ → 2 |]) [ulc| λ → 2 |] |]
                [| Some [ulc| λ → λ → 4 |] |]

-- append --

𝔱 "subst:⧺" [| subst null                          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)                 [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (dshiftSubst 1 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (dshiftSubst 2 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst null                          [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)                 [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]

𝔱 "subst:⧺" [| subst (dintroSubst 1)               [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ dintroSubst 1 ⧺ null) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst (dintroSubst 1)               [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ dintroSubst 1 ⧺ null) [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]

𝔱 "subst:⧺" [| subst (dbindSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ dbindSubst [ulc| λ → 0 |] ⧺ null) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (dintroSubst 2)                 [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]
𝔱 "subst:⧺" [| subst (dintroSubst 1 ⧺ dintroSubst 1) [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]

𝔱 "subst:⧺" [| subst (dbindSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (dshiftSubst 1 (dbindSubst [ulc| λ → 0 |]) ⧺ dintroSubst 1) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (dintroSubst 1 ⧺ dbindSubst [ulc| 1 |]) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]
𝔱 "subst:⧺" [| subst (dshiftSubst 1 (dbindSubst [ulc| 1 |]) ⧺ dintroSubst 1) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]

𝔱 "subst:⧺" [| subst (dintroSubst 1) *$ subst (dshiftSubst 1 null) [ulc| 0 |] |]
            [| subst (dintroSubst 1 ⧺ dshiftSubst 1 null) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (dbindSubst [ulc| 1 |]) *$ subst (dshiftSubst 1 (dintroSubst 1)) [ulc| 0 |] |]
            [| subst (dbindSubst [ulc| 1 |] ⧺ dshiftSubst 1 (dintroSubst 1)) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (dshiftSubst 1 (dbindSubst [ulc| 1 |])) *$ subst (dshiftSubst 1 null) [ulc| 1 |] |]
            [| subst (dshiftSubst 1 (dbindSubst [ulc| 1 |]) ⧺ dshiftSubst 1 null) [ulc| 1 |] |]

𝔱 "subst:⧺" [| subst (dshiftSubst 1 (dbindSubst [ulc| 3 |]) ⧺ null) [ulc| 0 |] |]
            [| subst (dshiftSubst 1 (dbindSubst [ulc| 3 |])) [ulc| 0 |] |]

-- de bruijn conversion --

𝔱 "subst:todbr" [| todbr [ulc| λ x → x             |] |] [| Some [ulc| λ x → 0             |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ x → 0             |] |] [| Some [ulc| λ x → 0             |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0           |] |] [| Some [ulc| λ x → 0 0           |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 1         |] |] [| Some [ulc| λ x → 0 0 1         |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 y         |] |] [| Some [ulc| λ x → 0 0 y         |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ x → x 0 1 y       |] |] [| Some [ulc| λ x → 0 0 1 y       |] |]

𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x       |] |] [| Some [ulc| λ y → λ x → 0       |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → 0       |] |] [| Some [ulc| λ y → λ x → 0       |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0     |] |] [| Some [ulc| λ y → λ x → 0 0     |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 1   |] |] [| Some [ulc| λ y → λ x → 0 0 1   |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 y   |] |] [| Some [ulc| λ y → λ x → 0 0 1   |] |]
𝔱 "subst:todbr" [| todbr [ulc| λ y → λ x → x 0 1 y |] |] [| Some [ulc| λ y → λ x → 0 0 1 1 |] |]

𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x             |] |] [| Some [ulc| λ x → x             |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ x → 0             |] |] [| Some [ulc| λ x → x             |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0           |] |] [| Some [ulc| λ x → x x           |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 1         |] |] [| Some [ulc| λ x → x x 1         |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 y         |] |] [| Some [ulc| λ x → x x y         |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ x → x 0 1 y       |] |] [| Some [ulc| λ x → x x 1 y       |] |]

𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x       |] |] [| Some [ulc| λ y → λ x → x       |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → 0       |] |] [| Some [ulc| λ y → λ x → x       |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0     |] |] [| Some [ulc| λ y → λ x → x x     |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 1   |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 y   |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
𝔱 "subst:tonmd" [| tonmd [ulc| λ y → λ x → x 0 1 y |] |] [| Some [ulc| λ y → λ x → x x y y |] |]

𝔱 "subst:fvs" [| fvs [ulc| λ x → 0           |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → x           |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → 1 0   |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → x 0   |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → 1 y   |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → λ y → x y   |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 0) 0 |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → y) 0 |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 0) x |] |] [| null |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → y) x |] |] [| null |]

𝔱 "subst:fvs" [| fvs [ulc| 0                   |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs [ulc| 0 1                 |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0,1]     |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → 0 1           |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 2) 0   |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 1) 1   |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → 2) 1   |] |] [| (↦♭) () $ pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs [ulc| x                   |] |] [| (↦♭) () $ pow𝑃 $ map (znuvar∘var) ["x"]     |]
𝔱 "subst:fvs" [| fvs [ulc| x y                 |] |] [| (↦♭) () $ pow𝑃 $ map (znuvar∘var) ["x","y"] |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → y             |] |] [| (↦♭) () $ pow𝑃 $ map (znuvar∘var) ["y"]     |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → x) y   |] |] [| (↦♭) () $ pow𝑃 $ map (znuvar∘var) ["y"]     |]
𝔱 "subst:fvs" [| fvs [ulc| λ x → (λ y → x) x y |] |] [| (↦♭) () $ pow𝑃 $ map (znuvar∘var) ["y"]     |]

𝔱 "subst:metas" [| subst  (nbindSubst (var "x") [ulc| y |]) [ulc| x         |] |] [| Some [ulc| y         |] |]
𝔱 "subst:metas" [| subst  (nbindSubst (var "x") [ulc| y |]) [ulc| λ y → x   |] |] [| Some [ulc| λ y → y:1 |] |]
𝔱 "subst:metas" [| msubst (mbindSubst (var "x") [ulc| y |]) [ulc| x:m       |] |] [| Some [ulc| y         |] |]
𝔱 "subst:metas" [| msubst (mbindSubst (var "x") [ulc| y |]) [ulc| λ y → x:m |] |] [| Some [ulc| λ y → y   |] |]

-- 𝔱 "subst:metas:delayed-subst (the one that solves our problem!!!)"
--   [| msubst (mbindSubst (var "x") [ulc| 0 |]) [ulc| x:m (λ y → x:m[1]) |] |]
--   [| Some [ulc| 0 (λ y → 1) |] |]
-- 
-- 𝔱 "subst:other"
--   [| msubst (mbindSubst (var "χ") [ulc| 0 |]) [ulc| χ:m (λ y → x:m[1]) |] |]
--   [| Some [ulc| 0 (λ y → 2) |] |]

-- -- --   -- {m:x ↦ int}      m:x, m:y, m:z
-- -- --   --                  int, (m:y){m:x↦int}, (m:x){m:x↦int}    don't want
-- -- --   -- {⌊0⌋ ↦ int}      m:x, m:y, m:z
-- -- --   --                  (m:x){⌊0⌋ ↦ int}
-- -- --
-- -- -- -- metavariables
-- -- --
-- -- -- -- fuzzing --
-- 
-- 𝔣 "zzz:subst:hom:refl:tny" 1
--   [| do e ← randTny @ULCExpRaw
--         return e
--   |]
--   [| \ e → subst null e ≡ Some e |]
-- 
-- -- 𝔣 "zzz:subst:hom:refl:sml" 100
-- --   [| do e ← randSml @ULCExpRaw
-- --         return e
-- --   |]
-- --   [| \ e → subst null e ≡ Some e |]
-- 
-- -- -- 𝔣 "zzz:subst:hom:⧺:nometa" 100
-- -- --   -- generate things to test (100 things)
-- -- --   [| do 𝓈₁ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
-- -- --         𝓈₂ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
-- -- --         e ← randSml @ULCExpRaw
-- -- --         return $ 𝓈₁ :* 𝓈₂ :* e
-- -- --   |]
-- -- --   -- test one of the things that was generated
-- -- --   [| \ (𝓈₁ :* 𝓈₂ :* e) → subst (𝓈₁ ⧺ 𝓈₂) e ≡ (subst 𝓈₁ *$ subst 𝓈₂ e) |]
-- -- 
-- -- -- 𝔣 "zzz:subst:lunit:⧺" 100
-- -- --   [| do 𝓈 ← randSml @(Subst () ULCExpRaw)
-- -- --         e ← randSml @ULCExpRaw
-- -- --         return $ 𝓈 :* e
-- -- --   |]
-- -- --   [| \ (𝓈 :* e) → subst (null ⧺ 𝓈) e ≡ subst 𝓈 e |]
-- -- 
-- -- -- 𝔣 "zzz:subst:runit:⧺" 100
-- -- --   [| do 𝓈 ← randSml @(Subst () ULCExpRaw)
-- -- --         e ← randSml @ULCExpRaw
-- -- --         return $ 𝓈 :* e
-- -- --   |]
-- -- --   [| \ (𝓈 :* e) → subst (𝓈 ⧺ null) e ≡ subst 𝓈 e |]
-- -- 
-- -- -- 𝔣 "zzz:subst:assoc:⧺" 10
-- -- --   [| do 𝓈₁ ← randSml @(Subst () ULCExpRaw)
-- -- --         𝓈₂ ← randSml @(Subst () ULCExpRaw)
-- -- --         𝓈₃ ← randSml @(Subst () ULCExpRaw)
-- -- --         e ← randSml @ULCExpRaw
-- -- --         return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
-- -- --   |]
-- -- --   [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → subst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ subst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e |]
-- -- 
-- -- -- 𝔣 "zzz:subst:unit:shift" 100
-- -- --   [| do i ← randSml @ℕ64
-- -- --         e ← randSml @ULCExpRaw
-- -- --         return $ i :* e
-- -- --   |]
-- -- --   [| \ (i :* e) → subst (dshiftSubst i null) e ≡ Some e |]
-- -- 
-- -- -- 𝔣 "zzz:subst:unit:bind∘intro" 100
-- -- --   [| do e₁ ← randSml @ULCExpRaw
-- -- --         e₂ ← randSml @ULCExpRaw
-- -- --         return $ e₁ :* e₂
-- -- --   |]
-- -- --   [| \ (e₁ :* e₂) → (subst (dbindSubst e₁) *$ subst (dintroSubst 1) e₂) ≡ Some e₂ |]
-- -- 
-- -- -- 𝔣 "zzz:subst:commute:intro∘bind" 100
-- -- --   [| do e₁ ← randSml @ULCExpRaw
-- -- --         e₂ ← randSml @ULCExpRaw
-- -- --         return $ e₁ :* e₂
-- -- --   |]
-- -- --   [| \ (e₁ :* e₂) →
-- -- --          (subst (dintroSubst 1) *$ subst (dbindSubst e₁) e₂)
-- -- --          ≡
-- -- --          (subst (dshiftSubst 1 $ dbindSubst e₁) *$ subst (dintroSubst 1) e₂)
-- -- --   |]
-- -- 
-- -- -- 𝔣 "zzz:subst:dist:shift/⧺:nometa" 100
-- -- --   [| do n  ← randSml @ℕ64
-- -- --         𝓈₁ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
-- -- --         𝓈₂ ← alter (gsubstMetasL ⊚ unSubstL) null ^$ randSml @(Subst () ULCExpRaw)
-- -- --         e  ← randSml @ULCExpRaw
-- -- --         return $ n :* 𝓈₁ :* 𝓈₂ :* e
-- -- --   |]
-- -- --   [| \ (n :* 𝓈₁ :* 𝓈₂ :* e) → subst (dshiftSubst n (𝓈₁ ⧺ 𝓈₂)) e ≡ subst (dshiftSubst n 𝓈₁ ⧺ dshiftSubst n 𝓈₂) e |]
-- -- 
-- -- -- 𝔣 "zzz:subst:todbr:idemp" 100
-- -- --   [| do randSml @ULCExpRaw |]
-- -- --   [| \ e → todbr e ≡ (todbr *$ todbr e)  |]
-- -- -- 
-- -- -- 𝔣 "zzz:subst:todbr:∘tonmd" 100
-- -- --   [| do randSml @ULCExpRaw |]
-- -- --   [| \ e → todbr e ≡ (todbr *$ tonmd e)  |]
-- -- -- 
-- -- -- 𝔣 "zzz:subst:tonmd:idemp" 100
-- -- --   [| do randSml @ULCExpRaw |]
-- -- --   [| \ e → tonmd e ≡ (tonmd *$ tonmd e)  |]
-- -- -- 
-- -- -- 𝔣 "zzz:subst:tonmd:∘todbr" 100
-- -- --   [| do randSml @ULCExpRaw |]
-- -- --   [| \ e → tonmd e ≡ (tonmd *$ todbr e)  |]

buildTests
