module UVMHS.Tests.Substitution (g__TESTS__UVMHS__Tests__Substitution) where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Pretty
import UVMHS.Lib.Shrinky
import UVMHS.Lib.Testing
import UVMHS.Lib.TreeNested

import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Substy
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Var

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
𝔱 "subst:parse" [| [ulc| χ:m{:0…:0↦[≡]} |] |] 
                [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar (var "χ") $ Subst $ SubstSpaced null $ 
                     (↦) (() :* None) $ SubstScoped 1 null 0 
                |]
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

𝔱 "subst:canon" [| canonULC [ulc| χ:m{}                        |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0…x:0↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0…x:1↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0…x:∞↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0…x:1↦[≡],x:2…x:∞↦[≡]} |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0↦x:0}                 |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0↦x:0,x:1↦x:1}         |] |] [| [ulc| χ:m{}             |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0↦x:1,x:1…x:∞↦[+1]}    |] |] [| [ulc| χ:m{x:0…x:∞↦[+1]} |] |]
𝔱 "subst:canon" [| canonULC [ulc| χ:m{x:0…x:1↦[≡],x:2↦x:2,x:3↦x:3,x:4↦(λ→0),x:5↦x:6,x:6↦x:7,x:7…x:∞↦[+1]} |] |] 
                [|          [ulc| χ:m{x:0…x:3↦[≡],x:4↦(λ→0),x:5…x:∞↦[+1]} |] |]

-- basic --

𝔱 "subst:id" [| subst null [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 1   |] |] [| Some [ulc| λ → 1   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 2   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:id" [| subst null [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 2 |] |]

𝔱 "subst:intro" [| subst (introDSubst 1) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (introDSubst 1) [ulc| λ → 1   |] |] [| Some [ulc| λ → 2   |] |]
𝔱 "subst:intro" [| subst (introDSubst 1) [ulc| λ → 2   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (introDSubst 1) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 3 |] |]

𝔱 "subst:intro" [| subst (introDSubst 2) [ulc| λ → 0   |] |] [| Some [ulc| λ → 0   |] |]
𝔱 "subst:intro" [| subst (introDSubst 2) [ulc| λ → 1   |] |] [| Some [ulc| λ → 3   |] |]
𝔱 "subst:intro" [| subst (introDSubst 2) [ulc| λ → 2   |] |] [| Some [ulc| λ → 4   |] |]
𝔱 "subst:intro" [| subst (introDSubst 2) [ulc| λ → 0 2 |] |] [| Some [ulc| λ → 0 4 |] |]

𝔱 "subst:bind" [| subst (bindDSubst [ulc| λ → 0 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (bindDSubst [ulc| λ → 1 |]) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0     |] |]
𝔱 "subst:bind" [| subst (bindDSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:bind" [| subst (bindDSubst [ulc| λ → 1 |]) [ulc| λ → 1 |] |] [| Some [ulc| λ → λ → 2 |] |]

𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 0 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 1 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 1 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 2 |]) [ulc| λ → 0 |] |]
                [| Some [ulc| λ → 0 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 2 |]) [ulc| λ → 1 |] |]
                [| Some [ulc| λ → 1 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 1 |]) [ulc| λ → 2 |] |]
                [| Some [ulc| λ → λ → 3 |] |]
𝔱 "subst:shift" [| subst (shiftDSubst 1 $ bindDSubst [ulc| λ → 2 |]) [ulc| λ → 2 |] |]
                [| Some [ulc| λ → λ → 4 |] |]

-- append --

𝔱 "subst:⧺" [| subst null                          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)                 [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (shiftDSubst 1 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (shiftDSubst 2 null)          [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst null                          [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)                 [ulc| λ → 1 |] |] [| Some [ulc| λ → 1 |] |]

𝔱 "subst:⧺" [| subst (introDSubst 1)               [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ introDSubst 1 ⧺ null) [ulc| λ → 0 |] |] [| Some [ulc| λ → 0 |] |]

𝔱 "subst:⧺" [| subst (introDSubst 1)               [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ introDSubst 1 ⧺ null) [ulc| λ → 1 |] |] [| Some [ulc| λ → 2 |] |]

𝔱 "subst:⧺" [| subst (bindDSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ bindDSubst [ulc| λ → 0 |] ⧺ null) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (introDSubst 2)                 [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]
𝔱 "subst:⧺" [| subst (introDSubst 1 ⧺ introDSubst 1) [ulc| λ → 1 |] |] [| Some [ulc| λ → 3 |] |]

𝔱 "subst:⧺" [| subst (bindDSubst [ulc| λ → 0 |]) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (shiftDSubst 1 (bindDSubst [ulc| λ → 0 |]) ⧺ introDSubst 1) [ulc| λ → 1 |] |]
            [| Some [ulc| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (introDSubst 1 ⧺ bindDSubst [ulc| 1 |]) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]
𝔱 "subst:⧺" [| subst (shiftDSubst 1 (bindDSubst [ulc| 1 |]) ⧺ introDSubst 1) [ulc| 0 (λ → 2) |] |]
            [| Some [ulc| 2 (λ → 2) |] |]

𝔱 "subst:⧺" [| subst (introDSubst 1) *$ subst (shiftDSubst 1 null) [ulc| 0 |] |]
            [| subst (introDSubst 1 ⧺ shiftDSubst 1 null) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (bindDSubst [ulc| 1 |]) *$ subst (shiftDSubst 1 (introDSubst 1)) [ulc| 0 |] |]
            [| subst (bindDSubst [ulc| 1 |] ⧺ shiftDSubst 1 (introDSubst 1)) [ulc| 0 |] |]

𝔱 "subst:⧺" [| subst (shiftDSubst 1 (bindDSubst [ulc| 1 |])) *$ subst (shiftDSubst 1 null) [ulc| 1 |] |]
            [| subst (shiftDSubst 1 (bindDSubst [ulc| 1 |]) ⧺ shiftDSubst 1 null) [ulc| 1 |] |]

𝔱 "subst:⧺" [| subst (shiftDSubst 1 (bindDSubst [ulc| 3 |]) ⧺ null) [ulc| 0 |] |]
            [| subst (shiftDSubst 1 (bindDSubst [ulc| 3 |])) [ulc| 0 |] |]

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

𝔱 "subst:fvs" [| fvs () [ulc| λ x → 0           |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → x           |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → λ y → 1 0   |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → λ y → x 0   |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → λ y → 1 y   |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → λ y → x y   |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → 0) 0 |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → y) 0 |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → 0) x |] |] [| null |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → y) x |] |] [| null |]

𝔱 "subst:fvs" [| fvs () [ulc| 0                   |] |] [| pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs () [ulc| 0 1                 |] |] [| pow𝑃 $ map duvar        [0,1]     |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → 0 1           |] |] [| pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → 2) 0   |] |] [| pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → 1) 1   |] |] [| pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → 2) 1   |] |] [| pow𝑃 $ map duvar        [0]       |]
𝔱 "subst:fvs" [| fvs () [ulc| x                   |] |] [| pow𝑃 $ map (znuvar∘var) ["x"]     |]
𝔱 "subst:fvs" [| fvs () [ulc| x y                 |] |] [| pow𝑃 $ map (znuvar∘var) ["x","y"] |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → y             |] |] [| pow𝑃 $ map (znuvar∘var) ["y"]     |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → x) y   |] |] [| pow𝑃 $ map (znuvar∘var) ["y"]     |]
𝔱 "subst:fvs" [| fvs () [ulc| λ x → (λ y → x) x y |] |] [| pow𝑃 $ map (znuvar∘var) ["y"]     |]

𝔱 "subst:metas" [| subst  (bindNSubst (var "x") [ulc| y |]) [ulc| x         |] |] [| Some [ulc| y         |] |]
𝔱 "subst:metas" [| subst  (bindNSubst (var "x") [ulc| y |]) [ulc| λ y → x   |] |] [| Some [ulc| λ y → y:1 |] |]
𝔱 "subst:metas" [| msubst (bindMSubst (var "x") [ulc| y |]) [ulc| x:m       |] |] [| Some [ulc| y         |] |]
𝔱 "subst:metas" [| msubst (bindMSubst (var "x") [ulc| y |]) [ulc| λ y → x:m |] |] [| Some [ulc| λ y → y   |] |]

𝔱 "subst:metas"
  [| msubst (bindMSubst (var "x") [ulc| 0 |]) [ulc| x:m{} (λ → x:m) |] |]
  [| Some [ulc| 0 (λ → 0) |] |]

𝔱 "subst:metas"
  [| msubst (bindMSubst (var "x") [ulc| 0 |]) [ulc| x:m{} (λ → x:m{:0…:∞↦[+1]}) |] |]
  [| Some [ulc| 0 (λ → 1) |] |]

𝔱 "subst:metas"
  [| msubst (bindMSubst (var "x") [ulc| 0 |]) [ulc| x:m{} (λ → x:m{:0↦y,:1…:∞↦[-1]}) |] |]
  [| Some [ulc| 0 (λ → y) |] |]

𝔱 "subst:metas"
  [| msubst (bindMSubst (var "x") [ulc| 1 |]) [ulc| x:m{} (λ → x:m{:0↦y,:1…:∞↦[-1]}) |] |]
  [| Some [ulc| 1 (λ → 0) |] |]

𝔱 "subst:metas"
  [| subst (bindDSubst [ulc| 1 |]) [ulc| χ:m |] |]
  [| Some [ulc| χ:m{:0↦1,:1…:∞↦[-1]} |] |]

𝔣 "zzz:subst:fuzzy"
  [| do e ← fuzzy @(Subst () ULCExpRaw)
        return e
  |]
  [| wfSubst |]
  [| pretty |]

𝔣 "zzz:subst:shrink"
  [| do e ← fuzzy @(Subst () ULCExpRaw)
        return e
  |]
  [| \ e → and $ map wfSubst $ shrink e |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"        $ 𝐯 $ pretty e
       , 𝐤 "shrink e" $ 𝐯 $ pretty $ shrink e
       ]
  |]

𝔣 "zzz:subst:canon"
  [| do e ← fuzzy @ULCExpRaw
        𝓈 ← fuzzy @(Subst () ULCExpRaw)
        return $ e :* 𝓈
  |]
  [| \ (e :* 𝓈) → eqs
       [ map canonULC $ subst 𝓈 e
       , map canonULC $ subst (canonSubst canonULC 𝓈) $ canonULC e
       ]
  |]
  [| \ (e :* 𝓈) → pretty $ concat
       [ 𝐤 "e"       $ 𝐯 $ pretty e
       , 𝐤 "𝓈"       $ 𝐯 $ pretty 𝓈
       , 𝐤 "canon e" $ 𝐯 $ pretty $ canonULC e
       , 𝐤 "canon 𝓈" $ 𝐯 $ pretty $ canonSubst canonULC 𝓈
       , 𝐤 "LHS"     $ 𝐯 $ pretty $ map canonULC $ subst 𝓈 e
       , 𝐤 "RHS"     $ 𝐯 $ pretty $ map canonULC $ subst (canonSubst canonULC 𝓈) $ canonULC e
       ]
  |]
  

𝔣 "zzz:subst:hom:refl"
  [| do e ← fuzzy @ULCExpRaw
        return e
  |]
  [| \ e → eqs
       [ map canonULC $ subst null e
       , map canonULC $ Some e
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"   $ 𝐯 $ pretty e
       , 𝐤 "LHS" $ 𝐯 $ pretty $ map canonULC $ subst null e
       , 𝐤 "RHS" $ 𝐯 $ pretty $ map canonULC $ Some e
       ]
  |]

𝔣 "zzz:subst:lunit:⧺"
  [| do 𝓈 ← fuzzy @(Subst () ULCExpRaw)
        return 𝓈
  |]
  [| \ 𝓈 → eqs
       [ canonSubst canonULC $ null ⧺ 𝓈
       , canonSubst canonULC 𝓈 
       ]
  |]
  [| \ 𝓈 → pretty $ concat
       [ 𝐤 "𝓈"   $ 𝐯 $ pretty 𝓈
       , 𝐤 "LHS" $ 𝐯 $ pretty $ canonSubst canonULC $ null ⧺ 𝓈
       , 𝐤 "RHS" $ 𝐯 $ pretty $ canonSubst canonULC 𝓈
       ]
  |]

𝔣 "zzz:subst:runit:⧺"
  [| do 𝓈 ← fuzzy @(Subst () ULCExpRaw)
        return 𝓈
  |]
  [| \ 𝓈 → eqs
       [ canonSubst canonULC $ 𝓈 ⧺ null
       , canonSubst canonULC 𝓈 
       ]
  |]
  [| \ 𝓈 → pretty $ concat
       [ 𝐤 "𝓈"   $ 𝐯 $ pretty 𝓈
       , 𝐤 "LHS" $ 𝐯 $ pretty $ canonSubst canonULC $ 𝓈 ⧺ null
       , 𝐤 "RHS" $ 𝐯 $ pretty $ canonSubst canonULC 𝓈
       ]
  |]

𝔣 "zzz:subst:hom:⧺"
  [| do 𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
        𝓈₂ ← fuzzy @(Subst () ULCExpRaw)
        e ← fuzzy @ULCExpRaw
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → eqs
       [ map canonULC $ subst (𝓈₁ ⧺ 𝓈₂) e
       , map canonULC $ subst 𝓈₁ *$ subst 𝓈₂ e
       ]
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → pretty $ concat
       [ 𝐤 "𝓈₁"    $ 𝐯 $ pretty 𝓈₁
       , 𝐤 "𝓈₂"    $ 𝐯 $ pretty 𝓈₂
       , 𝐤 "e"     $ 𝐯 $ pretty e
       , 𝐤 "𝓈₁⧺𝓈₂" $ 𝐯 $ pretty $ 𝓈₁ ⧺ 𝓈₂
       , 𝐤 "LHS"   $ 𝐯 $ pretty $ map canonULC $ subst (𝓈₁ ⧺ 𝓈₂) e
       , 𝐤 "RHS"   $ 𝐯 $ pretty $ map canonULC $ subst 𝓈₁ *$ subst 𝓈₂ e
       ]
  |]
     
𝔣 "zzz:subst:assoc:⧺"
  [| do 𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
        𝓈₂ ← fuzzy @(Subst () ULCExpRaw)
        𝓈₃ ← fuzzy @(Subst () ULCExpRaw)
        return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃) → eqs
       [ canonSubst canonULC $ (𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃
       , canonSubst canonULC $ 𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)
       ]
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃) → pretty $ concat
       [ 𝐤 "𝓈₁"    $ 𝐯 $ pretty 𝓈₁
       , 𝐤 "𝓈₂"    $ 𝐯 $ pretty 𝓈₂
       , 𝐤 "𝓈₃"    $ 𝐯 $ pretty 𝓈₃
       , 𝐤 "𝓈₁⧺𝓈₂" $ 𝐯 $ pretty $ 𝓈₁ ⧺ 𝓈₂
       , 𝐤 "𝓈₂⧺𝓈₃" $ 𝐯 $ pretty $ 𝓈₂ ⧺ 𝓈₃
       , 𝐤 "LHS"   $ 𝐯 $ pretty $ canonSubst canonULC $ (𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃
       , 𝐤 "RHS"   $ 𝐯 $ pretty $ canonSubst canonULC $ 𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)
       ]
  |]

𝔣 "zzz:subst:unit:shift"
  [| do i ← fuzzy @ℕ64
        e ← fuzzy @ULCExpRaw
        return $ i :* e
  |]
  [| \ (i :* e) → eqs
       [ canonULC ^$ subst (shiftDSubst i null) e 
       , canonULC ^$ Some e 
       ]
  |]
  [| \ (i :* e) → pretty $ concat
       [ 𝐤 "i"    $ 𝐯 $ pretty i 
       , 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ subst (shiftDSubst i null) e 
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ Some e 
       ]
  |]

𝔣 "zzz:subst:unit:bind∘intro"
  [| do e ← fuzzy @ULCExpRaw
        return e
  |]
  [| \ e  → eqs
       [ canonSubst canonULC $ bindDSubst e ⧺ introDSubst 1
       , null
       ] 
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ bindDSubst e ⧺ introDSubst 1
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ null @(Subst () ULCExpRaw)
       ]
  |]

𝔣 "zzz:subst:commute:intro∘bind"
  [| do e ← fuzzy @ULCExpRaw
        return e
  |]
  [| \ e → eqs
       [ canonSubst canonULC $ introDSubst 1 ⧺ bindDSubst e
       , canonSubst canonULC $ (shiftDSubst 1 $ bindDSubst e) ⧺ introDSubst 1
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ introDSubst 1 ⧺ bindDSubst e
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ (shiftDSubst 1 $ bindDSubst e) ⧺ introDSubst 1
       ]
  |]

𝔣 "zzz:subst:dist:shift/⧺"
  [| do n  ← fuzzy @ℕ64
        𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
        𝓈₂ ← fuzzy @(Subst () ULCExpRaw)
        return $ n :* 𝓈₁ :* 𝓈₂
  |]
  [| \ (n :* 𝓈₁ :* 𝓈₂) → eqs
       [ canonSubst canonULC $ shiftDSubst n $ 𝓈₁ ⧺ 𝓈₂
       , canonSubst canonULC $ shiftDSubst n 𝓈₁ ⧺ shiftDSubst n 𝓈₂
       ]
  |]
  [| \ (n :* 𝓈₁ :* 𝓈₂) → pretty $ concat
       [ 𝐤 "n"    $ 𝐯 $ pretty n
       , 𝐤 "𝓈₁"   $ 𝐯 $ pretty 𝓈₁
       , 𝐤 "𝓈₂"   $ 𝐯 $ pretty 𝓈₂
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ shiftDSubst n $ 𝓈₁ ⧺ 𝓈₂
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ shiftDSubst n 𝓈₁ ⧺ shiftDSubst n 𝓈₂
       ]
  |]

𝔣 "zzz:subst:todbr:idemp"
  [| do fuzzy @ULCExpRaw |]
  [| \ e → eqs 
       [ canonULC ^$ todbr e
       , canonULC ^$ todbr *$ todbr e
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ todbr e
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ todbr *$ todbr e
       ]
  |]

𝔣 "zzz:subst:todbr:∘tonmd"
  [| do fuzzy @ULCExpRaw |]
  [| \ e → eqs
       [ canonULC ^$ todbr e 
       , canonULC ^$ todbr *$ tonmd e
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ todbr e
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ todbr *$ tonmd e
       ]
  |]

𝔣 "zzz:subst:tonmd:idemp"
  [| do fuzzy @ULCExpRaw |]
  [| \ e → eqs
       [ canonULC ^$ tonmd e 
       , canonULC ^$ tonmd *$ tonmd e
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ tonmd e
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ tonmd *$ tonmd e
       ]
  |]

𝔣 "zzz:subst:tonmd:∘todbr"
  [| do fuzzy @ULCExpRaw |]
  [| \ e → eqs
       [ canonULC ^$ tonmd e 
       , canonULC ^$ tonmd *$ todbr e
       ]
  |]
  [| \ e → pretty $ concat
       [ 𝐤 "e"    $ 𝐯 $ pretty e
       , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ tonmd e
       , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ tonmd *$ todbr e
       ]
  |]

buildTests

