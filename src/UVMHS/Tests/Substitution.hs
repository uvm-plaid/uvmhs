module UVMHS.Tests.Substitution (g__TESTS__UVMHS__Tests__Substitution) where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Pretty
import UVMHS.Lib.Shrinky
import UVMHS.Lib.Testing
import UVMHS.Lib.TreeNested

import UVMHS.Lib.Substitution.Name
import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Substy
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Var

import UVMHS.Lang.ULC

testSection "subst:pretty"

test [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
     [| "{}" |]
test [| ppRenderNoFmtWide $ pretty $ SubstScoped 1 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
     [| "{•:0…•:0↦[≡]}" |]
test [| ppRenderNoFmtWide $ pretty $ SubstScoped 2 (id @(𝕍 (SSubstElem () ())) $ null) 0 |] 
     [| "{•:0…•:1↦[≡]}" |]
test [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) 1 |] 
     [| "{•:0…•:∞↦[+1]}" |]
test [| ppRenderNoFmtWide $ pretty $ SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ null) $ neg 1 |] 
     [| "{•:0…•:∞↦[-1]}" |]
test [| ppRenderNoFmtWide $ pretty $ 
          SubstScoped 0 (id @(𝕍 (SSubstElem () ())) $ vec [Trm_SSE $ SubstElem null $ Some ()]) 0 
     |] 
     [| "{•:0↦()}" |]
test [| ppRenderNoFmtWide $ pretty $ 
          SubstScoped 1 (id @(𝕍 (SSubstElem () ())) $ vec [Trm_SSE $ SubstElem null $ Some ()]) 3 
     |] 
     [| "{•:0…•:0↦[≡],•:1↦(),•:2…•:∞↦[+3]}" |]

testSection "subst:parse"

test [| [ulc| χ:m{} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") null |]
test [| [ulc| χ:m{•:0…•:0↦[≡]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* D_SName) $ SubstScoped 1 null 0 
     |]
test [| [ulc| χ:m{x:0…x:0↦[≡]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ SubstScoped 1 null 0 
     |]
test [| [ulc| χ:m{x:0…x:1↦[≡]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ SubstScoped 2 null 0 
     |]
test [| [ulc| χ:m{x:0↦•:0} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ 
            let es = vec [Trm_SSE $ SubstElem null $ Some [ulc|•:0|]]
            in SubstScoped 0 es 0 
     |]
test [| [ulc| χ:m{x:0…x:∞↦[≡]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ SubstScoped 0 null 0 
     |]
test [| [ulc| χ:m{x:0…x:∞↦[+1]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ SubstScoped 0 null 1
     |]
test [| [ulc| χ:m{x:0…x:0↦[≡],x:1↦•:0,x:2…x:∞↦[+1]} |] |] 
     [| ULCExp $ 𝐴 null $ Var_ULC $ M_UVar $ flip MVar (mkName "χ") $ Subst $ SubstSpaced null $ 
          (↦) (() :* N_SName (mkName "x")) $ 
            let es = vec [Trm_SSE $ SubstElem null $ Some [ulc|•:0|]]
            in SubstScoped 1 es 1 
     |]

testSection "subst:canon"

test [| canonULC [ulc| χ:m{}                        |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0…x:0↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0…x:1↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0…x:∞↦[≡]}             |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0…x:1↦[≡],x:2…x:∞↦[≡]} |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0↦x:0}                 |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0↦x:0,x:1↦x:1}         |] |] [| [ulc| χ:m{}             |] |]
test [| canonULC [ulc| χ:m{x:0↦x:1,x:1…x:∞↦[+1]}    |] |] [| [ulc| χ:m{x:0…x:∞↦[+1]} |] |]
test [| canonULC [ulc| χ:m{x:0…x:1↦[≡],x:2↦x:2,x:3↦x:3,x:4↦(λ→•:0),x:5↦x:6,x:6↦x:7,x:7…x:∞↦[+1]} |] |] 
     [|          [ulc| χ:m{x:0…x:3↦[≡],x:4↦(λ→•:0),x:5…x:∞↦[+1]} |] |]

-- basic --

testSection "subst:id"

test [| subst null [ulc| λ → •:0     |] |] [| Some [ulc| λ → •:0     |] |]
test [| subst null [ulc| λ → •:1     |] |] [| Some [ulc| λ → •:1     |] |]
test [| subst null [ulc| λ → •:2     |] |] [| Some [ulc| λ → •:2     |] |]
test [| subst null [ulc| λ → •:0 •:2 |] |] [| Some [ulc| λ → •:0 •:2 |] |]

testSection "subst:intro"

test [| subst (dintroSubst () 1) [ulc| λ → •:0     |] |] [| Some [ulc| λ → •:0     |] |]
test [| subst (dintroSubst () 1) [ulc| λ → •:1     |] |] [| Some [ulc| λ → •:2     |] |]
test [| subst (dintroSubst () 1) [ulc| λ → •:2     |] |] [| Some [ulc| λ → •:3     |] |]
test [| subst (dintroSubst () 1) [ulc| λ → •:0 •:2 |] |] [| Some [ulc| λ → •:0 •:3 |] |]

testSection "subst:intro"

test [| subst (dintroSubst () 2) [ulc| λ → •:0     |] |] [| Some [ulc| λ → •:0     |] |]
test [| subst (dintroSubst () 2) [ulc| λ → •:1     |] |] [| Some [ulc| λ → •:3     |] |]
test [| subst (dintroSubst () 2) [ulc| λ → •:2     |] |] [| Some [ulc| λ → •:4     |] |]
test [| subst (dintroSubst () 2) [ulc| λ → •:0 •:2 |] |] [| Some [ulc| λ → •:0 •:4 |] |]

testSection "subst:bind"

test [| subst (dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0     |] |]
test [| subst (dbindSubst () [ulc| λ → •:1 |]) [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0     |] |]
test [| subst (dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:1 |] |] [| Some [ulc| λ → λ → •:0 |] |]
test [| subst (dbindSubst () [ulc| λ → •:1 |]) [ulc| λ → •:1 |] |] [| Some [ulc| λ → λ → •:2 |] |]

testSection "subst:shift"

test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:0 |] |]
     [| Some [ulc| λ → •:0 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:1 |]) [ulc| λ → •:0 |] |]
     [| Some [ulc| λ → •:0 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → •:1 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:1 |]) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → •:1 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:2 |]) [ulc| λ → •:0 |] |]
     [| Some [ulc| λ → •:0 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:2 |]) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → •:1 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:1 |]) [ulc| λ → •:2 |] |]
     [| Some [ulc| λ → λ → •:3 |] |]
test [| subst (dshiftSubst () 1 $ dbindSubst () [ulc| λ → •:2 |]) [ulc| λ → •:2 |] |]
     [| Some [ulc| λ → λ → •:4 |] |]

-- append --

testSection "subst:⧺"

test [| subst null                    [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]
test [| subst (null ⧺ null)           [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]
test [| subst (dshiftSubst () 1 null) [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]
test [| subst (dshiftSubst () 2 null) [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]

test [| subst null          [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:1 |] |]
test [| subst (null ⧺ null) [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:1 |] |]

test [| subst (dintroSubst () 1)               [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]
test [| subst (null ⧺ dintroSubst () 1 ⧺ null) [ulc| λ → •:0 |] |] [| Some [ulc| λ → •:0 |] |]

test [| subst (dintroSubst () 1)               [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:2 |] |]
test [| subst (null ⧺ dintroSubst () 1 ⧺ null) [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:2 |] |]

test [| subst (dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → λ → •:0 |] |]
test [| subst (null ⧺ dbindSubst () [ulc| λ → •:0 |] ⧺ null) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → λ → •:0 |] |]

test [| subst (dintroSubst () 2)                    [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:3 |] |]
test [| subst (dintroSubst () 1 ⧺ dintroSubst () 1) [ulc| λ → •:1 |] |] [| Some [ulc| λ → •:3 |] |]

test [| subst (dbindSubst () [ulc| λ → •:0 |]) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → λ → •:0 |] |]
test [| subst (dshiftSubst () 1 (dbindSubst () [ulc| λ → •:0 |]) ⧺ dintroSubst () 1) [ulc| λ → •:1 |] |]
     [| Some [ulc| λ → λ → •:0 |] |]

test [| subst (dintroSubst () 1 ⧺ dbindSubst () [ulc| •:1 |]) [ulc| •:0 (λ → •:2) |] |]
     [| Some [ulc| •:2 (λ → •:2) |] |]
test [| subst (dshiftSubst () 1 (dbindSubst () [ulc| •:1 |]) ⧺ dintroSubst () 1) [ulc| •:0 (λ → •:2) |] |]
     [| Some [ulc| •:2 (λ → •:2) |] |]

test [| subst (dintroSubst () 1) *$ subst (dshiftSubst () 1 null) [ulc| •:0 |] |]
     [| subst (dintroSubst () 1 ⧺ dshiftSubst () 1 null)          [ulc| •:0 |] |]

test [| subst (dbindSubst () [ulc| •:1 |]) *$ subst (dshiftSubst () 1 (dintroSubst () 1)) [ulc| •:0 |] |]
     [| subst (dbindSubst () [ulc| •:1 |] ⧺ dshiftSubst () 1 (dintroSubst () 1))          [ulc| •:0 |] |]

test [| subst (dshiftSubst () 1 (dbindSubst () [ulc| •:1 |])) *$ subst (dshiftSubst () 1 null) [ulc| •:1 |] |]
     [| subst (dshiftSubst () 1 (dbindSubst () [ulc| •:1 |]) ⧺ dshiftSubst () 1 null)          [ulc| •:1 |] |]

test [| subst (dshiftSubst () 1 (dbindSubst () [ulc| •:3 |]) ⧺ null) [ulc| •:0 |] |]
     [| subst (dshiftSubst () 1 (dbindSubst () [ulc| •:3 |]))        [ulc| •:0 |] |]

-- de bruijn conversion --

testSection "subst:tobdr"

test [| todbr [ulc| λ x → x                 |] |] [| Some [ulc| λ x → •:0           |] |]
test [| todbr [ulc| λ x → •:0               |] |] [| Some [ulc| λ x → •:0           |] |]
test [| todbr [ulc| λ x → x •:0             |] |] [| Some [ulc| λ x → •:0 •:0       |] |]
test [| todbr [ulc| λ x → x •:0 •:1         |] |] [| Some [ulc| λ x → •:0 •:0 •:1   |] |]
test [| todbr [ulc| λ x → x •:0 y           |] |] [| Some [ulc| λ x → •:0 •:0 y     |] |]
test [| todbr [ulc| λ x → x •:0 •:1 y       |] |] [| Some [ulc| λ x → •:0 •:0 •:1 y |] |]

test [| todbr [ulc| λ y → λ x → x           |] |] [| Some [ulc| λ y → λ x → •:0             |] |]
test [| todbr [ulc| λ y → λ x → •:0         |] |] [| Some [ulc| λ y → λ x → •:0             |] |]
test [| todbr [ulc| λ y → λ x → x •:0       |] |] [| Some [ulc| λ y → λ x → •:0 •:0         |] |]
test [| todbr [ulc| λ y → λ x → x •:0 •:1   |] |] [| Some [ulc| λ y → λ x → •:0 •:0 •:1     |] |]
test [| todbr [ulc| λ y → λ x → x •:0 y     |] |] [| Some [ulc| λ y → λ x → •:0 •:0 •:1     |] |]
test [| todbr [ulc| λ y → λ x → x •:0 •:1 y |] |] [| Some [ulc| λ y → λ x → •:0 •:0 •:1 •:1 |] |]

testSection "subst:tonmd"

test [| tonmd [ulc| λ x → x                |] |] [| Some [ulc| λ x → x         |] |]
test [| tonmd [ulc| λ x → •:0              |] |] [| Some [ulc| λ x → x         |] |]
test [| tonmd [ulc| λ x → x •:0            |] |] [| Some [ulc| λ x → x x       |] |]
test [| tonmd [ulc| λ x → x •:0 •:1        |] |] [| Some [ulc| λ x → x x •:1   |] |]
test [| tonmd [ulc| λ x → x •:0 y          |] |] [| Some [ulc| λ x → x x y     |] |]
test [| tonmd [ulc| λ x → x •:0 •:1 y      |] |] [| Some [ulc| λ x → x x •:1 y |] |]

test [| tonmd [ulc| λ y → λ x → x           |] |] [| Some [ulc| λ y → λ x → x       |] |]
test [| tonmd [ulc| λ y → λ x → •:0         |] |] [| Some [ulc| λ y → λ x → x       |] |]
test [| tonmd [ulc| λ y → λ x → x •:0       |] |] [| Some [ulc| λ y → λ x → x x     |] |]
test [| tonmd [ulc| λ y → λ x → x •:0 •:1   |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
test [| tonmd [ulc| λ y → λ x → x •:0 y     |] |] [| Some [ulc| λ y → λ x → x x y   |] |]
test [| tonmd [ulc| λ y → λ x → x •:0 •:1 y |] |] [| Some [ulc| λ y → λ x → x x y y |] |]

testSection "subst:fvs"

test [| fvs () [ulc| λ x → •:0             |] |] [| null |]
test [| fvs () [ulc| λ x → x               |] |] [| null |]
test [| fvs () [ulc| λ x → λ y → •:1 •:0   |] |] [| null |]
test [| fvs () [ulc| λ x → λ y → x •:0     |] |] [| null |]
test [| fvs () [ulc| λ x → λ y → •:1 y     |] |] [| null |]
test [| fvs () [ulc| λ x → λ y → x y       |] |] [| null |]
test [| fvs () [ulc| λ x → (λ y → •:0) •:0 |] |] [| null |]
test [| fvs () [ulc| λ x → (λ y → y) •:0   |] |] [| null |]
test [| fvs () [ulc| λ x → (λ y → •:0) x   |] |] [| null |]
test [| fvs () [ulc| λ x → (λ y → y) x     |] |] [| null |]

testSection "subst:fvs"

test [| fvs () [ulc| •:0                   |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0]   |]
test [| fvs () [ulc| •:0 •:1               |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0,1] |]
test [| fvs () [ulc| λ x → •:0 •:1         |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0]   |]
test [| fvs () [ulc| λ x → (λ y → •:2) •:0 |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0]   |]
test [| fvs () [ulc| λ x → (λ y → •:1) •:1 |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0]   |]
test [| fvs () [ulc| λ x → (λ y → •:2) •:1 |] |] [| pow𝑃 $ map (D_UVar∘DVar) [0]   |]
test [| fvs () [ulc| x                     |] |] [| pow𝑃 $ map (uvar_Name∘mkName) ["x"]     |]
test [| fvs () [ulc| x y                   |] |] [| pow𝑃 $ map (uvar_Name∘mkName) ["x","y"] |]
test [| fvs () [ulc| λ x → y               |] |] [| pow𝑃 $ map (uvar_Name∘mkName) ["y"]     |]
test [| fvs () [ulc| λ x → (λ y → x) y     |] |] [| pow𝑃 $ map (uvar_Name∘mkName) ["y"]     |]
test [| fvs () [ulc| λ x → (λ y → x) x y   |] |] [| pow𝑃 $ map (uvar_Name∘mkName) ["y"]     |]

testSection "subst:metas"

test [| subst  (nbindSubst () (mkName "x") [ulc| y |]) [ulc| x         |] |] [| Some [ulc| y         |] |]
test [| subst  (nbindSubst () (mkName "x") [ulc| y |]) [ulc| λ y → x   |] |] [| Some [ulc| λ y → y:1 |] |]
test [| msubst (mbindSubst () (mkName "x") [ulc| y |]) [ulc| x:m       |] |] [| Some [ulc| y         |] |]
test [| msubst (mbindSubst () (mkName "x") [ulc| y |]) [ulc| λ y → x:m |] |] [| Some [ulc| λ y → y   |] |]

test [| msubst (mbindSubst () (mkName "x") [ulc| •:0 |]) [ulc| x:m{} (λ → x:m) |] |]
     [| Some [ulc| •:0 (λ → •:0) |] |]

test [| msubst (mbindSubst () (mkName "x") [ulc| •:0 |]) [ulc| x:m{} (λ → x:m{•:0…•:∞↦[+1]}) |] |]
     [| Some [ulc| •:0 (λ → •:1) |] |]

test [| msubst (mbindSubst () (mkName "x") [ulc| •:0 |]) [ulc| x:m{} (λ → x:m{•:0↦y,•:1…•:∞↦[-1]}) |] |]
     [| Some [ulc| •:0 (λ → y) |] |]

test [| msubst (mbindSubst () (mkName "x") [ulc| •:1 |]) [ulc| x:m{} (λ → x:m{•:0↦y,•:1…•:∞↦[-1]}) |] |]
     [| Some [ulc| •:1 (λ → •:0) |] |]

test [| subst (dbindSubst () [ulc| •:1 |]) [ulc| χ:m |] |]
     [| Some [ulc| χ:m{•:0↦•:1,•:1…•:∞↦[-1]} |] |]

testSection "subst:fuzzy"

fuzz [| do e ← fuzzy @(Subst () ULCExpRaw)
           return e
     |]
     [| wfSubst |]
     [| pretty |]

testSection "subst:shrink"

fuzz [| do e ← fuzzy @(Subst () ULCExpRaw)
           return e
     |]
     [| \ e → and $ map wfSubst $ shrink e |]
     [| \ e → pretty $ concat
          [ 𝐤 "e"        $ 𝐯 $ pretty e
          , 𝐤 "shrink e" $ 𝐯 $ pretty $ shrink e
          ]
     |]

testSection "subst:canon"

fuzz
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
  
testSection "subst:null:hom"

fuzz [| do e ← fuzzy @ULCExpRaw
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

testSection "subst:⧺:lunit"

fuzz [| do 𝓈 ← fuzzy @(Subst () ULCExpRaw)
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

testSection "subst:⧺:runit"
   
fuzz [| do 𝓈 ← fuzzy @(Subst () ULCExpRaw)
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

testSection "subst:⧺:hom"

fuzz [| do 𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
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
     
testSection "subst:⧺:assoc"

fuzz [| do 𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
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

testSection "subst:shift:unit"

fuzz [| do i ← fuzzy @ℕ64
           e ← fuzzy @ULCExpRaw
           return $ i :* e
     |]
     [| \ (i :* e) → eqs
          [ canonULC ^$ subst (dshiftSubst () i null) e 
          , canonULC ^$ Some e 
          ]
     |]
     [| \ (i :* e) → pretty $ concat
          [ 𝐤 "i"    $ 𝐯 $ pretty i 
          , 𝐤 "e"    $ 𝐯 $ pretty e
          , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonULC ^$ subst (dshiftSubst () i null) e 
          , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonULC ^$ Some e 
          ]
     |]

testSection "subst:bind∘intro:unit"

fuzz [| do e ← fuzzy @ULCExpRaw
           return e
     |]
     [| \ e  → eqs
          [ canonSubst canonULC $ dbindSubst () e ⧺ dintroSubst () 1
          , null
          ] 
     |]
     [| \ e → pretty $ concat
          [ 𝐤 "e"    $ 𝐯 $ pretty e
          , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ dbindSubst () e ⧺ dintroSubst () 1
          , 𝐤 "RHS"  $ 𝐯 $ pretty $ null @(Subst () ULCExpRaw)
          ]
     |]

testSection "subst:intro∘bind:commute"

fuzz [| do e ← fuzzy @ULCExpRaw
           return e
     |]
     [| \ e → eqs
          [ canonSubst canonULC $ dintroSubst () 1 ⧺ dbindSubst () e
          , canonSubst canonULC $ (dshiftSubst () 1 $ dbindSubst () e) ⧺ dintroSubst () 1
          ]
     |]
     [| \ e → pretty $ concat
          [ 𝐤 "e"    $ 𝐯 $ pretty e
          , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ dintroSubst () 1 ⧺ dbindSubst () e
          , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ (dshiftSubst () 1 $ dbindSubst () e) ⧺ dintroSubst () 1
          ]
     |]

testSection "subst:intro∘bind"

fuzz [| do n  ← fuzzy @ℕ64
           𝓈₁ ← fuzzy @(Subst () ULCExpRaw)
           𝓈₂ ← fuzzy @(Subst () ULCExpRaw)
           return $ n :* 𝓈₁ :* 𝓈₂
     |]
     [| \ (n :* 𝓈₁ :* 𝓈₂) → eqs
          [ canonSubst canonULC $ dshiftSubst () n $ 𝓈₁ ⧺ 𝓈₂
          , canonSubst canonULC $ dshiftSubst () n 𝓈₁ ⧺ dshiftSubst () n 𝓈₂
          ]
     |]
     [| \ (n :* 𝓈₁ :* 𝓈₂) → pretty $ concat
          [ 𝐤 "n"    $ 𝐯 $ pretty n
          , 𝐤 "𝓈₁"   $ 𝐯 $ pretty 𝓈₁
          , 𝐤 "𝓈₂"   $ 𝐯 $ pretty 𝓈₂
          , 𝐤 "LHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ dshiftSubst () n $ 𝓈₁ ⧺ 𝓈₂
          , 𝐤 "RHS"  $ 𝐯 $ pretty $ canonSubst canonULC $ dshiftSubst () n 𝓈₁ ⧺ dshiftSubst () n 𝓈₂
          ]
     |]

testSection "subst:todbr:idemp"

fuzz [| do fuzzy @ULCExpRaw |]
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

testSection "subst:todbr∘tonmd:collapse"

fuzz [| do fuzzy @ULCExpRaw |]
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

testSection "subst:tonmd:idemp"

fuzz [| do fuzzy @ULCExpRaw |]
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

testSection "subst:tonmd∘todbr:collapse"

fuzz [| do fuzzy @ULCExpRaw |]
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
