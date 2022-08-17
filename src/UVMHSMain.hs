{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  $$(testModules False
    [ "UVMHS.Tests.Core"
    , "UVMHS.Tests.Substitution"
    ])
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"
  -- let 𝓈₂ ∷ Subst 𝕏 (𝔖 ()) ULCDExpSrc
  --     𝓈₂ = 
  --       Subst null $ dict
  --         [ D𝔖() ↦ 
  --             GDSubst 2
  --                     (vec 
  --                       [ Val_GSE null $ const $ return [ulcd|x|]
  --                       ])
  --                     0
  --         , N𝔖() (var "x") ↦
  --             GDSubst 0
  --                     null
  --                     0
  --         ]
  --     𝓈₁ ∷ Subst 𝕏 (𝔖 ()) ULCDExpSrc
  --     𝓈₁ = 
  --       Subst null $ dict
  --         [ N𝔖() (var "x") ↦ 
  --             GDSubst 0
  --                     (vec
  --                       [ Val_GSE null $ const $ return [ulcd|2|]
  --                       ])
  --                     0
  --         ]
  --     e = [ulcd|x|]
  -- pprint e
  -- pprint 𝓈₁
  -- pprint $ subst 𝓈₁ e
  -- pprint $ ppPun "======================================="
  -- pprint $ subst 𝓈₁ e
  -- pprint 𝓈₂
  -- pprint $ subst 𝓈₂ *$ subst 𝓈₁ e
  -- pprint $ ppPun "======================================="
  -- pprint 𝓈₂
  -- pprint 𝓈₁
  -- pprint $ 𝓈₂ ⧺ 𝓈₁
  -- pprint $ ppPun "======================================="
  -- pprint e
  -- pprint $ 𝓈₂ ⧺ 𝓈₁
  -- pprint $ subst (𝓈₂⧺𝓈₁) e
  -- pprint $ ppPun "======================================="
  -- pprint $ 𝓈dshift 1 𝓈₂
  -- pprint $ 𝓈dshift 1 $ 𝓈₂⧺𝓈₁
  -- pprint $ (subst 𝓈₂ *$ subst 𝓈₁ e) ≡ (subst (𝓈₂ ⧺ 𝓈₁) e)
  -- return ()
