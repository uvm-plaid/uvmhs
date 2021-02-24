{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

-- -- state space
-- data SS val lτ dτ = SS
--   { ssCxt ∷ ℕ
--   , ssStore ∷ ℕ
--   , ssStack ∷ ℕ
--   } deriving (Eq, Ord)
-- makePrettyRecord ''SS

-- data Thing = Thing
--   { thing ∷ ℕ
--   , think ∷ ℕ
--   }
-- makePrettyRecord ''Thing

-- data Exp =
--     Var_E 𝕊
--   | Let_E 𝕊 Exp Exp
-- 
-- instance Pretty Exp where
--   pretty = \case
--     Var_E x → ppString x
--     Let_E x e₁ e₂ → concat
--       [ ppForceBreak
--       , ppVertical
--           [ ppKey "let"
--           , concat
--               [ ppSpace $ 𝕟64 2
--               , ppHorizontal
--                   [ ppString x
--                   , ppPun "="
--                   , ppGA $ pretty e₁
--                   ]
--               ]
--           , ppGroup $ pretty e₂
--           ]
--       ]

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"
  -- let e₁ = Let_E "x" (Var_E "y") $ Var_E "x"
  -- -- pprint $ e₁ :* e₁
  -- -- pprint $ ppGroup $ concat
  -- --   [ pretty $ ((ppForceBreak ⧺ pretty 1) :* pretty 2) :* ((ppForceBreak ⧺ pretty 3) :* pretty 4)  
  -- --   ]
  -- pprint $ concat
  --   [ ppSpaceIfBreak
  --   , ppSpaceIfBreak
  --   , ppAlign $ pretty e₁
  --   ]
  -- pprint $ concat
  --   [ ppSpace $ 𝕟64 2
  --   , ppAlign $ pretty e₁
  --   ]
