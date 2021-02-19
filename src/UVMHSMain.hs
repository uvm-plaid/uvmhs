{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

-- state space
data SS val lτ dτ = SS
  { ssCxt ∷ ℕ
  , ssStore ∷ ℕ
  , ssStack ∷ ℕ
  } deriving (Eq, Ord)
makePrettyRecord ''SS

data Thing = Thing
  { thing ∷ ℕ
  , think ∷ ℕ
  }
makePrettyRecord ''Thing

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"
  -- pprint $ ppHeader "ALIGN TEST"
  -- debugShape $ ppVertical
  --   [ ppString "AA" 
  --   , ppString "BBBBBBBB" 
  --   , concat
  --       [ ppString "CCCC"
  --       , ppAlign $ ppVertical
  --           [ ppString "DD"
  --           , ppString "EEEE"
  --           , ppString "FFF"
  --           ]
  --       ]
  --   ]
  -- let eachGA f = do
  --       pprint $ f $ ppAlign ∘ ppGroup
  --       pprint $ f $ ppGroup ∘ ppAlign
  -- eachGA $ \ ppGA →
  --   ppGA $ ppHorizontal
  --     [ ppString "XXX"
  --     , ppGA $ ppVertical
  --         [ ppString "AAA"
  --         , ppGA $ ppHorizontal $ 
  --             [ ppString "BBB"
  --             , ppGA $ ppVertical
  --                 [ ppString "WWW"
  --                 , ppString "VVV"
  --                 ]
  --             , ppGA $ ppVertical
  --                 [ ppString "WWW"
  --                 , ppString "VVV"
  --                 , ppForceBreak
  --                 ]
  --             ]
  --         ]
  --     ]
