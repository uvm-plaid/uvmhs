module UVMHS.Lib.DTree where

import UVMHS.Core
import UVMHS.Lib.Pretty

data DTree a = DTree 
  { dtreeValues ∷ 𝐼 a
  , dtreeNested ∷ 𝐼 (𝕊 ∧ DTree a)
  } deriving (Show)

instance Null (DTree a) where 
  null = DTree null null
instance Append (DTree a) where 
  DTree m₁ n₁ ⧺ DTree m₂ n₂ = DTree (m₁ ⧺ m₂) $ n₁ ⧺ n₂
instance Monoid (DTree a)

instance Eps (DTree a) where 
  eps = DTree null null
instance Seq (DTree a) where
  DTree v₁ n₁ ▷ DTree v₂ n₂
    | isEmpty $ list n₁ = DTree (v₁ ⧺ v₂) n₂
    | otherwise = DTree v₁ $ map (mapSnd (▷ DTree v₂ n₂)) n₁
instance Seqoid (DTree a)

dtk ∷ 𝕊 → DTree a → DTree a
dtk s x = DTree null $ single $ s :* x

dtv ∷ a → DTree a
dtv x = DTree (single x) null

instance (Pretty a) ⇒ Pretty (DTree a) where
  pretty (DTree v n) = ppVertical $ concat
    [ map pretty v
    , mapOn n $ \ (k :* v') → ppHorizontal
        [ ppFG teal $ ppBD $ ppString k
        , ppGA $ pretty v'
        ]
    ]
