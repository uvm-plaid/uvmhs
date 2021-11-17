module UVMHS.Lib.GTree where

import UVMHS.Core
import UVMHS.Lib.Pretty

data GTree a = GTree 
  { dtreeValues ∷ 𝐼 a
  , dtreeNested ∷ 𝕊 ⇰ GTree a
  } deriving (Show)

instance Null (GTree a) where 
  null = GTree null null
instance Append (GTree a) where 
  GTree m₁ n₁ ⧺ GTree m₂ n₂ = GTree (m₁ ⧺ m₂) $ n₁ ⧺ n₂
instance Monoid (GTree a)

instance Eps (GTree a) where 
  eps = GTree null null
instance Seq (GTree a) where
  GTree v₁ n₁ ▷ GTree v₂ n₂
    | isEmpty $ list n₁ = GTree (v₁ ⧺ v₂) n₂
    | otherwise = GTree v₁ $ assoc $ map (mapSnd (▷ GTree v₂ n₂)) $ iter n₁
instance Seqoid (GTree a)

instance Single a (GTree a) where
  single = gtv

foldGTreeWith ∷ (Monoid b) ⇒ (𝐼 a → b) → (𝕊 → b → b) → GTree a → b
foldGTreeWith fₗ fₙ = loop
  where 
    loop (GTree vs sxs) = concat
      [ fₗ vs
      , concat $ mapOn (iter sxs) $ \ (s :* xs) →
          fₙ s $ loop xs
      ]

foldGTreeOn ∷ (Monoid b) ⇒ GTree a → (𝐼 a → b) → (𝕊 → b → b) → b
foldGTreeOn = rotateR foldGTreeWith

gtk ∷ 𝕊 → GTree a → GTree a
gtk s x = GTree null $ single $ s :* x

gtks ∷ 𝐿 𝕊 → GTree a → GTree a
gtks ss x = foldrOnFrom ss x gtk

gtv ∷ a → GTree a
gtv x = GTree (single x) null

instance (Pretty a) ⇒ Pretty (GTree a) where
  pretty (GTree v n) = ppVertical $ concat
    [ map pretty v
    , mapOn (iter n) $ \ (k :* v') → ppHorizontal
        [ ppFG teal $ ppBD $ ppString k
        , ppGA $ pretty v'
        ]
    ]

