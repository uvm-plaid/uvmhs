module UVMHS.Lib.TreeNested where

import UVMHS.Core
import UVMHS.Lib.Pretty

data 𝑇A a = 𝑇A
  { vals𝑇A ∷ 𝐼 a
  , nest𝑇A ∷ 𝐼 (𝕊 ∧ 𝑇A a)
  } deriving (Show)

instance Null (𝑇A a) where
  null = 𝑇A null null
instance Append (𝑇A a) where
  𝑇A m₁ n₁ ⧺ 𝑇A m₂ n₂ = 𝑇A (m₁ ⧺ m₂) $ n₁ ⧺ n₂
instance Monoid (𝑇A a)

instance Eps (𝑇A a) where
  eps = 𝑇A null null
instance Seq (𝑇A a) where
  𝑇A v₁ n₁ ▷ 𝑇A v₂ n₂
    | isEmpty n₁ = 𝑇A (v₁ ⧺ v₂) n₂
    | otherwise = 𝑇A v₁ $ map (mapSnd (▷ 𝑇A v₂ n₂)) n₁
instance Seqoid (𝑇A a)

fold𝑇AWith ∷ (Monoid b) ⇒ (𝐼 a → b) → (𝕊 → b → b) → 𝑇A a → b
fold𝑇AWith fₗ fₙ = loop
  where
    loop (𝑇A vs sxs) = concat
      [ fₗ vs
      , concat $ mapOn (iter sxs) $ \ (s :* xs) →
          fₙ s $ loop xs
      ]

fold𝑇AOn ∷ (Monoid b) ⇒ 𝑇A a → (𝐼 a → b) → (𝕊 → b → b) → b
fold𝑇AOn = rotateR fold𝑇AWith

key𝑇A ∷ 𝕊 → 𝑇A a → 𝑇A a
key𝑇A s x = 𝑇A null $ single $ s :* x

val𝑇A ∷ a → 𝑇A a
val𝑇A x = 𝑇A (single x) null

𝐤 ∷ 𝕊 → 𝑇A a → 𝑇A a
𝐤 = key𝑇A

𝐯 ∷ a → 𝑇A a
𝐯 = val𝑇A

keys𝑇A ∷ 𝐿 𝕊 → 𝑇A a → 𝑇A a
keys𝑇A = foldrWithOn key𝑇A

instance (Pretty a) ⇒ Pretty (𝑇A a) where
  pretty (𝑇A v n) = ppVertical $ concat
    [ map (ppGA ∘ pretty) v
    , mapOn n $ \ (k :* v') →
        if csize k < 2
        then 
          ppHorizontal 
            [ ppFG teal $ ppBD $ ppString k
            , ppGA $ pretty v'
            ]
        else
          ppGA $ concat
            [ ppFG teal $ ppBD $ ppString k
            , ppSpaceNewlineIfBreak
            , ppSpaceIfBreak
            , ppSpaceIfBreak
            , ppGA $ pretty v'
            ]
    ]

data 𝑇D a = 𝑇D
  { vals𝑇D ∷ 𝐼 a
  , nest𝑇D ∷ 𝕊 ⇰ 𝑇D a
  } deriving (Show)

instance Null (𝑇D a) where
  null = 𝑇D null null
instance Append (𝑇D a) where
  𝑇D m₁ n₁ ⧺ 𝑇D m₂ n₂ = 𝑇D (m₁ ⧺ m₂) $ n₁ ⧺ n₂
instance Monoid (𝑇D a)

instance Eps (𝑇D a) where
  eps = 𝑇D null null
instance Seq (𝑇D a) where
  𝑇D v₁ n₁ ▷ 𝑇D v₂ n₂
    | isEmpty n₁ = 𝑇D (v₁ ⧺ v₂) n₂
    | otherwise = 𝑇D v₁ $ map (▷ 𝑇D v₂ n₂) n₁
instance Seqoid (𝑇D a)

fold𝑇DWith ∷ (Monoid b) ⇒ (𝐼 a → b) → (𝕊 → b → b) → 𝑇D a → b
fold𝑇DWith fₗ fₙ = loop
  where
    loop (𝑇D vs sxs) = concat
      [ fₗ vs
      , concat $ mapOn (iter sxs) $ \ (s :* xs) →
          fₙ s $ loop xs
      ]

fold𝑇DOn ∷ (Monoid b) ⇒ 𝑇D a → (𝐼 a → b) → (𝕊 → b → b) → b
fold𝑇DOn = rotateR fold𝑇DWith

key𝑇D ∷ 𝕊 → 𝑇D a → 𝑇D a
key𝑇D s x = 𝑇D null $ single $ s :* x

val𝑇D ∷ a → 𝑇D a
val𝑇D x = 𝑇D (single x) null

keys𝑇D ∷ 𝐿 𝕊 → 𝑇D a → 𝑇D a
keys𝑇D = foldrWithOn key𝑇D

instance (Pretty a) ⇒ Pretty (𝑇D a) where
  pretty (𝑇D v n) = ppVertical $ concat
    [ map (ppGA ∘ pretty) v
    , mapOn (iter n) $ \ (k :* v') →
        if csize k < 2
        then 
          ppHorizontal 
            [ ppFG teal $ ppBD $ ppString k
            , ppGA $ pretty v'
            ]
        else
          ppGA $ concat
            [ ppFG teal $ ppBD $ ppString k
            , ppSpaceNewlineIfBreak
            , ppSpaceIfBreak
            , ppSpaceIfBreak
            , ppGA $ pretty v'
            ]
    ]
