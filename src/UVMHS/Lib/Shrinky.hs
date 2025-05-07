module UVMHS.Lib.Shrinky where

import UVMHS.Core

class Shrinky a where
  shrink ∷ a → 𝐼 a

instance (Shrinky a) ⇒ Shrinky (𝑂 a) where
  shrink = \case
    None → null
    Some a → concat
      [ single None
      , Some ^$ shrink a
      ]

instance (Shrinky a) ⇒ Shrinky (𝐿 a) where
  shrink = \case
    Nil → null
    x :& xs → concat
      [ single xs
      , do xs' ← shrink xs ; return $ x  :& xs'
      , do x'  ← shrink x  ; return $ x' :& xs
      ]

instance (Shrinky a) ⇒ Shrinky (𝐼 a) where shrink = map iter ∘ shrink ∘ list
instance (Shrinky a) ⇒ Shrinky (𝕍 a) where shrink = map vec ∘ shrink ∘ list

shrinkAssoc ∷ (Shrinky v) ⇒ 𝐿 (k ∧ v) → 𝐼 (𝐿 (k ∧ v))
shrinkAssoc = \case
  Nil → null
  (k :* v) :& kvs → concat
    [ single kvs
    , do kvs' ← shrinkAssoc kvs ; return $ (k :* v ) :& kvs'
    , do v'   ← shrink      v   ; return $ (k :* v') :& kvs
    ]

instance (Ord k,Shrinky v) ⇒ Shrinky (k ⇰ v) where
  shrink = map assoc ∘ shrinkAssoc ∘ list
