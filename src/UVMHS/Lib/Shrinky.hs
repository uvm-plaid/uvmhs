module UVMHS.Lib.Shrinky where

import UVMHS.Core

class Shrinky a where
  shrink ∷ a → 𝐼 a

isoShrink ∷ (a ⇄ b,Shrinky b) ⇒ a → 𝐼 a
isoShrink = isofr ^∘ shrink ∘ isoto

instance Shrinky () where shrink = const null

instance Shrinky 𝔹 where
  shrink = \case
    False → null
    True → single False

instance Shrinky ℕ64 where
  shrink n = if 
    | n ≡ 0     → null
    | n ≡ 1     → single 0
    | otherwise → iter [0,n-1,n-2,n⌿2]

instance Shrinky ℤ64 where
  shrink i = if
    | i ≡ 0     → null
    | i ≡ 1     → single 0
    | i ≡ neg 1 → single 0
    | otherwise →
        let iP = intΩ64 $ zabs $ int i
            iN = neg iP
        in
        concat
          [ iter [0]
          , iter $ if iN ≢ i then [iN] else []
          , iter [iP-1,iN+1,iP-2,iN+2,iP⌿2,iN⌿2]
          ]

instance Shrinky 𝔻 where
  shrink d = if
    | d ≡ 0     → null
    | d ≢ d     → null -- NaN
    | otherwise →
        let dP = abs d
            dN = neg dP
        in
        iter $ pow𝑃 $ filter ((≢) d) $ iter [0,tcate $ dP/2,tcate $ dN/2,dN]
    where
      tcate = truncateDecimals 2

instance (Shrinky a,Shrinky b) ⇒ Shrinky (a,b) where
  shrink (x,y) = concat
    [ do x' ← shrink x ; return (x',y )
    , do y' ← shrink y ; return (x ,y')
    ]

instance (Shrinky a,Shrinky b,Shrinky c) ⇒ Shrinky (a,b,c) where
  shrink (x,y,z) = do
    (x',(y',z')) ← shrink (x,(y,z))
    return (x',y',z')

instance (Shrinky a) ⇒ Shrinky (𝑂 a) where
  shrink = \case
    None → null
    Some a → concat
      [ single None
      , Some ^$ shrink a
      ]

instance (Shrinky a,Shrinky b) ⇒ Shrinky (a ∧ b) where
  shrink (x :* y) = do
    (x',y') ← shrink (x,y) 
    return $ x' :* y'

instance (Shrinky a,Shrinky b) ⇒ Shrinky (a ∨ b) where
  shrink = \case
    Inl x → Inl ^$ shrink x
    Inr y → Inr ^$ shrink y

instance (Shrinky a) ⇒ Shrinky (𝐿 a) where
  shrink = \case
    Nil → null
    x :& xs → concat
      [ single xs
      , do (x',xs') ← shrink (x,xs) ; return $ x' :& xs'
      ]

instance (Shrinky a) ⇒ Shrinky (𝐼 a) where shrink = map iter ∘ shrink ∘ list
instance (Shrinky a) ⇒ Shrinky (𝕍 a) where shrink = map vec ∘ shrink ∘ list

shrinkAssoc ∷ (Shrinky v) ⇒ 𝐿 (k ∧ v) → 𝐼 (𝐿 (k ∧ v))
shrinkAssoc = \case
  Nil → null
  (k :* v) :& kvs → concat
    [ single kvs
    , do v'   ← shrink      v   ; return $ (k :* v') :& kvs
    , do kvs' ← shrinkAssoc kvs ; return $ (k :* v ) :& kvs'
    ]

instance (Ord k,Shrinky v) ⇒ Shrinky (k ⇰ v) where
  shrink = map assoc ∘ shrinkAssoc ∘ list


shrunkR ∷ (Shrinky a) ⇒ (a → 𝔹) → ℕ64 → a → 𝑆 a → ℕ64 ∧ a
shrunkR p =
  let outerLoop n x₀ =
        let loop xs = case un𝑆 xs () of
              None → n :* x₀
              Some (x  :* xs') →
                if p x 
                then outerLoop (n + 1) x $ stream $ shrink x
                else loop xs'
        in loop
  in outerLoop

shrunk ∷ (Shrinky a) ⇒ (a → 𝔹) → a → ℕ64 ∧ a
shrunk p x = shrunkR p 0 x $ stream $ shrink x
