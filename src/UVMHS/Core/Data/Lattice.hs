module UVMHS.Core.Data.Lattice where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.Arithmetic ()

-- The supplied function should be monotonic
lfp ∷ (POrd a) ⇒ a → (a → a) → a
lfp i f = loop i 
  where
   loop x =
     let x' = f x
     in case x' ⊑ x of
       True → x 
       False → loop x'

lfpN ∷ (POrd a) ⇒ ℕ → a → (a → a) → a
lfpN n₀ i f = loop n₀ i 
  where
    loop n x
      | n ≡ 0 = x
      | otherwise = 
          let x' = f x
          in case x' ⊑ x of
            True → x 
            False → loop (n - 1) x'

-- The supplied function should be antitonic
gfp ∷ (POrd a) ⇒ a → (a → a) → a
gfp i f = loop i 
  where
    loop x = 
      let x' = f x
      in case x ⊑ x' of
        True → x
        False → loop x'

gfpN ∷ (POrd a) ⇒ ℕ → a → (a → a) → a
gfpN n₀ i f = loop n₀ i 
  where
    loop n x 
      | n ≡ 0 = x
      | otherwise = 
          let x' = f x
          in case x ⊑ x' of
            True → x
            False → loop (n - 1) x'
