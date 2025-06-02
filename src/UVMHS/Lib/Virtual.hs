module UVMHS.Lib.Virtual where

import UVMHS.Core

-- Canonical use for this is when using an iterator type as the implementation
-- of a set monad. You can "ground" the monad at points where you know the type
-- parameter obeys Ord, which would have the effect of removing duplicates.

class Virtual c r v | v→r,v→c where
  virtualize ∷ r → v
  realize ∷ (c) ⇒ v → r

ground ∷ ∀ c r v. (Virtual c r v,c) ⇒  v → v
ground = virtualize ∘ realize

instance Virtual (Ord a) (𝑃 a) (𝐼 a) where
  virtualize = iter
  realize = pow

