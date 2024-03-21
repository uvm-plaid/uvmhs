module UVMHS.Lib.Pipeline where

import UVMHS.Core

-- | A `Pipeline` is essentially just a list of annotated monadic functions. Its
--   definitions uses a GADT to capture chaining `a → m b` with `b → m c` as a
--   `Pipeline` from `a` to `c`, and where `b` ends up existentially quantified
--   in the chain.
--
--   A `Pipeline` 𝒸 m i a b` imposes constraint `𝒸` on all intermediate result
--   types of monadic computations in the list, annotates each function in the
--   list with a value of type `i`, and ultimately consumes a value of type `a`
--   and produces a value of type `m b`.
data Pipeline 𝒸 m i a b where
  UnitPipeline ∷ Pipeline 𝒸 m i a a
  StepPipeline ∷ (𝒸 b) ⇒ Pipeline 𝒸 m i a b → i → (b → m c) → Pipeline 𝒸 m i a c

runPipeline ∷ (Monad m) ⇒ Pipeline 𝒸 m i a b → a → m b
runPipeline = \case
  UnitPipeline → return
  StepPipeline fs _ f → f *∘ runPipeline fs
