# Future

- added to Core.Data.Function:
  
      wrapAB ∷ (c → d) → (a → b) → (b → c) → a → d
      wrapAB h f g = h ∘ g ∘ f
      
      wrapBA ∷ (a → b) → (c → d) → (b → c) → a → d
      wrapBA f h g = h ∘ g ∘ f

- added to Core.Data.Iter:

      zipAllWith ∷ (ToIter a t₁,ToIter b t₂) ⇒ (a → c) → (b → c) → (a → b → c) → t₁ → t₂ → 𝐼 c
      zipAllWith f₁ f₂ f₃ xs ys = iter $ zipAllWith𝑆 f₁ f₂ f₃ (stream xs) $ stream ys
      
      zipAll ∷ (ToIter a t₁,ToIter b t₂) ⇒ t₁ → t₂ → 𝐼 ((a ∨ b) ∨ a ∧ b)
      zipAll = zipAllWith (Inl ∘ Inl) (Inl ∘ Inr) $ Inr ∘∘ (:*)

- added to Core.Data.Stream:

      zipAllWith𝑆 ∷ (a → c) → (b → c) → (a → b → c) → 𝑆 a → 𝑆 b → 𝑆 c
      zipAllWith𝑆 f₁ f₂ f₃ = loop
        where
          loop xs ys = 𝑆 $ \ () → case (un𝑆 xs (),un𝑆 ys ()) of
            (Some (x :* xs'),None           ) → Some (f₁ x   :* map f₁ xs'  )
            (None           ,Some (y :* ys')) → Some (f₂ y   :* map f₂ ys'  )
            (Some (x :* xs'),Some (y :* ys')) → Some (f₃ x y :* loop xs' ys')
            (None           ,None           ) → None

- added to Core.Data.Set:

      extend𝑃 ∷ (Ord b) ⇒ (a → 𝑃 b) → 𝑃 a → 𝑃 b
      extend𝑃 f = pow ∘ extend (iter ∘ f) ∘ iter

- added new module Lib.Virtual:

      class Virtual c r v | v→r,v→c where
        virtualize ∷ r → v
        realize ∷ (c) ⇒ v → r
      
      ground ∷ ∀ c r v. (Virtual c r v,c) ⇒  v → v
      ground = virtualize ∘ realize
      
      instance Virtual (Ord a) (𝑃 a) (𝐼 a) where
        virtualize = iter
        realize = pow
