# Future

- changes to Core.Data.Function:
  - added:
  
        wrapAB ∷ (c → d) → (a → b) → (b → c) → a → d
        wrapAB h f g = h ∘ g ∘ f
        
        wrapBA ∷ (a → b) → (c → d) → (b → c) → a → d
        wrapBA f h g = h ∘ g ∘ f

- changes to Core.Data.Iter:
  - added:

        zipAllWith ∷ (ToIter a t₁,ToIter b t₂) ⇒ (a → c) → (b → c) → (a → b → c) → t₁ → t₂ → 𝐼 c
        zipAllWith f₁ f₂ f₃ xs ys = iter $ zipAllWith𝑆 f₁ f₂ f₃ (stream xs) $ stream ys
        
        zipAll ∷ (ToIter a t₁,ToIter b t₂) ⇒ t₁ → t₂ → 𝐼 ((a ∨ b) ∨ a ∧ b)
        zipAll = zipAllWith (Inl ∘ Inl) (Inl ∘ Inr) $ Inr ∘∘ (:*)

- changes to Core.Data.Stream:
  - added:

        zipAllWith𝑆 ∷ (a → c) → (b → c) → (a → b → c) → 𝑆 a → 𝑆 b → 𝑆 c
        zipAllWith𝑆 f₁ f₂ f₃ = loop
          where
            loop xs ys = 𝑆 $ \ () → case (un𝑆 xs (),un𝑆 ys ()) of
              (Some (x :* xs'),None           ) → Some (f₁ x   :* map f₁ xs'  )
              (None           ,Some (y :* ys')) → Some (f₂ y   :* map f₂ ys'  )
              (Some (x :* xs'),Some (y :* ys')) → Some (f₃ x y :* loop xs' ys')
              (None           ,None           ) → None

- changes to Core.Data.Set:
  - added:

        extend𝑃 ∷ (Ord b) ⇒ (a → 𝑃 b) → 𝑃 a → 𝑃 b
        extend𝑃 f = pow ∘ extend (iter ∘ f) ∘ iter

- changes to Lib.Parser.CParser.hs
  - rename:
    - `cpNaturalWS` to `cpNatNWS`
    - `cpNat64N` to `cpNatN64`
    - `cpIntegerWS` to `cpIntWS`
  - added:

        cpName ∷ CParser TokenBasic 𝕊
        cpName = cpShaped $ view nameTBasicL
        
        cpNameWS ∷ CParser TokenWSBasic 𝕊
        cpNameWS = cpShaped $ view nameTWSBasicL

        cpNat64NWS ∷ CParser TokenWSBasic ℕ64
        cpNat64NWS = failEff ∘ natO64 *$ cpNatNWS

        cpInt64WS ∷ CParser TokenWSBasic ℤ64
        cpInt64WS = failEff ∘ intO64 *$ cpIntWS

        cpNatWS ∷ CParser TokenWSBasic ℕ
        cpNatWS = failEff ∘ natO *$ cpIntWS

        cpNat64WS ∷ CParser TokenWSBasic ℕ64
        cpNat64WS = failEff ∘ natO64 *$ cpIntWS

        cpCharWS ∷ CParser TokenWSBasic ℂ
        cpCharWS = cpShaped $ view charTWSBasicL

- changes to Lib.Annotated:
  - added:
    
        class HasRaw r a | a → r where
          toRaw ∷ a → r
          frRaw ∷ r → a

        instance (Null e) ⇒ HasRaw a (𝐴 e a) where
          toRaw = aval
          frRaw = 𝐴 null

- new module Lib.Virtual:

      class Virtual c r v | v→r,v→c where
        virtualize ∷ r → v
        realize ∷ (c) ⇒ v → r
      
      ground ∷ ∀ c r v. (Virtual c r v,c) ⇒  v → v
      ground = virtualize ∘ realize
      
      instance Virtual (Ord a) (𝑃 a) (𝐼 a) where
        virtualize = iter
        realize = pow

