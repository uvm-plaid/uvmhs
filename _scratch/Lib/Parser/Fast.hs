module UVMHS.Lib.Parser.Fast where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.ParserContext

data CResults t a = CResults
  { cResultsFails ∷ 𝔹 -- 𝑃 𝕊
  , cResultsParsers ∷ CParser t a
  }

mapCResultsParsers ∷ (CParser t a → CParser u b) → CResults t a → CResults u b
mapCResultsParsers f (CResults l p) = CResults l $ f p

instance Null (CResults t a) where
  -- {-# INLINE null #-}
  null ∷ CResults t a
  null = CResults False null

instance (Ord t) ⇒ Append (CResults t a) where
  -- {-# INLINE (⧺) #-}
  (⧺) ∷ CResults t a → CResults t a → CResults t a
  CResults f₁ cp₁ ⧺ CResults f₂ cp₂ = CResults (f₁ ⩔ f₂) (cp₁ ⧺ cp₂)

instance (Ord t) ⇒ Monoid (CResults t a)

data CParser t a = CParser
  { cParserNext ∷ t ⇰ CResults t a
  , cParserNextFallback ∷ 𝑂 (CResults t a)
  , cParserFallback ∷ Formats ⇰ (𝐼 t → Parser t a)
  }

instance Return (CParser t) where 
  -- {-# INLINE return #-}
  return ∷ ∀ a. a → CParser t a
  return x = CParser dø None $ null ↦ const (return x)
instance (Ord t) ⇒ Bind (CParser t) where
  -- {-# INLINE (≫=) #-}
  (≫=) ∷ ∀ a b. CParser t a → (a → CParser t b) → CParser t b
  CParser n nf f ≫= k = 
    CParser (map (mapCResultsParsers $ extend k) n) 
            (map (mapCResultsParsers $ extend k) nf) 
            (map (map $ extend $ cparser ∘ k) f)
instance (Ord t) ⇒ Functor (CParser t) where map = mmap
instance (Ord t) ⇒ Monad (CParser t)

instance Null (CParser t a) where
  -- {-# INLINE null #-}
  null ∷ CParser t a
  null = CParser dø None dø

instance (Ord t) ⇒ Append (CParser t a) where
  -- {-# INLINE (⧺) #-}
  (⧺) ∷ CParser t a → CParser t a → CParser t a
  CParser n₁ nf₁ f₁ ⧺ CParser n₂ nf₂ f₂ =
    let nBoth = interWith (⧺) n₁ n₂
        n₁Only = without (keys n₂) n₁
        n₂Only = without (keys n₁) n₂
        n₁Extra = case nf₂ of
          None → id
          Some cp₂ → map (\ cp → cp ⧺ cp₂)
        n₂Extra = case nf₁ of
          None → id
          Some cp₁ → map (\ cp → cp ⧺ cp₁)
        nfBoth = case (nf₁,nf₂) of
          (None,None) → None
          (Some nf,None) → Some nf
          (None,Some nf) → Some nf
          (Some nf₁',Some nf₂') → Some $ nf₁' ⧺ nf₂'
    in CParser (unionsWith (⧺) [nBoth,n₁Extra n₁Only,n₂Extra n₂Only]) nfBoth $ unionWith (\ p₁ p₂ ts → p₁ ts ⎅ p₂ ts) f₁ f₂

instance (Ord t) ⇒ Monoid (CParser t a)

cparser ∷ ∀ t a. (Ord t) ⇒ CParser t a → Parser t a
cparser cp₀ = loop null cp₀
  where
    loop ∷ 𝐼 t → CParser t a → Parser t a
    loop ts cp = tries
      [ do t ← pPluck
           case cParserNext cp ⋕? parserTokenValue t of
             Some (CResults ess cp') → tries
               [ if ess then pFail (parserTokenContext t) (parserTokenSuffix t) else abort -- tries $ mapOn (iter ess) $ \ es → pErr es $ pFail $ parserTokenContext t
               , do pRecord t
                    loop (ts ⧺ single (parserTokenValue t)) cp'
               ]
             None → case cParserNextFallback cp of
               Some (CResults ess cp') → tries
                 [ if ess then pFail (parserTokenContext t) (parserTokenSuffix t) else abort -- tries $ mapOn (iter ess) $ \ es → pErr es $ pFail $ parserTokenContext t
                 , do pRecord t
                      loop (ts ⧺ single (parserTokenValue t)) cp'
                 ]
               None → abort
      , tries $ mapOn (iter $ cParserFallback cp) $ \ (f :* p) → do
            modifyL parserStateContextL $ formatParserContext f
            p ts
      ]

-- {-# INLINE cunit #-}
cunit ∷ Formats → (𝐼 t → a) → CParser t a
cunit fm f = CParser dø None $ fm ↦ return ∘ f

-- {-# INLINE cpWord #-}
cpWord ∷ ∀ s t. (Ord t,Eq t,s ⇄ 𝐼 t) ⇒ Formats → s → CParser t s
cpWord fm ts = foldrOnFrom (isoto ts) (cunit fm isofr) $ \ c cp → 
  CParser (c ↦ CResults False {- pø -} cp) (Some (CResults True {- null -} {- (single $ ppshow ts) -} null)) dø
