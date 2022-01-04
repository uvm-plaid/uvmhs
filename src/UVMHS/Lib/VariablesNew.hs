module UVMHS.Lib.VariablesNew where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing
import UVMHS.Lib.Annotated
import UVMHS.Lib.Rand
import UVMHS.Lang.ULCD

-- subst(ub,i,vs)(n) =
--   n+i    if  n ≥ ub
--   vs(n)  if  n < ub
data Subst a = Subst
  { substShft ∷ ℕ64
  , substUB ∷ ℕ64
  , substIncr ∷ ℤ64
  , substVals ∷ ℕ64 ⇰ ℕ64 ∨ (ℕ64 ∧ a)
  } deriving (Eq,Ord,Show)
makePrettyRecord ''Subst

class HasSubst a where
  subst ∷ Subst a → a → a

wfSubst ∷ Subst a → 𝔹
wfSubst (Subst _ ub i vs) = and
  -- the key space of the map should be exactly the set {0..ub-1}
  [ keys vs ≡ pow (upTo ub)
  -- i + ub should be non-negative
  , i + intΩ64 ub ≥ 𝕫64 0
  ]

-- subst(id)(n) = n
-- id = (0,0,∅)
idSubst ∷ Subst a
idSubst = Subst
  { substShft = 𝕟64 0
  , substUB = 𝕟64 0
  , substIncr = 𝕫64 0
  , substVals = dø
  }

-- subst(intro)(n) = n+1
-- intro = (0,1,∅)
introSubst ∷ ℕ64 → Subst a
introSubst n = Subst
  { substShft = 𝕟64 0
  , substUB = 𝕟64 0
  , substIncr = intΩ64 n
  , substVals = dø
  }

-- subst(bind(v))(n) =
--   v    if  n = 0
--   n-1  if  n > 0
-- bind(v) = (1,-1,{0↦v})
bindSubst ∷ a → Subst a
bindSubst v = Subst
  { substShft = 𝕟64 0
  , substUB = 𝕟64 1
  , substIncr = neg $ 𝕫64 1
  , substVals = 𝕟64 0 ↦ Inr (𝕟64 0 :* v)
  }

-- subst(wkSubst(𝓈))(n) = 𝓈(n+1)
--          𝓈 = (ub,i,vs)
-- wkSubst(𝓈) = (ub+1,i,{0↦0}∪{(i+1)↦vs(i)|0<i≤ub})
wkSubst ∷ ℕ64 → Subst a → Subst a
wkSubst 𝓃 (Subst s ub i vs) = Subst (𝓃 + s) ub i $ mapOn vs $ \case
  Inl n → Inl $ 𝓃 + n
  Inr (n :* e) → Inr $ (𝓃 + n) :* e

vsubst ∷ Subst a → ℕ64 → ℕ64 ∨ (ℕ64 ∧ a)
vsubst (Subst s ub i vs) n =
  if | n < s → Inl n
     -- n ≥ s
     | n - s < ub → vs ⋕! (n - s)
     -- n ≥ s 
     -- n - s < substUB 𝓈
     | otherwise → Inl $ natΩ64 $ intΩ64 n + i

wkULCDN ∷ ℕ64 → ℕ64 → ULCDExp 𝒸 → ULCDExp 𝒸
wkULCDN 𝓃 𝒾 ė = ULCDExp $ mapOn (unULCDExp ė) $ \case
  Var_ULCD x → Var_ULCD $
    if x < 𝓃 
    then x
    else x + 𝒾
  Lam_ULCD e → Lam_ULCD $ wkULCDN (𝓃 + 1) 𝒾 e
  App_ULCD e₁ e₂ → App_ULCD (wkULCDN 𝓃 𝒾 e₁) $ wkULCDN 𝓃 𝒾 e₂

wkULCD ∷ ℕ64 → ULCDExp 𝒸 → ULCDExp 𝒸
wkULCD = wkULCDN zero

substULCD ∷ Subst (ULCDExp 𝒸) → ULCDExp 𝒸 → ULCDExp 𝒸
substULCD 𝓈 (ULCDExp (𝐴 𝒸 e₀)) = case e₀ of
  Var_ULCD x → case vsubst 𝓈 x of
    Inl x' → ULCDExp $ 𝐴 𝒸 $ Var_ULCD x'
    Inr (𝓃 :* e) → wkULCD 𝓃 e
  Lam_ULCD e → ULCDExp $ 𝐴 𝒸 $ Lam_ULCD $ substULCD (wkSubst 1 𝓈) e
  App_ULCD e₁ e₂ → ULCDExp $ 𝐴 𝒸 $ App_ULCD (substULCD 𝓈 e₁) $ substULCD 𝓈 e₂

instance HasSubst (ULCDExp 𝒸) where subst = substULCD

𝔱 "subst:id" [| substULCD idSubst [ulcd| λ → 0   |] |] [| [ulcd| λ → 0   |] |]
𝔱 "subst:id" [| substULCD idSubst [ulcd| λ → 1   |] |] [| [ulcd| λ → 1   |] |]
𝔱 "subst:id" [| substULCD idSubst [ulcd| λ → 2   |] |] [| [ulcd| λ → 2   |] |]
𝔱 "subst:id" [| substULCD idSubst [ulcd| λ → 0 2 |] |] [| [ulcd| λ → 0 2 |] |]

𝔱 "subst:intro" [| substULCD (introSubst one) [ulcd| λ → 0   |] |] [| [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| substULCD (introSubst one) [ulcd| λ → 1   |] |] [| [ulcd| λ → 2   |] |]
𝔱 "subst:intro" [| substULCD (introSubst one) [ulcd| λ → 2   |] |] [| [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| substULCD (introSubst one) [ulcd| λ → 0 2 |] |] [| [ulcd| λ → 0 3 |] |]

𝔱 "subst:intro" [| substULCD (introSubst $ 𝕟64 2) [ulcd| λ → 0   |] |] [| [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| substULCD (introSubst $ 𝕟64 2) [ulcd| λ → 1   |] |] [| [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| substULCD (introSubst $ 𝕟64 2) [ulcd| λ → 2   |] |] [| [ulcd| λ → 4   |] |]
𝔱 "subst:intro" [| substULCD (introSubst $ 𝕟64 2) [ulcd| λ → 0 2 |] |] [| [ulcd| λ → 0 4 |] |]

𝔱 "subst:bind" [| substULCD (bindSubst [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] 
               [| [ulcd| λ → 0 |] |]
𝔱 "subst:bind" [| substULCD (bindSubst [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] 
               [| [ulcd| λ → 0 |] |]
𝔱 "subst:bind" [| substULCD (bindSubst [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
               [| [ulcd| λ → λ → 0 |] |]
𝔱 "subst:bind" [| substULCD (bindSubst [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] 
               [| [ulcd| λ → λ → 2 |] |]

𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] 
                  [| [ulcd| λ → 0 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] 
                  [| [ulcd| λ → 0 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
                  [| [ulcd| λ → 1 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] 
                  [| [ulcd| λ → 1 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 2 |]) [ulcd| λ → 0 |] |] 
                  [| [ulcd| λ → 0 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 2 |]) [ulcd| λ → 1 |] |] 
                  [| [ulcd| λ → 1 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 1 |]) [ulcd| λ → 2 |] |] 
                  [| [ulcd| λ → λ → 3 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 2 |]) [ulcd| λ → 2 |] |] 
                  [| [ulcd| λ → λ → 4 |] |]

appendSubst ∷ (HasSubst a) ⇒ Subst a → Subst a → Subst a
appendSubst 𝓈₂@(Subst s₂ ub₂ i₂ _vs₂) (Subst s₁ ub₁ i₁ vs₁) =
  let ub = joins
        [ ub₁
        , natΩ64 (intΩ64 ub₂ - (i₁ ⊓ intΩ64 ub₂)) + ((s₂ ⊔ s₁) - s₁)
        ]
      i = i₁ + i₂
      vs = dict $ concat
        [ mapOn (iter vs₁) $ \ (iᵢ :* iv) → (iᵢ ↦) $ case iv of
            Inl i' → vsubst 𝓈₂ i'
            Inr (n :* v) → Inr $ 𝕟64 0 :* subst (𝓈₂ ⧺ introSubst n) v
        , mapOn (range ub₁ ub) $ \ iᵢ → (iᵢ ↦) $ vsubst 𝓈₂ (natΩ64 $ intΩ64 iᵢ + i₁)
        ]
  in Subst s₁ ub i vs

instance Null (Subst a) where null = idSubst
instance (HasSubst a) ⇒ Append (Subst a) where (⧺) = appendSubst
instance (HasSubst a) ⇒ Monoid (Subst a)

𝔱 "subst:⧺" [| substULCD null                   [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| substULCD (null ⧺ null)          [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| substULCD (wkSubst one null)     [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| substULCD (wkSubst (𝕟64 2) null) [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| substULCD null          [ulcd| λ → 1 |] |] [| [ulcd| λ → 1 |] |]
𝔱 "subst:⧺" [| substULCD (null ⧺ null) [ulcd| λ → 1 |] |] [| [ulcd| λ → 1 |] |]

𝔱 "subst:⧺" [| substULCD (introSubst one)               [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| substULCD (null ⧺ introSubst one ⧺ null) [ulcd| λ → 0 |] |] [| [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| substULCD (introSubst one)               [ulcd| λ → 1 |] |] [| [ulcd| λ → 2 |] |]
𝔱 "subst:⧺" [| substULCD (null ⧺ introSubst one ⧺ null) [ulcd| λ → 1 |] |] [| [ulcd| λ → 2 |] |]

𝔱 "subst:⧺" 
  [| substULCD (bindSubst [ulcd| λ → 0 |])               [ulcd| λ → 1 |] |] [| [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" 
  [| substULCD (null ⧺ bindSubst [ulcd| λ → 0 |] ⧺ null) [ulcd| λ → 1 |] |] [| [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| substULCD (introSubst $ 𝕟64 2)              [ulcd| λ → 1 |] |] [| [ulcd| λ → 3 |] |]
𝔱 "subst:⧺" [| substULCD (introSubst one ⧺ introSubst one) [ulcd| λ → 1 |] |] [| [ulcd| λ → 3 |] |]

𝔱 "subst:⧺" 
  [| substULCD (bindSubst [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
  [| [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" 
  [| substULCD (wkSubst one (bindSubst [ulcd| λ → 0 |]) ⧺ introSubst one) [ulcd| λ → 1 |] |] 
  [| [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" 
  [| substULCD (introSubst one ⧺ bindSubst [ulcd| 1 |]) [ulcd| 0 (λ → 2) |] |] 
  [| [ulcd| 2 (λ → 2) |] |]
𝔱 "subst:⧺" 
  [| substULCD (wkSubst one (bindSubst [ulcd| 1 |]) ⧺ introSubst one) [ulcd| 0 (λ → 2) |] |] 
  [| [ulcd| 2 (λ → 2) |] |]

prandULCDVar ∷ ℕ64 → ℕ64 → State RG ℕ64
prandULCDVar nˢ nᵇ = prandr (𝕟64 0) $ nᵇ + nˢ

prandULCDExp ∷ ℕ64 → ℕ64 → ℕ64 → State RG ULCDExpR
prandULCDExp nˢ nᵇ nᵈ = ULCDExp ∘ 𝐴 () ^$ mjoin $ prwchoose
    [ (𝕟64 2 :*) $ \ () → do
        Var_ULCD ^$ prandULCDVar nˢ nᵇ
    , (nᵈ :*) $ \ () → do
        Lam_ULCD ^$ prandULCDExp nˢ (nᵇ+one) $ nᵈ-one
    , (nᵈ :*) $ \ () → do
        e₁ ← prandULCDExp nˢ nᵇ $ nᵈ-one
        e₂ ← prandULCDExp nˢ nᵇ $ nᵈ-one
        return $ App_ULCD e₁ e₂
    ]

instance Rand ULCDExpR where prand = flip prandULCDExp zero

prandSubst ∷ (Rand a) ⇒ ℕ64 → ℕ64 → State RG (Subst a)
prandSubst nˢ nᵈ = do
  s ← return $ 𝕟64 0
  ub ← prandr zero nˢ
  incr ← prandr (neg $ intΩ64 ub) $ intΩ64 ub
  vals ← dict ^$ mapMOn (upTo ub) $ \ i → do
    x ← prandChoice (const ∘ flip prandULCDVar zero) prand nˢ nᵈ
    return $ i ↦ x
  return $ Subst s ub incr vals

instance (Rand a) ⇒  Rand (Subst a) where prand = prandSubst

𝔣 "zzz:subst:wf" (𝕟64 100) [| randSml @ (Subst ULCDExpR) |] [| wfSubst |]

𝔣 "zzz:subst:⧺:wf" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        return $ 𝓈₁ :* 𝓈₂
  |]
  [| \ (𝓈₁ :* 𝓈₂) → wfSubst (𝓈₁ ⧺ 𝓈₂) |]

𝔣 "zzz:subst:⧺:hom" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        e ← randSml @ ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → 
       substULCD (𝓈₁ ⧺ 𝓈₂) e ≡ substULCD 𝓈₁ (substULCD 𝓈₂ e)
  |]

𝔣 "zzz:subst:⧺:trans" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        𝓈₃ ← randSml @ (Subst ULCDExpR)
        e ← randSml @ ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
       substULCD ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ substULCD (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
  |]

𝔣 "zzz:subst:bind" (𝕟64 100)
  [| do e₁ ← randSml @ ULCDExpR
        e₂ ← randSml @ ULCDExpR
        return $ e₁ :* e₂
  |]
  [| \ (e₁ :* e₂) → 
       substULCD (bindSubst e₁ ⧺ introSubst one) e₂ 
       ≡ 
       e₂
  |]

𝔣 "zzz:subst:commute" (𝕟64 100)
  [| do e₁ ← randSml @ ULCDExpR
        e₂ ← randSml @ ULCDExpR
        return $ e₁ :* e₂
  |]
  [| \ (e₁ :* e₂) → 
       substULCD (introSubst one ⧺ bindSubst e₁) e₂
       ≡ 
       substULCD (wkSubst one (bindSubst e₁) ⧺ introSubst one) e₂
  |]

buildTests
