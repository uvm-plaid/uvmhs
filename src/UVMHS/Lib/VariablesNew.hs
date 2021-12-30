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
  { substUB ∷ ℕ64
  , substIncr ∷ ℤ64
  , substVals ∷ ℕ64 ⇰ ℕ64 ∨ a
  } deriving (Eq,Ord,Show)
makePrettyRecord ''Subst

class HasSubst a where
  subst ∷ Subst a → a → a

wfSubst ∷ Subst a → 𝔹
wfSubst (Subst ub i vs) = and
  -- the key space of the map should be exactly the set {0..ub-1}
  [ keys vs ≡ pow (upTo ub)
  -- i + ub should be non-negative
  , i + intΩ64 ub ≥ 𝕫64 0
  ]

-- subst(id)(n) = n
-- id = (0,0,∅)
idSubst ∷ Subst a
idSubst = Subst
  { substUB = 𝕟64 0
  , substIncr = 𝕫64 0
  , substVals = dø
  }

-- subst(intro)(n) = n+1
-- intro = (0,1,∅)
introSubst ∷ ℕ64 → Subst a
introSubst n = Subst
  { substUB = 𝕟64 0
  , substIncr = intΩ64 n
  , substVals = dø
  }

-- subst(bind(v))(n) =
--   v    if  n = 0
--   n-1  if  n > 0
-- bind(v) = (1,-1,{0↦v})
bindSubst ∷ a → Subst a
bindSubst v = Subst
  { substUB = 𝕟64 1
  , substIncr = neg $ 𝕫64 1
  , substVals = 𝕟64 0 ↦ Inr v
  }

-- subst(wkSubst(𝓈))(n) = 𝓈(n+1)
--          𝓈 = (ub,i,vs)
-- wkSubst(𝓈) = (ub+1,i,{0↦0}∪{(i+1)↦vs(i)|0<i≤ub})
wkSubst ∷ ℕ64 → Subst a → Subst a
wkSubst 𝓃 𝓈 =
  let Subst ub i vs = 𝓈
      ub' = ub + 𝓃
      i'  = i
      vs' = dict $ concat
        [ mapOn (upTo 𝓃) $ \ n → n ↦ Inl n
        , mapOn (iter vs) $ \ (iᵢ :* v) → (iᵢ + 𝓃) ↦ v
        ]
  in Subst ub' i' vs'

vsubstN ∷ ℕ64 → Subst a → ℕ64 → ℕ64 ∨ a
vsubstN 𝓃 𝓈 n =
  if | n < 𝓃 → Inl n
     -- n ≥ 𝓃
     | n - 𝓃 < substUB 𝓈 → mapInl (+ 𝓃) $ substVals 𝓈 ⋕! (n - 𝓃)
     -- n ≥ 𝓃 
     -- n - 𝓃 < substUB 𝓈
     | otherwise → Inl $ natΩ64 $ intΩ64 n + substIncr 𝓈

vsubst ∷ Subst a → ℕ64 → ℕ64 ∨ a
vsubst = vsubstN zero

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

substULCDN ∷ ℕ64 → Subst (ULCDExp 𝒸) → ULCDExp 𝒸 → ULCDExp 𝒸
substULCDN 𝓃 𝓈 (ULCDExp (𝐴 𝒸 e₀)) = case e₀ of
  Var_ULCD x → case vsubstN 𝓃 𝓈 x of
    Inl x' → ULCDExp $ 𝐴 𝒸 $ Var_ULCD x'
    Inr e → wkULCD 𝓃 e
  Lam_ULCD e → ULCDExp $ 𝐴 𝒸 $ Lam_ULCD $ substULCDN (𝓃 + 1) 𝓈 e
  App_ULCD e₁ e₂ → ULCDExp $ 𝐴 𝒸 $ App_ULCD (substULCDN 𝓃 𝓈 e₁) $ substULCDN 𝓃 𝓈 e₂

substULCD ∷ Subst (ULCDExp 𝒸) → ULCDExp 𝒸 → ULCDExp 𝒸
substULCD = substULCDN zero

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
                  [| [ulcd| λ → λ → 2 |] |]
𝔱 "subst:wkSubst" [| substULCD (wkSubst one $ bindSubst [ulcd| λ → 2 |]) [ulcd| λ → 2 |] |] 
                  [| [ulcd| λ → λ → 3 |] |]

appendSubst ∷ (HasSubst a) ⇒ Subst a → Subst a → Subst a
appendSubst 𝓈₂@(Subst ub₂ i₂ _vs₂) (Subst ub₁ i₁ vs₁) =
  let ub = joins
        [ ub₁ 
        , natΩ64 $ intΩ64 ub₂ - (i₁ ⊓ intΩ64 ub₂)
        ]
      i = i₁ + i₂
      vs = dict $ concat
        [ mapOn (iter vs₁) $ \ (iᵢ :* iv) → (iᵢ ↦) $ case iv of
            Inl i' → vsubst 𝓈₂ i'
            Inr v → Inr $ subst 𝓈₂ v
        , mapOn (range ub₁ ub) $ \ iᵢ → (iᵢ ↦) $ vsubst 𝓈₂ (natΩ64 $ intΩ64 iᵢ + i₁)
        ]
  in Subst ub i vs

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
  ub ← prandr zero nˢ
  incr ← prandr (neg $ intΩ64 ub) $ intΩ64 ub
  vals ← dict ^$ mapMOn (upTo ub) $ \ i → do
    x ← prandChoice (const ∘ flip prandULCDVar zero) prand nˢ nᵈ
    return $ i ↦ x
  return $ Subst ub incr vals

instance (Rand a) ⇒  Rand (Subst a) where prand = prandSubst

𝔣 "subst:wf:fuzzy" (𝕟64 100) [| randSml @ (Subst ULCDExpR) |] [| wfSubst |]

𝔣 "subst:⧺:wf:fuzzy" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        return $ 𝓈₁ :* 𝓈₂
  |]
  [| \ (𝓈₁ :* 𝓈₂) → wfSubst (𝓈₁ ⧺ 𝓈₂) |]

𝔣 "subst:⧺:hom:fuzzy" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        e ← randSml @ ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → 
       substULCD (𝓈₁ ⧺ 𝓈₂) e ≡ substULCD 𝓈₁ (substULCD 𝓈₂ e)
  |]

𝔣 "subst:⧺:trans:fuzzy" (𝕟64 100) 
  [| do 𝓈₁ ← randSml @ (Subst ULCDExpR)
        𝓈₂ ← randSml @ (Subst ULCDExpR)
        𝓈₃ ← randSml @ (Subst ULCDExpR)
        e ← randSml @ ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
       substULCD ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ substULCD (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
  |]

buildTests

-- substN ∷ ℕ64 → Subst ℕ64 → ULCalc → ULCalc
-- substN 𝓃 𝓈 = \case
--   Var n → Var $ substVarN 𝓃 𝓈 n
--   Lam x e → Lam x $ substN (succ 𝓃) (wkSubst 𝓈) e
--   App e₁ e₂ → App (substN 𝓃 𝓈 e₁) $ substN 𝓃 𝓈 e₂
-- 
-- subst ∷ Subst ℕ64 → ULCalc → ULCalc
-- subst = substN zero
--   
-- 
-- 
-- infixl 5 `App`
-- 
-- data ULCalc =
--     Var ℕ64
--   | Lam 𝕊 ULCalc
--   | App ULCalc ULCalc
-- 
-- var ∷ ℕ → ULCalc
-- var = Var ∘ 𝕟64
-- 
-- -- λ x → x $0
-- te₁ ∷ ULCalc
-- te₁ = Lam "x" $ var 0 `App` var 1
-- 
-- -- λ x → (λ y → x y $1 $0) $1 $0
-- te₂ ∷ ULCalc
-- te₂ = Lam "x" $ (Lam "y" $ var 1 `App` var 0 `App` var 3 `App` var 2) `App` var 2 `App` var 1
-- 
-- instance Pretty ULCalc where
--   pretty = \case
--     Var n → ppLit $ "$" ⧺ show𝕊 n
--     Lam x e → ppPreSep pLET (ppHorizontal [ppCon "λ",ppBdr x,ppCon "→"]) $ pretty e
--     App e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂
-- 
-- prettyNamed ∷ ℕ64 → ℕ64 ⇰ 𝕊 → ULCalc → Doc
-- prettyNamed 𝓃 𝓈 = \case
--   Var n 
--     | n < 𝓃 → ppBdr $ 𝓈 ⋕! n
--     | otherwise → ppLit $ "$" ⧺ show𝕊 (n - 𝓃)
--   Lam x e → ppPreSep pLET (ppHorizontal [ppCon "λ",ppBdr x,ppCon "→"]) $ 
--     prettyNamed (succ 𝓃) ((zero ↦ x) ⩌ assoc (map (mapFst succ) $ iter 𝓈)) e
--   App e₁ e₂ → ppInfl pAPP (ppSpace one) (prettyNamed 𝓃 𝓈 e₁) $ prettyNamed 𝓃 𝓈 e₂
-- 
-- newtype Named = Named { unNamed ∷ ULCalc }
-- 
-- instance Pretty Named where pretty = prettyNamed zero null ∘ unNamed
-- 
-- range ∷ (Ord n,Plus n,One n) ⇒ n → n → 𝐼 n
-- range lb₀ ub = 𝐼 HS.$ \ f → flip $ \ 𝓀 → 
--   let loop lb i = 
--         if lb > ub 
--         then 
--           𝓀 i
--         else 
--           f lb i $ \ i' →
--           loop (succ lb) i'
--   in loop lb₀
--   
-- 
-- (⎊) ∷ Subst ℕ64 → Subst ℕ64 → Subst ℕ64
-- 𝓈₂ ⎊ 𝓈₁ =
--   let Subst lb₁ ub₁ incr₁ vals₁ = 𝓈₁
--       Subst lb₂ ub₂ incr₂ vals₂ = 𝓈₂
--   in
--   let _ = pptrace lb₁ in
--   let _ = pptrace lb₂ in
--   let _ = pptrace ub₁ in
--   let _ = pptrace incr₁ in
--   let lb₃ = natΩ64 $ intΩ64 lb₁ ⊓ (intΩ64 lb₂ + intΩ64 ub₁ + incr₁)
--       ub₃ = natΩ64 $ intΩ64 ub₁ ⊔ (intΩ64 ub₂ - incr₁)
--       incr₃ = incr₁ + incr₂
--       vals₃ = dict
--         [ map (substVarN zero 𝓈₂) vals₁
--         , dict $ mapOn (range (natΩ64 $ intΩ64 ub₁ + incr₁) (ub₂ - one)) $ \ i →
--             (natΩ64 $ intΩ64 i - incr₁) ↦ vals₂ ⋕! i
--         ]
--   in Subst  lb₃ ub₃ incr₃ vals₃
-- 
-- 𝓈bb ∷ Subst ℕ64
-- 𝓈bb = Subst
--   { substLB = 𝕟64 0
--   , substUB = 𝕟64 2
--   , substIncr = neg $ 𝕫64 2
--   , substVals = dict
--       [ 𝕟64 0 ↦ 𝕟64 99
--       , 𝕟64 1 ↦ 𝕟64 100
--       ]
--   }
-- 
-- 𝔱 "var-new" [| 𝕟 1 |] [| 𝕟 1 |]
-- 𝔱 "var-new" [| 𝕟 1 |] [| 𝕟 1 |]
-- 𝔱 "var-new" [| 𝕟 1 |] [| 𝕟 1 |]
-- 
-- buildTests
-- 
-- -- subst ∷ Subst a → ULCalc → ULCalc
-- 
-- -- data 𝕏 = 𝕏
-- --   { 𝕩name ∷ 𝕊
-- --   , 𝕩mark ∷ 𝑂 ℕ64
-- --   } deriving (Eq,Ord,Show)
-- -- makeLenses ''𝕏
-- -- 
-- -- var ∷ 𝕊 → 𝕏
-- -- var x = 𝕏 x None
-- -- 
-- -- instance Pretty 𝕏 where
-- --   pretty (𝕏 x nO) = concat
-- --     [ ppString x
-- --     , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
-- --     ]
-- -- 
-- -- cpName ∷ CParser TokenBasic 𝕏
-- -- cpName = var ^$ cpShaped $ view nameTBasicL
-- -- 
-- -- cpNameWS ∷ CParser TokenWSBasic 𝕏
-- -- cpNameWS = var ^$ cpShaped $ view nameTWSBasicL
-- -- 
-- -- -----------------------------------------
-- -- -- LOCALLY NAMELESS WITH SHIFTED NAMES --
-- -- -----------------------------------------
-- -- 
-- -- data 𝕐 =
-- --     GlobalVar 𝕏
-- --   | NamedVar 𝕏 ℕ64
-- --   | NamelessVar ℕ64
-- --   deriving (Eq,Ord,Show)
-- -- makePrisms ''𝕐
-- -- 
-- -- gvar ∷ 𝕏 → 𝕐
-- -- gvar = GlobalVar
-- -- 
-- -- nvar ∷ 𝕏 → 𝕐
-- -- nvar x = NamedVar x zero
-- -- 
-- -- gvarL ∷ 𝕐 ⌲ 𝕏
-- -- gvarL = prism gvar $ \case
-- --   GlobalVar x → return x
-- --   _ → abort
-- -- 
-- -- nvarL ∷ 𝕐 ⌲ 𝕏
-- -- nvarL = prism nvar $ \case
-- --   NamedVar x n | n ≡ zero → return x
-- --   _ → abort
-- -- 
-- -- instance Pretty 𝕐 where
-- --   pretty = \case
-- --     GlobalVar x → pretty x
-- --     NamedVar x n → concat
-- --       [ pretty x
-- --       , if n ≡ zero then null else concat [ppPun "@",ppPun $ show𝕊 n]
-- --       ]
-- --     NamelessVar n → concat [ppPun "!",ppString $ show𝕊 n]
-- -- 
-- -- data Subst a = Subst
-- --   { globalSubs ∷ 𝕏 ⇰ a
-- --   , namedSubs ∷ 𝕏 ⇰ ℕ64 ∧ (ℕ64 ⇰ a)
-- --   , namelessSubs ∷ ℕ64 ⇰ a
-- --   , namelessShift ∷ 𝔹 ∧ ℕ64
-- --   }
-- -- 
-- -- class FromVar s a | a → s where
-- --   frvar ∷ 𝑃 SrcCxt → s → 𝕐 → 𝑂 a
-- -- 
-- -- nullSubst ∷ Subst a
-- -- nullSubst = Subst null null null null
-- -- 
-- -- applySubstVar ∷ (𝕐 → 𝑂 a) → Subst a → 𝕐 → 𝑂 a
-- -- applySubstVar mkvar (Subst g𝓈 n𝓈 i𝓈 (sd :* sn)) y =
-- --   let shft = 
-- --         if sd
-- --         then (+) sn
-- --         else (-) sn
-- --   in tries
-- --     [ do x ← view globalVarL y
-- --          g𝓈 ⋕? x
-- --     , do x :* n ← view namedVarL y
-- --          mn :* nes ← n𝓈 ⋕? x
-- --          if n ≤ mn
-- --          then return $ nes ⋕! n
-- --          else mkvar $ NamedVar x $ n - mn
-- --     , do n ← view namelessVarL y
-- --          tries
-- --            [ i𝓈 ⋕? n
-- --            , mkvar $ NamelessVar $ shft n
-- --            ]
-- --     , mkvar y
-- --     ]
