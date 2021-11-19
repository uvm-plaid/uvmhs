module UVMHS.Lib.VariablesNew where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Testing

import qualified Prelude as HS

data Subst a = Subst
  { substLB ∷ ℕ64
  , substUB ∷ ℕ64
  , substIncr ∷ ℤ64
  , substVals ∷ ℕ64 ⇰ a
  } deriving (Eq,Ord,Show)
makePrettyRecord ''Subst

infixl 5 `App`

data ULCalc =
    Var ℕ64
  | Lam 𝕊 ULCalc
  | App ULCalc ULCalc

var ∷ ℕ → ULCalc
var = Var ∘ 𝕟64

-- λ x → x $0
te₁ ∷ ULCalc
te₁ = Lam "x" $ var 0 `App` var 1

-- λ x → (λ y → x y $1 $0) $1 $0
te₂ ∷ ULCalc
te₂ = Lam "x" $ (Lam "y" $ var 1 `App` var 0 `App` var 3 `App` var 2) `App` var 2 `App` var 1

instance Pretty ULCalc where
  pretty = \case
    Var n → ppLit $ "$" ⧺ show𝕊 n
    Lam x e → ppPreSep pLET (ppHorizontal [ppCon "λ",ppBdr x,ppCon "→"]) $ pretty e
    App e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂

prettyNamed ∷ ℕ64 → ℕ64 ⇰ 𝕊 → ULCalc → Doc
prettyNamed 𝓃 𝓈 = \case
  Var n 
    | n < 𝓃 → ppBdr $ 𝓈 ⋕! n
    | otherwise → ppLit $ "$" ⧺ show𝕊 (n - 𝓃)
  Lam x e → ppPreSep pLET (ppHorizontal [ppCon "λ",ppBdr x,ppCon "→"]) $ 
    prettyNamed (succ 𝓃) ((zero ↦ x) ⩌ assoc (map (mapFst succ) $ iter 𝓈)) e
  App e₁ e₂ → ppInfl pAPP (ppSpace one) (prettyNamed 𝓃 𝓈 e₁) $ prettyNamed 𝓃 𝓈 e₂

newtype Named = Named { unNamed ∷ ULCalc }

instance Pretty Named where pretty = prettyNamed zero null ∘ unNamed

wkSubst ∷ Subst a → Subst a
wkSubst 𝓈 =
  let Subst lb ub incr vals = 𝓈
  in Subst (succ lb) (succ ub) incr $ assoc $ map (mapFst succ) $ iter vals

substVarN ∷ ℕ64 → Subst ℕ64 → ℕ64 → ℕ64
substVarN 𝓃 𝓈 n =
  if | n < substLB 𝓈 → n
     | n < substUB 𝓈 → 𝓃 + substVals 𝓈 ⋕! n
     {- n ≥ substUB -} 
     | otherwise → natΩ64 $ intΩ64 n + substIncr 𝓈

substN ∷ ℕ64 → Subst ℕ64 → ULCalc → ULCalc
substN 𝓃 𝓈 = \case
  Var n → Var $ substVarN 𝓃 𝓈 n
  Lam x e → Lam x $ substN (succ 𝓃) (wkSubst 𝓈) e
  App e₁ e₂ → App (substN 𝓃 𝓈 e₁) $ substN 𝓃 𝓈 e₂

subst ∷ Subst ℕ64 → ULCalc → ULCalc
subst = substN zero
  
idSubst ∷ Subst a
idSubst = Subst
  { substLB = 𝕟64 0
  , substUB = 𝕟64 0
  , substIncr = 𝕫64 0
  , substVals = null
  }

introSubst ∷ Subst a
introSubst = Subst
  { substLB = 𝕟64 0
  , substUB = 𝕟64 0
  , substIncr = 𝕫64 1
  , substVals = null
  }

bindSubst ∷ a → Subst a
bindSubst x = Subst
  { substLB = 𝕟64 0
  , substUB = 𝕟64 0
  , substIncr = neg $ 𝕫64 1
  , substVals = 𝕟64 0 ↦ x
  }

range ∷ (Ord n,Plus n,One n) ⇒ n → n → 𝐼 n
range lb₀ ub = 𝐼 HS.$ \ f → flip $ \ 𝓀 → 
  let loop lb i = 
        if lb > ub 
        then 
          𝓀 i
        else 
          f lb i $ \ i' →
          loop (succ lb) i'
  in loop lb₀
  

(⎊) ∷ Subst ℕ64 → Subst ℕ64 → Subst ℕ64
𝓈₂ ⎊ 𝓈₁ =
  let Subst lb₁ ub₁ incr₁ vals₁ = 𝓈₁
      Subst lb₂ ub₂ incr₂ vals₂ = 𝓈₂
  in
  let _ = pptrace lb₁ in
  let _ = pptrace lb₂ in
  let _ = pptrace ub₁ in
  let _ = pptrace incr₁ in
  let lb₃ = natΩ64 $ intΩ64 lb₁ ⊓ (intΩ64 lb₂ + intΩ64 ub₁ + incr₁)
      ub₃ = natΩ64 $ intΩ64 ub₁ ⊔ (intΩ64 ub₂ - incr₁)
      incr₃ = incr₁ + incr₂
      vals₃ = dict
        [ map (substVarN zero 𝓈₂) vals₁
        , dict $ mapOn (range (natΩ64 $ intΩ64 ub₁ + incr₁) (ub₂ - one)) $ \ i →
            (natΩ64 $ intΩ64 i - incr₁) ↦ vals₂ ⋕! i
        ]
  in Subst  lb₃ ub₃ incr₃ vals₃

𝓈bb ∷ Subst ℕ64
𝓈bb = Subst
  { substLB = 𝕟64 0
  , substUB = 𝕟64 2
  , substIncr = neg $ 𝕫64 2
  , substVals = dict
      [ 𝕟64 0 ↦ 𝕟64 99
      , 𝕟64 1 ↦ 𝕟64 100
      ]
  }

𝔱 "var-new" [| 1 |] [| 1 |]
𝔱 "var-new" [| 1 |] [| 1 |]
𝔱 "var-new" [| 1 |] [| 1 |]

buildTests

-- subst ∷ Subst a → ULCalc → ULCalc

-- data 𝕏 = 𝕏
--   { 𝕩name ∷ 𝕊
--   , 𝕩mark ∷ 𝑂 ℕ64
--   } deriving (Eq,Ord,Show)
-- makeLenses ''𝕏
-- 
-- var ∷ 𝕊 → 𝕏
-- var x = 𝕏 x None
-- 
-- instance Pretty 𝕏 where
--   pretty (𝕏 x nO) = concat
--     [ ppString x
--     , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
--     ]
-- 
-- cpName ∷ CParser TokenBasic 𝕏
-- cpName = var ^$ cpShaped $ view nameTBasicL
-- 
-- cpNameWS ∷ CParser TokenWSBasic 𝕏
-- cpNameWS = var ^$ cpShaped $ view nameTWSBasicL
-- 
-- -----------------------------------------
-- -- LOCALLY NAMELESS WITH SHIFTED NAMES --
-- -----------------------------------------
-- 
-- data 𝕐 =
--     GlobalVar 𝕏
--   | NamedVar 𝕏 ℕ64
--   | NamelessVar ℕ64
--   deriving (Eq,Ord,Show)
-- makePrisms ''𝕐
-- 
-- gvar ∷ 𝕏 → 𝕐
-- gvar = GlobalVar
-- 
-- nvar ∷ 𝕏 → 𝕐
-- nvar x = NamedVar x zero
-- 
-- gvarL ∷ 𝕐 ⌲ 𝕏
-- gvarL = prism gvar $ \case
--   GlobalVar x → return x
--   _ → abort
-- 
-- nvarL ∷ 𝕐 ⌲ 𝕏
-- nvarL = prism nvar $ \case
--   NamedVar x n | n ≡ zero → return x
--   _ → abort
-- 
-- instance Pretty 𝕐 where
--   pretty = \case
--     GlobalVar x → pretty x
--     NamedVar x n → concat
--       [ pretty x
--       , if n ≡ zero then null else concat [ppPun "@",ppPun $ show𝕊 n]
--       ]
--     NamelessVar n → concat [ppPun "!",ppString $ show𝕊 n]
-- 
-- data Subst a = Subst
--   { globalSubs ∷ 𝕏 ⇰ a
--   , namedSubs ∷ 𝕏 ⇰ ℕ64 ∧ (ℕ64 ⇰ a)
--   , namelessSubs ∷ ℕ64 ⇰ a
--   , namelessShift ∷ 𝔹 ∧ ℕ64
--   }
-- 
-- class FromVar s a | a → s where
--   frvar ∷ 𝑃 SrcCxt → s → 𝕐 → 𝑂 a
-- 
-- nullSubst ∷ Subst a
-- nullSubst = Subst null null null null
-- 
-- applySubstVar ∷ (𝕐 → 𝑂 a) → Subst a → 𝕐 → 𝑂 a
-- applySubstVar mkvar (Subst g𝓈 n𝓈 i𝓈 (sd :* sn)) y =
--   let shft = 
--         if sd
--         then (+) sn
--         else (-) sn
--   in tries
--     [ do x ← view globalVarL y
--          g𝓈 ⋕? x
--     , do x :* n ← view namedVarL y
--          mn :* nes ← n𝓈 ⋕? x
--          if n ≤ mn
--          then return $ nes ⋕! n
--          else mkvar $ NamedVar x $ n - mn
--     , do n ← view namelessVarL y
--          tries
--            [ i𝓈 ⋕? n
--            , mkvar $ NamelessVar $ shft n
--            ]
--     , mkvar y
--     ]
