module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

---------------
-- VARIABLES --
---------------

-- simple variables
data 𝕏 = 𝕏
  { 𝕩mark ∷ 𝑂 ℕ64
  , 𝕩name ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

-- marked variables (scoped or meta)
data 𝕐 = 𝕐
  { 𝕪meta ∷ 𝔹
  , 𝕪name ∷ 𝕏
  } deriving (Eq,Ord,Show)
makeLenses ''𝕐

var ∷ 𝕊 → 𝕏
var = 𝕏 None

svar𝕏 ∷ 𝕏 → 𝕐
svar𝕏 = 𝕐 False

mvar𝕏 ∷ 𝕏 → 𝕐
mvar𝕏 = 𝕐 True

svar ∷ 𝕊 → 𝕐
svar = svar𝕏 ∘ var

mvar ∷ 𝕊 → 𝕐
mvar = mvar𝕏 ∘ var

svar𝕏L ∷ 𝕐 ⌲ 𝕏
svar𝕏L = prism svar𝕏 $ \ (𝕐 m x) → if not m then Some x else None

mvar𝕏L ∷ 𝕐 ⌲ 𝕏
mvar𝕏L = prism mvar𝕏 $ \ (𝕐 m x) → if m then Some x else None

instance Pretty 𝕏 where
  pretty (𝕏 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
    ]

instance Pretty 𝕐 where
  pretty (𝕐 m x) = concat
    [ pretty x
    , if not m then null else ppPun "†"
    ]

cpVar ∷ CParser TokenBasic 𝕏
cpVar = var ^$ cpShaped $ view nameTBasicL

cpSVar ∷ CParser TokenBasic 𝕐
cpSVar = svar ^$ cpShaped $ view nameTBasicL

cpMVar ∷ CParser TokenBasic 𝕐
cpMVar = mvar ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

cpSVarWS ∷ CParser TokenWSBasic 𝕐
cpSVarWS = svar ^$ cpShaped $ view nameTWSBasicL

cpMVarWS ∷ CParser TokenWSBasic 𝕐
cpMVarWS = mvar ^$ cpShaped $ view nameTWSBasicL

-------------------------
-- FREE AND BOUND VARS --
-------------------------

data FBV = FBV
  { fbvBound ∷ 𝑃 𝕏
  , fbvFree ∷ 𝑃 𝕐
  }

instance Null FBV where null = FBV null null
instance Append FBV where FBV bv₁ fv₁ ⧺ FBV bv₂ fv₂ = FBV (bv₁ ⧺ bv₂) $ fv₁ ⧺ fv₂
instance Monoid FBV

instance Bot FBV where bot = FBV bot bot
instance Join FBV where FBV bv₁ fv₁ ⊔ FBV bv₂ fv₂ = FBV (bv₁ ⊔ bv₂) $ fv₁ ⊔ fv₂
instance JoinLattice FBV

class HasFBV a where
  fbv ∷ a → FBV

instance HasFBV 𝕏 where fbv x = FBV (single x) null
instance HasFBV 𝕐 where fbv x = FBV null $ single x

bv ∷ (HasFBV a) ⇒ a → 𝑃 𝕏
bv = fbvBound ∘ fbv

fv ∷ (HasFBV a) ⇒ a → 𝑃 𝕐
fv = fbvFree ∘ fbv

scopeFBV ∷ FBV → FBV → FBV
scopeFBV (FBV bv₁ fv₁) (FBV bv₂ fv₂) = 
  let bv' = bv₂
      fv' = joins [fv₁,fv₂ ∖ pow (map svar𝕏 $ iter bv₁)]
  in FBV bv' fv'

class HasSFBV s a | a → s where
  sfbv ∷ a → s ⇰ FBV

sbv ∷ (Ord s,HasSFBV s a) ⇒ s → a → 𝑃 𝕏
sbv s x = ifNone bot $ map fbvBound $ sfbv x ⋕? s

sfv ∷ (Ord s,HasSFBV s a) ⇒ s → a → 𝑃 𝕐
sfv s x = ifNone bot $ map fbvFree $ sfbv x ⋕? s

scopeSFBV ∷ (Ord s) ⇒ s ⇰ FBV → s ⇰ FBV → s ⇰ FBV
scopeSFBV = unionWithD bot scopeFBV

------------------
-- SUBSTITUTION --
------------------

data Subst s a m = Subst
  { substBdr ∷ s ⇰ 𝕏 ⇰ 𝕏
  , substVar ∷ s ⇰ 𝕐 ⇰ 𝕐 ∨ FailT m a
  }
makeLenses ''Subst

data SubstEnv s a b m = SubstEnv
  { substEnvFresh ∷ 𝑂 (m ℕ64)
  , substEnvView  ∷ a → 𝑂 b
  , substEnvSubst ∷ Subst s a m
  }
makeLenses ''SubstEnv

newtype SubstT s e₁ e₂ m a = SubstM { unSubstM ∷ UContT (ReaderT (SubstEnv s e₁ e₂ m) (FailT m)) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadFail
  , MonadReader (SubstEnv s e₁ e₂ m)
  , MonadUCont
  )

instance Transformer (SubstT s e₁ e₂) where lift = SubstM ∘ lift ∘ lift ∘ lift

runSubstT ∷ (Return m) ⇒ SubstEnv s e₁ e₂ m → SubstT s e₁ e₂ m a → m (𝑂 a)
runSubstT γ = unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstM

class Substy s e a | a→s,a→e where
  substy ∷ ∀ e' m. (Monad m) ⇒ a → SubstT s e' e m a

subst ∷ (Substy s e a,Monad m) ⇒ Subst s e m → a → m (𝑂 a)
subst γ = runSubstT (SubstEnv None return γ) ∘ substy

freshen ∷ (Substy s e a,Monad m) ⇒ m ℕ64 → a → m (𝑂 a)
freshen 𝑓M = runSubstT (SubstEnv (Some 𝑓M) return null) ∘ substy

instance Null (Subst s a m) where
  null = Subst null null
instance (Ord s,Monad m,Substy s a a) ⇒ Append (Subst s a m) where
  𝓈₁@(Subst sρ₁ sγ₁) ⧺ Subst sρ₂ sγ₂=
    let sρ₂' = dmapOnWithKey sρ₂ $ \ s → map $ \ x →
          ifNone x $ do
            ρ ← sρ₁ ⋕? s
            ρ ⋕? x
        sγ₂' = dmapOnWithKey sγ₂ $ \ s → map $ \case
          Inl x → ifNone (Inl x) $ do
            γ ← sγ₁ ⋕? s
            γ ⋕? x
          Inr eM → Inr $ do
            e ← eM
            FailT $ subst 𝓈₁ e
        sρ = unionWith (⩌) sρ₂' sρ₁
        sγ = unionWith (⩌) sγ₂' sγ₁
    in Subst sρ sγ 
instance (Ord s,Monad m,Substy s a a) ⇒ Monoid (Subst s a m)

𝓈rescope ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ 𝕏 → Subst s a m
𝓈rescope ρ= Subst ρ null

𝓈rename ∷ (Ord s) ⇒ s ⇰ 𝕐 ⇰ 𝕐 → Subst s a m
𝓈rename sxx = Subst null $ map (map Inl) sxx

𝓈bindM ∷ (Ord s,Monad m) ⇒ s ⇰ 𝕐 ⇰ m a → Subst s a m
𝓈bindM sxeM = Subst null $ map (map $ Inr ∘ lift) sxeM

𝓈bind ∷ (Ord s,Monad m) ⇒ s ⇰ 𝕐 ⇰ a → Subst s a m
𝓈bind = 𝓈bindM ∘ mapp return

substyVar ∷ (Ord s,Monad m) ⇒ (𝕐 → e₂) → s → 𝕐 → SubstT s e₁ e₂ m e₂
substyVar v s x = mjoin $ tries
  [ do SubstEnv _ 𝓋 (Subst _ sγ) ← ask
       γ ← failEff $ sγ ⋕? s
       xeM ← failEff $ γ ⋕? x
       return $ case xeM of
         Inl x' → return $ v x'
         Inr eM → do
           e ← failEff *$ lift $ unFailT eM
           failEff $ 𝓋 e
  , return $ return $ v x
  ]

substyBdr ∷ (Ord s,Monad m,ToIter s t) ⇒ t → 𝕏 → SubstT s e₁ e₂ m 𝕏
substyBdr ss x = do
  sρ ← askL $ substBdrL ⊚ substEnvSubstL
  𝑓M ← askL substEnvFreshL
  xO ← tries $ concat
    -- first see if we are rescoping
    [ mapOn (iter ss) $ \ s → do
        do ρ ← failEff $ sρ ⋕? s
           x' ← failEff $ ρ ⋕? x
           return $ Some x'
    -- next see if we are freshening binders
    , single $ do
        n ← lift *$ failEff 𝑓M
        let x' = 𝕏 (Some n) $ 𝕩name x
        return $ Some x'
    -- just leave the binder alone...
    , single $ return None
    ]
  x' ← case xO of
    Some x' → do
      eachOn ss $ \ s →
        upmodifyEnvL (keyL s ⊚ substVarL ⊚ substEnvSubstL) $ \ 𝓈O →
          Some $ (svar𝕏 x ↦ Inl (svar𝕏 x')) ⩌ ifNone null 𝓈O
      return x'
    None → return x
  eachOn ss $ \ s →
    upmodifyEnvL (keyL s ⊚ substVarL ⊚ substEnvSubstL) $ map $ delete $ svar𝕏 x'
  return x'

substyFrame ∷ (Monad m) ⇒ (e₂ → 𝑂 e₃) → SubstT s e₁ e₃ m a → SubstT s e₁ e₂ m a
substyFrame 𝓋 xM = do
  SubstEnv 𝑓M 𝓋' 𝓈 ← ask
  failEff *$ lift $ runSubstT (SubstEnv 𝑓M (𝓋 *∘ 𝓋') 𝓈) xM

