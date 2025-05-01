module UVMHS.Lib.Substitution.Substy where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Var
import UVMHS.Lib.Substitution.SVar

-------------------------------------------
-- SUBSTY (STANDARD SCOPED SUBSTITUTION) --
-------------------------------------------

data FreeVarsAction s e = FreeVarsAction
  { freeVarsActionFilter ∷ s → 𝕐 s e → 𝔹
  , freeVarsActionScope  ∷ (s ∧ 𝑂 𝕎) ⇰ ℕ64
  }
makeLenses ''FreeVarsAction

data SubstAction s e = SubstAction
  -- None == leave binders alone
  -- Some True ==  make everything nameless
  -- Some False == make everything named
  { substActionReBdr ∷ 𝑂 𝔹
  , substActionSubst ∷ Subst s e
  }
makeLenses ''SubstAction
makePrettyRecord ''SubstAction

-- Substy things are things that support having an action in the SubstM monad.
-- This "action" can either be a "compute free variables" action or a
-- "substition" action. This action is encoded as a parameter in the monadic
-- environment.
data SubstEnv s e =
    FVsSubstEnv (FreeVarsAction s e)
  | SubSubstEnv (SubstAction s e)
  | MetaSubstEnv (MetaSubst s e)
makePrisms ''SubstEnv

instance (Pretty e, Pretty s) ⇒ Pretty (SubstEnv s e) where
  pretty (FVsSubstEnv{}) = ppString "FVsSubstEnv (cannot be prettified)"
  pretty (SubSubstEnv sa) = pretty sa
  pretty (MetaSubstEnv s) = pretty s

-- ReaderT (SubstEnv s e)
-- ⇈ the action, which is either compute free variables
-- or perform substitution
-- WriterT (s ⇰ 𝑃 𝕐)
-- ⇈ computes free variables (I think only when the action says to do so TODO:
-- confirm)
newtype SubstM s e a = SubstM
  { unSubstM ∷ UContT (ReaderT (SubstEnv s e) (FailT (WriterT (s ⇰ 𝑃 (𝕐 s e)) ID))) a
  } deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (SubstEnv s e)
  , MonadWriter (s ⇰ 𝑃 (𝕐 s e))
  , MonadFail
  )

mkSubstM ∷ (∀ u. SubstEnv s e → (a → SubstEnv s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u) → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u)
         → SubstM s e a
mkSubstM f = SubstM $ UContT (\ 𝓀 → ReaderT $ \ γ → FailT $ WriterT $ ID $ f γ $ \ x γ' →
  unID $ unWriterT $ unFailT $ runReaderT γ' $ 𝓀 x)

runSubstM ∷
    SubstEnv s e
  → (a → SubstEnv s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u)
  → SubstM s e a
  → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u
runSubstM γ 𝓀 = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ runUContT 𝓀' ∘ unSubstM
  where
    𝓀' x = ReaderT $ \ γ' → FailT $ WriterT $ ID $ 𝓀 x γ'

runSubstMHalt ∷ SubstEnv s e → SubstM s e a → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 a
runSubstMHalt γ = runSubstM γ (\ x _ → null :* Some x)

----------------
-- Substy API --
----------------

class Substy s e a | a→s,a→e where
  substy ∷ STACK ⇒ a → SubstM s e a

-- This is the big top level API point of entry for applying a substitution.
-- Most of the API lower down is concerned with constructing substitutions.
-- ("substitution" = substitution or free variable computation, per SubstEnv)
subst ∷ STACK ⇒ (Substy s e a) ⇒ Subst s e → a → 𝑂 a
subst 𝓈 = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction None 𝓈) ∘ substy

msubst  ∷ STACK ⇒ (Substy s e a) ⇒ MetaSubst s e → a → 𝑂 a
msubst 𝓈 = snd ∘ runSubstMHalt (MetaSubstEnv 𝓈) ∘ substy

todbr ∷ (Substy s e a) ⇒ a → 𝑂 a
todbr = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction (Some True) null) ∘ substy

tonmd ∷ (Substy s e a) ⇒ a → 𝑂 a
tonmd = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction (Some False) null) ∘ substy

fvsWith ∷ (Substy s e a) ⇒ (FreeVarsAction s e → FreeVarsAction s e) → a → s ⇰ 𝑃 (𝕐 s e)
fvsWith f = fst ∘ runSubstMHalt (FVsSubstEnv $ f $ FreeVarsAction (const $ const True) null) ∘ substy

fvsSMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ 𝑃 s → a → s ⇰ 𝑃 (𝕎 ∧ Subst s e)
fvsSMetas ss =
  map (pow ∘ filterMap (view mVarL) ∘ iter)
  ∘ fvsWith (update freeVarsActionFilterL $ \ s y → s ∈ ss ⩓ shape mVarL y)

fvsMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ s → a → 𝑃 (𝕎 ∧ Subst s e)
fvsMetas s x = ifNone pø $ fvsSMetas (single s) x ⋕? s

fvs ∷ (Substy s e a) ⇒ a → s ⇰ 𝑃 (𝕐 s e)
fvs = fvsWith id

nullSubst ∷ Subst s e
nullSubst = Subst $ SubstSpaced null null

appendSubst ∷ (Ord s,Substy s e e) ⇒ Subst s e → Subst s e → Subst s e
appendSubst 𝓈₂ 𝓈₁ = Subst $ appendSubstSpaced (subst ∘ Subst) (unSubst 𝓈₂) $ unSubst 𝓈₁

instance                        Null   (Subst s e) where null = nullSubst
instance (Ord s,Substy s e e) ⇒ Append (Subst s e) where (⧺)  = appendSubst
instance (Ord s,Substy s e e) ⇒ Monoid (Subst s e)

-- 𝓈     = substitution library
-- s     = scoped
-- d     = nameless
-- shift = "going under a binder"
𝓈sdshift ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
𝓈sdshift = alter unSubstL ∘ shiftSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- n = named
𝓈snshift ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e → Subst s e
𝓈snshift 𝑠 = alter unSubstL $ shiftSubstSpaced $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- intro = "
𝓈sdintro ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
𝓈sdintro = Subst ∘ 𝓈introG ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

𝓈snintro ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e
𝓈snintro 𝑠 = Subst $ 𝓈introG $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- dbinds = "substitute de bruijn indices 0..n with elements of this vector"
𝓈sdbinds ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
𝓈sdbinds = Subst ∘ 𝓈sbindsG ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

𝓈sdbind ∷ (Ord s) ⇒ s → e → Subst s e
𝓈sdbind s e = 𝓈sdbinds $ s ↦ single e

-- nbinds = "substitude named variables with key/value pairings in this
-- dictionary"
𝓈snbinds ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ 𝕍 e → Subst s e
𝓈snbinds 𝑠 = Subst $ 𝓈sbindsG $ assoc $ do
  s :* xess ← iter 𝑠
  x :* es ← iter xess
  return $ s :* Some x :* es

𝓈snbind ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
𝓈snbind s x e = 𝓈snbinds $ s ↦ x ↦ single e

-- g = global
𝓈sgbinds ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → Subst s e
𝓈sgbinds sxes = Subst $ 𝓈sgbindsG $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

𝓈sgbind ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
𝓈sgbind s x e = 𝓈sgbinds $ s ↦ x ↦ e

-- m = meta
𝓈smbinds ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → MetaSubst s e
𝓈smbinds sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* SubstElem null (const (return e))

-- non-plural = singular
𝓈smbind ∷ (Ord s) ⇒ s → 𝕎 → e → MetaSubst s e
𝓈smbind s x e = 𝓈smbinds $ s ↦ x ↦ e

-- no s = unscoped
𝓈dshift ∷ ℕ64 → Subst () e → Subst () e
𝓈dshift = 𝓈sdshift ∘ (↦) ()

-- no s = unscoped
𝓈nshift ∷ 𝕎 ⇰ ℕ64 → Subst () e → Subst () e
𝓈nshift = 𝓈snshift ∘ (↦) ()

-- no s = unscoped
𝓈dintro ∷ ℕ64 → Subst () e
𝓈dintro = 𝓈sdintro ∘ (↦) ()

-- no s = unscoped
𝓈nintro ∷ 𝕎 ⇰ ℕ64 → Subst () e
𝓈nintro = 𝓈snintro ∘ (↦) ()

-- no s = unscoped
𝓈dbinds ∷ 𝕍 e → Subst () e
𝓈dbinds = 𝓈sdbinds ∘ (↦) ()

-- no s = unscoped
𝓈dbind ∷ e → Subst () e
𝓈dbind = 𝓈sdbind ()

-- no s = unscoped
𝓈nbinds ∷ 𝕎 ⇰ 𝕍 e → Subst () e
𝓈nbinds = 𝓈snbinds ∘ (↦) ()

-- no s = unscoped
𝓈nbind ∷ 𝕎 → e → Subst () e
𝓈nbind = 𝓈snbind ()

-- no s = unscoped
𝓈gbinds ∷ 𝕎 ⇰ e → Subst () e
𝓈gbinds = 𝓈sgbinds ∘ (↦) ()

-- no s = unscoped
𝓈gbind ∷ 𝕎 → e → Subst () e
𝓈gbind x e = 𝓈gbinds $ x ↦ e

-- no s = unscoped
𝓈mbinds ∷ 𝕎 ⇰ e → MetaSubst () e
𝓈mbinds = 𝓈smbinds ∘ (↦) ()

-- no s = unscoped
𝓈mbind ∷ 𝕎 → e → MetaSubst () e
𝓈mbind x e = 𝓈mbinds $ x ↦ e

--------------------------------------------------
-- CONCRETE IMPLEMENTATIONS OF SUBSTY INSTANCES --
--------------------------------------------------

substyDBdr ∷ (Ord s,Ord e) ⇒ s → SubstM s e ()
substyDBdr s = umodifyEnv $ compose
  [ alter subSubstEnvL $ alter substActionSubstL $ 𝓈sdshift $ s ↦ 1
  , alter fVsSubstEnvL $ alter freeVarsActionScopeL $ (⧺) $ (s :* None) ↦ 1
  ]

substyNBdr ∷ (Ord s,Ord e) ⇒ s → 𝕎 → SubstM s e ()
substyNBdr s x = umodifyEnv $ compose
  [ alter subSubstEnvL $ alter substActionSubstL $ 𝓈snshift $ s ↦ x ↦ 1
  , alter fVsSubstEnvL $ alter freeVarsActionScopeL $ (⧺) $ (s :* Some x) ↦ 1
  ]

substyBdr ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕐 s e' → e) → 𝕎 → SubstM s e ()
substyBdr s 𝓋 x = do
  substyDBdr s
  substyNBdr s x
  bO ← access substActionReBdrL *∘ view subSubstEnvL ^$ ask
  case bO of
    None → skip
    Some b → do
      if b
      then
        umodifyEnv $ alter subSubstEnvL $ alter substActionSubstL $ flip (⧺) $ concat
          [ 𝓈snintro $ s ↦ x ↦ 1
          , 𝓈snbind s x $ 𝓋 $ DVar 0
          ]
      else
        umodifyEnv $ alter subSubstEnvL $ alter substActionSubstL $ flip (⧺) $ concat
          [ 𝓈sdintro $ s ↦ 1
          , 𝓈sdbind s $ 𝓋 $ NVar 0 x
          ]

-- 𝑂 𝕎 parameter `xO`...
-- None = nameless
-- Some x = named with name `x`
-- this is "the name"
--
-- ℕ64 parameter `n` is the de bruijn level/number
substyVar ∷ (Ord s,Ord e,Substy s e e) ⇒ 𝑂 𝕎 → s → (ℕ64 → e) → ℕ64 → SubstM s e e
substyVar xO s 𝓋 n = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let n₀ = ifNone 0 (freeVarsActionScope 𝒶 ⋕? (s :* xO))
      when (n ≥ n₀) $ \ () → do
        let n' = n-n₀
            y = elim𝑂 (const DVar) (flip NVar) xO n'
        when (freeVarsActionFilter 𝒶 s y) $ \ () →
          tell $ s ↦ single y
      return $ 𝓋 n
    SubSubstEnv 𝒶 → do
      let 𝓈s = substSpacedLocal $ unSubst $ substActionSubst 𝒶
      case 𝓈s ⋕? (s :* xO) of
        None → return $ 𝓋 n
        Some 𝓈 → case interpSubstScoped 𝓈 n of
          Var_SSE n' → return $ 𝓋 n'
          Trm_SSE (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ 𝓈introG 𝑠) *$ ueO ()
    MetaSubstEnv{} → return $ 𝓋 n -- I think we just don't apply meta-substitutions to D/NVars?

substyDVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → ℕ64 → SubstM s e e
substyDVar = substyVar None

substyNVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → 𝕎 → ℕ64 → SubstM s e e
substyNVar s 𝓋 x = substyVar (Some x) s 𝓋

substyGVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕎 → e) → 𝕎 → SubstM s e e
substyGVar s 𝓋 x = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let y = GVar x
      when (freeVarsActionFilter 𝒶 s y) $ \ () →
        tell $ s ↦ single y
      return $ 𝓋 x
    SubSubstEnv 𝓈A → do
      let gsᴳ =  substSpacedGlobal $ unSubst $ substActionSubst 𝓈A
      case gsᴳ ⋕? (s :* x) of
        None → return $ 𝓋 x
        Some (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ 𝓈introG 𝑠) *$ ueO ()
    MetaSubstEnv{} → return $ 𝓋 x -- I think we just don't apply meta-substitutions to GVars?

substyMVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕎 → Subst s e → e) → 𝕎 → Subst s e → SubstM s e e
substyMVar s 𝓋 x 𝓈₀ = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let y = MVar x 𝓈₀
      when (freeVarsActionFilter 𝒶 s y) $ \ () →
        tell $ s ↦ single y
      return $ 𝓋 x 𝓈₀
    SubSubstEnv 𝓈A → do
      let 𝓈 = substActionSubst 𝓈A
          -- This versions makes more intuitive sense, in that the incoming substitution action
          -- should have the final word? (This assumes the append does RHS before LHS)
          𝓈' = 𝓈 ⧺ 𝓈₀
          -- This version seems to work better:
          -- 𝓈' = 𝓈₀ ⧺ 𝓈
      return $ 𝓋 x 𝓈'
    MetaSubstEnv (MetaSubst gs) →
      case gs ⋕? (s :* x) of
        None → return $ 𝓋 x 𝓈₀
        Some (SubstElem 𝑠 ueO) →
          failEff $ subst (Subst (𝓈introG 𝑠) ⧺ 𝓈₀) *$ ueO ()

-- subst (𝓈₁ ∘ 𝓈₂) e ≡ subst 𝓈₁ (subst 𝓈₂ e)
--
-- subst (apply 𝓈₁ 𝓈₂) e ≡ subst (mapOn 𝓈₂ (\ x e′ → apply 𝓈₁ e′)) e
-- apply 𝓈₁ id ≡ 𝓈₁
-- apply 𝓈 {0 ↦ 1 , 1 ↦ 2}
-- 𝓈₂(χ⋅𝓈₁)
--
-- (𝓈₂∘𝓈₁)(χ)
--
-- 𝓈₂(χ) = e
--
-- 𝓈₁(e) ← result
--
-- χ⋅id
--
-- 𝓈(χ⋅id) = χ⋅𝓈
--
-- 𝓈₁(𝓈₂(χ⋅id)) ≡ 𝓈₁(χ⋅𝓈₂) ≡ (𝓈₁∘𝓈₂)(χ)

substy𝕐 ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕐 s e → e) → 𝕐 s e → SubstM s e e
substy𝕐 s 𝓋 = \case
  DVar n     → substyDVar s (𝓋 ∘ DVar)        n
  NVar n x   → substyNVar s (𝓋 ∘ flip NVar x) x n
  GVar   x   → substyGVar s (𝓋 ∘ GVar)        x
  MVar   x 𝓈 → substyMVar s (𝓋 ∘∘ MVar)       x 𝓈
