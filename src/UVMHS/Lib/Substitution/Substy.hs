module UVMHS.Lib.Substitution.Substy where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Var
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Subst

-- ====== --
-- SUBSTY --
-- ====== --

-- When computing free variables, modify the standard way you would do it
-- using:
-- - `freeVarsActionFilter` (typically a global parameter) to filter out free
--    variables you don't want (e.g., by discriminating on scope `s`)
-- - `freeVarsActionScope` (local internal information) to indicate what
--   binders are in scope, e.g., free variables are those which are not bound,
--   so `⌊1⌋` is free but not `⌊0⌋` in the (nameless) lambda `λ. ⌊0⌋ ⌊1⌋`.
data FreeVarsAction s e = FreeVarsAction
  { freeVarsActionFilter ∷ s → 𝕐 s e → 𝔹
  , freeVarsActionScope  ∷ s ∧ 𝑂 𝕎 ⇰ ℕ64
  }
makeLenses ''FreeVarsAction

data RebindAction = ID_RA | AllNameless_RA | AllNamed_RA
  deriving (Eq,Ord,Show)

data SubstAction s e = SubstAction
  { substActionRebind ∷ RebindAction
  , substActionSubst ∷ Subst s e
  } deriving (Eq,Ord,Show)
makeLenses ''SubstAction

-- Substy things are things that support having an action in the SubstyM monad.
-- This "action" can either be a "compute free variables" action or a
-- "substition" action. This action is encoded as a parameter in the monadic
-- environment.
data SubstyAction s e =
    FreeVars_SA (FreeVarsAction s e)
  | Subst_SA (SubstAction s e)
  | MetaSubst_SA (MetaSubst s e)
makePrisms ''SubstyAction

-- instance (Pretty e, Pretty s) ⇒ Pretty (SubstAction s e) where
--   pretty (FreeVars_SA{}) = ppString "FreeVars_SA (cannot be prettified)"
--   pretty (Subst_SA sa) = pretty sa
--   pretty (MetaSubst_SA s) = pretty s

newtype SubstyM s e a = SubstyM
  { unSubstyM ∷ UContT (ReaderT (SubstyAction s e) (FailT (WriterT (s ⇰ 𝑃 (𝕐 s e)) ID))) a
  } deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (SubstyAction s e)
  , MonadWriter (s ⇰ 𝑃 (𝕐 s e))
  , MonadFail
  )

mkSubstM 
  ∷ (∀ u. SubstyAction s e 
        → (a → SubstyAction s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u) 
        → (s ⇰ 𝑃 (𝕐 s e)) 
        ∧ 𝑂 u)
  → SubstyM s e a
mkSubstM f = SubstyM $ UContT (\ 𝓀 → ReaderT $ \ γ → FailT $ WriterT $ ID $ f γ $ \ x γ' →
  unID $ unWriterT $ unFailT $ runReaderT γ' $ 𝓀 x)

runSubstM 
  ∷ SubstyAction s e
  → (a → SubstyAction s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u)
  → SubstyM s e a
  → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u
runSubstM γ 𝓀 = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ runUContT 𝓀' ∘ unSubstyM
  where
    𝓀' x = ReaderT $ \ γ' → FailT $ WriterT $ ID $ 𝓀 x γ'

evalSubstM
  ∷ SubstyAction s e
  → SubstyM s e a
  → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 a
evalSubstM γ = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstyM

------------
-- Substy --
------------

class Substy s e a | a→s,a→e where
  substy ∷ a → SubstyM s e a

-- These are the big top level API point of entry for applying a substy action,
-- which is either a free variables computation, a rebinding (named to namelss,
-- or vice versa), a standard substitution, or a metavariable substitution.

fvsWith ∷ (Substy s e a) ⇒ (s → 𝕐 s e → 𝔹) → a → s ⇰ 𝑃 (𝕐 s e)
fvsWith f = fst ∘ evalSubstM (FreeVars_SA $ FreeVarsAction f null) ∘ substy

fvsSMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ 𝑃 s → a → s ⇰ 𝑃 (𝕎 ∧ Subst s e)
fvsSMetas ss = map (pow ∘ filterMap (view m_UVarL) ∘ iter) ∘ fvsWith (\ s y → s ∈ ss ⩓ shape m_UVarL y)

fvsMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ s → a → 𝑃 (𝕎 ∧ Subst s e)
fvsMetas s x = ifNone pø $ fvsSMetas (single s) x ⋕? s

fvs ∷ (Substy s e a) ⇒ a → s ⇰ 𝑃 (𝕐 s e)
fvs = fvsWith $ const $ const True

todbr ∷ (Substy s e a) ⇒ a → 𝑂 a
todbr = snd ∘ evalSubstM (Subst_SA $ SubstAction AllNameless_RA null) ∘ substy

tonmd ∷ (Substy s e a) ⇒ a → 𝑂 a
tonmd = snd ∘ evalSubstM (Subst_SA $ SubstAction AllNamed_RA null) ∘ substy

subst ∷ (Substy s e a) ⇒ Subst s e → a → 𝑂 a
subst 𝓈 = snd ∘ evalSubstM (Subst_SA $ SubstAction ID_RA 𝓈) ∘ substy

msubst  ∷ (Substy s e a) ⇒ MetaSubst s e → a → 𝑂 a
msubst 𝓈 = snd ∘ evalSubstM (MetaSubst_SA 𝓈) ∘ substy

------------------
-- SUBST MONOID --
------------------

nullSubst ∷ Subst s e
nullSubst = Subst $ SubstSpaced null null

appendSubst ∷ (Ord s,Substy s e e) ⇒ Subst s e → Subst s e → Subst s e
appendSubst 𝓈₂ 𝓈₁ = Subst $ appendSubstSpaced (subst ∘ Subst) (unSubst 𝓈₂) $ unSubst 𝓈₁

-- appendMetaSubst??

instance                        Null   (Subst s e) where null = nullSubst
instance (Ord s,Substy s e e) ⇒ Append (Subst s e) where (⧺)  = appendSubst
instance (Ord s,Substy s e e) ⇒ Monoid (Subst s e)

----------------------------
-- BUILDING SUBSTITUTIONS --
----------------------------

--------------------
-- SHIFT NAMELESS --
--------------------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- shift = "going under a binder"
sdshiftSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
sdshiftSubst = alter unSubstL ∘ shiftSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- shift = "going under a binder"
dshiftSubst ∷ ℕ64 → Subst () e → Subst () e
dshiftSubst = sdshiftSubst ∘ (↦) ()

-----------------
-- SHIFT NAMED --
-----------------

-- s     = (name)spaced
-- n     = named (scoped)
-- shift = "going under a binder"
snshiftSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e → Subst s e
snshiftSubst 𝑠 = alter unSubstL $ shiftSubstSpaced $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- n     = named (scoped)
-- shift = "going under a binder"
nshiftSubst ∷ 𝕎 ⇰ ℕ64 → Subst () e → Subst () e
nshiftSubst = snshiftSubst ∘ (↦) ()

--------------------
-- INTRO NAMELESS --
--------------------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- intro = "a new variable has been introduced"
sdintroSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
sdintroSubst = Subst ∘ introSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- intro = "a new variable has been introduced"
dintroSubst ∷ ℕ64 → Subst () e
dintroSubst = sdintroSubst ∘ (↦) ()

-----------------
-- INTRO NAMED --
-----------------

-- s     = (name)spaced
-- d     = named (scoped)
-- intro = "a new variable has been introduced"
snintroSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e
snintroSubst 𝑠 = Subst $ introSubstSpaced $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- d     = named (scoped)
-- intro = "a new variable has been introduced"
nintroSubst ∷ 𝕎 ⇰ ℕ64 → Subst () e
nintroSubst = snintroSubst ∘ (↦) ()

----------
-- BIND --
----------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- binds = "substitute indices 0..n with elements from this vector"
sdbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
sdbindsSubst = Subst ∘ sbindsSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- binds = "substitute indices 0..n with elements from this vector"
dbindsSubst ∷ 𝕍 e → Subst () e
dbindsSubst = sdbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- d    = nameless (scoped) (i.e., de bruijn)
-- bind = "substitute index 0 with this element
sdbindSubst ∷ (Ord s) ⇒ s → e → Subst s e
sdbindSubst s e = sdbindsSubst $ s ↦ single e

-- d    = nameless (scoped) (i.e., de bruijn)
-- bind = "substitute index 0 with this element
dbindSubst ∷ e → Subst () e
dbindSubst = sdbindSubst ()

-- s     = (name)spaced
-- n     = named (scoped)
-- binds = "substitute variables with elements from this map"
snbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ 𝕍 e → Subst s e
snbindsSubst 𝑠 = Subst $ sbindsSubstSpaced $ assoc $ do
  s :* xess ← iter 𝑠
  x :* es ← iter xess
  return $ s :* Some x :* es

-- n     = named (scoped)
-- binds = "substitute variables with elements from this map"
nbindsSubst ∷ 𝕎 ⇰ 𝕍 e → Subst () e
nbindsSubst = snbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- n    = named (scoped)
-- bind = "substitute this variable with this element
snbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
snbindSubst s x e = snbindsSubst $ s ↦ x ↦ single e

-- n    = named (scoped)
-- bind = "substitute this variable with this element
nbindSubst ∷ 𝕎 → e → Subst () e
nbindSubst = snbindSubst ()

-- s     = (name)spaced
-- g     = global (unscoped)
-- binds = "substitute variables with elements from this vector"
sgbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → Subst s e
sgbindsSubst sxes = Subst $ ubindsSubstSpaced $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

-- g     = global (unscoped)
-- binds = "substitute variables with elements from this vector"
gbindsSubst ∷ 𝕎 ⇰ e → Subst () e
gbindsSubst = sgbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- g    = global (unscoped)
-- bind = "substitute this variable with this element
sgbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
sgbindSubst s x e = sgbindsSubst $ s ↦ x ↦ e

-- g    = global (unscoped)
-- bind = "substitute this variable with this element
gbindSubst ∷ 𝕎 → e → Subst () e
gbindSubst = sgbindSubst ()

-- s     = (name)spaced
-- m     = metavar (unscoped)
-- binds = "substitute variables with elements from this vector"
smbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → MetaSubst s e
smbindsSubst sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* SubstElem null (const (return e))

-- m     = metavar (unscoped)
-- binds = "substitute variables with elements from this vector"
mbindsSubst ∷ 𝕎 ⇰ e → MetaSubst () e
mbindsSubst = smbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- m    = metavar (unscoped)
-- bind = "substitute this variable with this element
smbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → MetaSubst s e
smbindSubst s x e = smbindsSubst $ s ↦ x ↦ e

-- m    = metavar (unscoped)
-- bind = "substitute this variable with this element
mbindSubst ∷ 𝕎 → e → MetaSubst () e
mbindSubst = smbindSubst ()

--------------------------------------------------
-- CONCRETE IMPLEMENTATIONS OF SUBSTY INSTANCES --
--------------------------------------------------

substyDBdr ∷ (Ord s,Ord e) ⇒ s → SubstyM s e ()
substyDBdr s = umodifyEnv $ compose
  [ alter subst_SAL $ alter substActionSubstL $ sdshiftSubst $ s ↦ 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* None) ↦ 1
  ]

substyNBdr ∷ (Ord s,Ord e) ⇒ s → 𝕎 → SubstyM s e ()
substyNBdr s x = umodifyEnv $ compose
  [ alter subst_SAL $ alter substActionSubstL $ snshiftSubst $ s ↦ x ↦ 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* Some x) ↦ 1
  ]

substyBdr ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕐 s e' → e) → 𝕎 → SubstyM s e ()
substyBdr s mkVar x = do
  substyDBdr s
  substyNBdr s x
  aO ← access substActionRebindL ^∘ view subst_SAL ^$ ask
  case aO of
    None → skip
    Some ID_RA → skip
    Some AllNameless_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ snintroSubst $ s ↦ x ↦ 1
        , snbindSubst s x $ mkVar $ duvar 0
        ]
    Some AllNamed_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ sdintroSubst $ s ↦ 1
        , sdbindSubst s $ mkVar $ znuvar x
        ]

-- ℕ64 parameter `n` is the de bruijn level/number
substyVar ∷ (Ord s,Ord e,Substy s e e) ⇒ 𝑂 𝕎 → s → (ℕ64 → e) → ℕ64 → SubstyM s e e
substyVar xO s mkVar n = do
  γ ← ask
  case γ of
    FreeVars_SA a → do
      let n₀ = ifNone 0 (freeVarsActionScope a ⋕? (s :* xO))
      when (n ≥ n₀) $ \ () → do
        let n' = n-n₀
            y = elim𝑂 (const duvar) (flip nuvar) xO n'
        when (freeVarsActionFilter a s y) $ \ () →
          tell $ s ↦ single y
      return $ mkVar n
    Subst_SA a → do
      let 𝓈s = substSpacedScoped $ unSubst $ substActionSubst a
      case 𝓈s ⋕? (s :* xO) of
        None → return $ mkVar n
        Some 𝓈 → case interpSubstScoped 𝓈 n of
          Var_SSE n' → return $ mkVar n'
          Trm_SSE (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ introSubstSpaced 𝑠) *$ ueO ()
    MetaSubst_SA{} → return $ mkVar n -- I think we just don't apply meta-substitutions to D/NVars?

substyDVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → ℕ64 → SubstyM s e e
substyDVar = substyVar None

substyNVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → 𝕎 → ℕ64 → SubstyM s e e
substyNVar s mkVar x = substyVar (Some x) s mkVar

substyGVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕎 → e) → 𝕎 → SubstyM s e e
substyGVar s mkVar x = do
  γ ← ask
  case γ of
    FreeVars_SA a → do
      let y = guvar x
      when (freeVarsActionFilter a s y) $ \ () →
        tell $ s ↦ single y
      return $ mkVar x
    Subst_SA 𝓈A → do
      let gsᴳ =  substSpacedUnscoped $ unSubst $ substActionSubst 𝓈A
      case gsᴳ ⋕? (s :* x) of
        None → return $ mkVar x
        Some (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ introSubstSpaced 𝑠) *$ ueO ()
    MetaSubst_SA{} → return $ mkVar x -- I think we just don't apply meta-substitutions to GVars?

substyMVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕎 → Subst s e → e) → 𝕎 → Subst s e → SubstyM s e e
substyMVar s mkVar x 𝓈₀ = do
  γ ← ask
  case γ of
    FreeVars_SA a → do
      let y = M_UVar x 𝓈₀
      when (freeVarsActionFilter a s y) $ \ () →
        tell $ s ↦ single y
      return $ mkVar x 𝓈₀
    Subst_SA 𝓈A → do
      let 𝓈 = substActionSubst 𝓈A
          -- This versions makes more intuitive sense, in that the incoming substitution action
          -- should have the final word? (This assumes the append does RHS before LHS)
          𝓈' = 𝓈 ⧺ 𝓈₀
          -- This version seems to work better:
          -- 𝓈' = 𝓈₀ ⧺ 𝓈
      return $ mkVar x 𝓈'
    MetaSubst_SA (MetaSubst gs) →
      case gs ⋕? (s :* x) of
        None → return $ mkVar x 𝓈₀
        Some (SubstElem 𝑠 ueO) →
          failEff $ subst (Subst (introSubstSpaced 𝑠) ⧺ 𝓈₀) *$ ueO ()

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

substy𝕏 ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕏 → e) → 𝕏 → SubstyM s e e
substy𝕏 s mkVar = \case
  D_SVar n   → substyDVar s (mkVar ∘ D_SVar)        n
  N_SVar n x → substyNVar s (mkVar ∘ flip N_SVar x) x n
  G_SVar   x → substyGVar s (mkVar ∘ G_SVar)        x

substy𝕐 ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕐 s e → e) → 𝕐 s e → SubstyM s e e
substy𝕐 s mkVar = \case
  S_UVar x   → substy𝕏    s (mkVar ∘ S_UVar)  x
  M_UVar x 𝓈 → substyMVar s (mkVar ∘∘ M_UVar) x 𝓈
