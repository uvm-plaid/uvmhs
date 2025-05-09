module UVMHS.Lib.Substitution.Substy where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

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

-- TODO: make this return a delayed subst elem?
class (SVarView s e) ⇒ Substy s e a | a→e,e→s where
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

canonSubst ∷ (Ord s,Eq e,Substy s e e) ⇒ Subst s e → Subst s e
canonSubst 𝓈 = 
  let introE ιs = subst $ concat $ mapOn (iter ιs) $ \ (s :* xO :* n) → case xO of
        None → sdintroSubst $ s ↦ n
        Some x → snintroSubst $ s ↦ x ↦ n
  in canonSubstWith (curry svarScopeL) introE 𝓈

nullSubst ∷ Subst s e
nullSubst = Subst $ SubstSpaced null null

appendSubst ∷ (Ord s,Substy s e e) ⇒ Subst s e → Subst s e → Subst s e
appendSubst 𝓈₂ 𝓈₁ = Subst $ appendSubstSpaced (curry svarScopeL) (subst ∘ Subst) (unSubst 𝓈₂) $ unSubst 𝓈₁

instance                        Null   (Subst s e) where null = nullSubst
instance (Ord s,Substy s e e) ⇒ Append (Subst s e) where (⧺)  = appendSubst
instance (Ord s,Substy s e e) ⇒ Monoid (Subst s e)

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
        Some 𝓈 → case lookupSubstScoped 𝓈 n of
          Var_SSE n' → return $ mkVar n'
          Trm_SSE (SubstElem ιs eO) → failEff $ subst (Subst $ introSubstSpaced ιs) *$ eO
    MetaSubst_SA _ → return $ mkVar n

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
        Some (SubstElem ιs eO) → failEff $ subst (Subst $ introSubstSpaced ιs) *$ eO
    MetaSubst_SA _ → return $ mkVar x

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
        Some (SubstElem ιs eO) →
          failEff $ subst (Subst (introSubstSpaced ιs) ⧺ 𝓈₀) *$ eO

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

-------------
-- PARSING --
-------------

syntaxUVar ∷ LexerBasicSyntax
syntaxUVar = concat
  [ syntaxVar
  , null { lexerBasicSyntaxPuns = pow 
             [ ",","...","…"
             , "{","}","[","]","|_","⌊","_|","⌋"
             , "|->","↦"
             , "^",":g",":m"
             , "==","≡","+"
             ] }
  ]

cpUVarNGMVar ∷ (Eq e,Substy () e e) ⇒ (() → CParser TokenBasic e) → CParser TokenBasic (𝕐 () e)
cpUVarNGMVar pE = do
  x ← cpVar
  concat
    [ do n ← ifNone 0 ^$ cpOptional $ do
           void $ cpSyntax "^"
           n ← failEff ∘ natO64 *$ cpInteger
           return n
         return $ nuvar n x
    , do void $ cpSyntax ":g"
         return $ guvar x
    , do void $ cpSyntax ":m"
         s ← ifNone null ^$ cpOptional $ do
           void $ cpSyntax "{"
           𝓈 ← concat ^$ cpManySepBy (void $ cpSyntax ",") $ concat
             [ do x₁ ← cpSVarRaw
                  void $ concat $ map cpSyntax ["...","…"]
                  x₂ ← cpSVarRaw
                  void $ concat $ map cpSyntax ["|->","↦"]
                  void $ concat $ map cpSyntax ["["]
                  void $ concat $ map cpSyntax ["==","≡"]
                  void $ concat $ map cpSyntax ["]"]
                  case (x₁,x₂) of
                    (D_SVar n₁,D_SVar n₂) 
                      | n₁ ≡ 0 → return $ dshiftSubst n₂ null
                    (N_SVar n₁ w₁,N_SVar n₂ w₂) 
                      | w₁ ≡ w₂ ⩓ n₁ ≡ 0 → return $ nshiftSubst (w₂ ↦ n₂) null
                    _ → abort
             , do x₁ ← cpSVarRaw
                  void $ concat $ map cpSyntax ["...","…"]
                  x₂ ← cpSVarRawInf
                  void $ concat $ map cpSyntax ["|->","↦"]
                  void $ concat $ map cpSyntax ["["]
                  i ← concat
                    [ do void $ concat $ map cpSyntax ["==","≡"]
                         return 0
                    , do i ← failEff ∘ intO64 *$ cpInteger
                         guard $ i < 0
                         return i
                    , do void $ cpSyntax "+"
                         i ← failEff ∘ intO64 *$ cpInteger
                         guard $ i > 0
                         return i
                    ]
                  void $ concat $ map cpSyntax ["]"]
                  case (x₁,x₂) of
                    (D_SVar n₁,Inr None) → 
                      return $ dshiftSubst n₁ $ dzintroSubst i
                    (N_SVar n₁ w₁,Inr (Some w₂)) | w₁ ≡ w₂ → 
                      return $ nshiftSubst (w₁ ↦ n₁) $ dzintroSubst i
                    _ → abort
             , do x' ← cpSVarRaw
                  void $ concat $ map cpSyntax ["|->","↦"]
                  e ← pE ()
                  return $ case x' of
                    D_SVar n     → dshiftSubst n $ dbindSubst e
                    N_SVar n w → nshiftSubst (w ↦ n) $ nbindSubst w e
                    G_SVar   w → gbindSubst w e
             ]
           void $ cpSyntax "}"
           return $ {- canonSubst $ -} 𝓈
             
         return $ M_UVar x s
   ]

cpUVar ∷ (Eq e,Substy () e e) ⇒ (() → CParser TokenBasic e) → CParser TokenBasic (𝕐 () e)
cpUVar pE = concat
  [ do n ← cpDVar
       return $ duvar n
  , cpUVarNGMVar pE
  ]

cpUVarRaw ∷ (Eq e,Substy () e e) ⇒ (() → CParser TokenBasic e) → CParser TokenBasic (𝕐 () e)
cpUVarRaw pE = concat
  [ do n ← cpDVarRaw
       return $ duvar n
  , cpUVarNGMVar pE
  ]
