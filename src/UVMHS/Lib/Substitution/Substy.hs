module UVMHS.Lib.Substitution.Substy where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.Name
import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Var

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
  { freeVarsActionFilter ∷ s → UVar s e → 𝔹
  , freeVarsActionScope  ∷ s ∧ SName ⇰ ℕ64
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
  { unSubstyM ∷ UContT (ReaderT (SubstyAction s e) (FailT (WriterT (s ⇰ 𝑃 (UVar s e)) ID))) a
  } deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (SubstyAction s e)
  , MonadWriter (s ⇰ 𝑃 (UVar s e))
  , MonadFail
  )

mkSubstM 
  ∷ (∀ u. SubstyAction s e 
        → (a → SubstyAction s e → (s ⇰ 𝑃 (UVar s e)) ∧ 𝑂 u) 
        → (s ⇰ 𝑃 (UVar s e)) 
        ∧ 𝑂 u)
  → SubstyM s e a
mkSubstM f = SubstyM $ UContT (\ 𝓀 → ReaderT $ \ γ → FailT $ WriterT $ ID $ f γ $ \ x γ' →
  unID $ unWriterT $ unFailT $ runReaderT γ' $ 𝓀 x)

runSubstM 
  ∷ SubstyAction s e
  → (a → SubstyAction s e → (s ⇰ 𝑃 (UVar s e)) ∧ 𝑂 u)
  → SubstyM s e a
  → (s ⇰ 𝑃 (UVar s e)) ∧ 𝑂 u
runSubstM γ 𝓀 = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ runUContT 𝓀' ∘ unSubstyM
  where
    𝓀' x = ReaderT $ \ γ' → FailT $ WriterT $ ID $ 𝓀 x γ'

evalSubstM
  ∷ SubstyAction s e
  → SubstyM s e a
  → (s ⇰ 𝑃 (UVar s e)) ∧ 𝑂 a
evalSubstM γ = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstyM

------------
-- Substy --
------------

class (SVarView s e) ⇒ Substy s e a | a→e,e→s where
  substy ∷ a → SubstyM s e a

fvssWith ∷ (Substy s e a) ⇒ (s → UVar s e → 𝔹) → a → s ⇰ 𝑃 (UVar s e)
fvssWith f = fst ∘ evalSubstM (FreeVars_SA $ FreeVarsAction f null) ∘ substy

fvsWith ∷ (Ord s,Substy s e a) ⇒ s → (UVar s e → 𝔹) → a → 𝑃 (UVar s e)
fvsWith s f = ifNone null ∘ lup s ∘ fvssWith (\ s' x → s ≡ s' ⩓ f x)

fvss ∷ (Substy s e a) ⇒ a → s ⇰ 𝑃 (UVar s e)
fvss = fvssWith $ const $ const True

fvs ∷ (Ord s,Substy s e a) ⇒ s → a → 𝑃 (UVar s e)
fvs s = fvsWith s $ const True

fvssMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ a → s ⇰ 𝑃 (MVar s e)
fvssMetas = map (pow ∘ filterMap (view m_UVarL) ∘ iter) ∘ fvssWith (\ _s y → shape m_UVarL y)

fvsMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ s → a → 𝑃 (MVar s e)
fvsMetas s = ifNone pø ∘ lup s ∘ fvssMetas

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

canonSubst ∷ (Ord s,Eq e,Substy s e e) ⇒ (e → e) → Subst s e → Subst s e
canonSubst canonE 𝓈 = 
  let introE ιs = subst $ concat $ mapOn (iter ιs) $ \ (s :* xO :* n) → case xO of
        D_SName → dintroSubst s n
        N_SName x → nintroSubst s x n
  in canonSubstWith (uncurry svarScopeL) introE canonE 𝓈

canonMVar ∷ (Ord s,Eq e,Substy s e e) ⇒ (e → e) → MVar s e → MVar s e
canonMVar canonE (MVar 𝓈 x) = MVar (canonSubst canonE 𝓈) x

substMVar ∷ (Ord s,Substy s e e) ⇒ Subst s e → MVar s e → MVar s e
substMVar 𝓈 (MVar 𝓈ₓ x) = MVar (𝓈 ⧺ 𝓈ₓ) x

canonUVar ∷ (Ord s,Eq e,Substy s e e) ⇒ (e → e) → UVar s e → UVar s e
canonUVar canonE = \case
  D_UVar x → D_UVar x
  N_UVar x → N_UVar x
  G_UVar x → G_UVar x
  M_UVar x → M_UVar $ canonMVar canonE x

nullSubst ∷ Subst s e
nullSubst = Subst $ SubstSpaced null null

appendSubst ∷ (Ord s,Substy s e e) ⇒ Subst s e → Subst s e → Subst s e
appendSubst 𝓈₂ 𝓈₁ = Subst $ appendSubstSpaced (uncurry svarScopeL) (subst ∘ Subst) (unSubst 𝓈₂) $ unSubst 𝓈₁

instance                        Null   (Subst s e) where null = nullSubst
instance (Ord s,Substy s e e) ⇒ Append (Subst s e) where (⧺)  = appendSubst
instance (Ord s,Substy s e e) ⇒ Monoid (Subst s e)

--------------------------------------------------
-- CONCRETE IMPLEMENTATIONS OF SUBSTY INSTANCES --
--------------------------------------------------

substyDBdr ∷ (Ord s,Ord e) ⇒ s → SubstyM s e ()
substyDBdr s = umodifyEnv $ compose
  [ alter subst_SAL $ alter substActionSubstL $ dshiftSubst s 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* D_SName) ↦ 1
  ]

substyNBdr ∷ (Ord s,Ord e) ⇒ s → Name → SubstyM s e ()
substyNBdr s x = umodifyEnv $ compose
  [ alter subst_SAL $ alter substActionSubstL $ nshiftSubst s x 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* N_SName x) ↦ 1
  ]

substyBdr ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (SVar → e) → Name → SubstyM s e ()
substyBdr s mkVar x = do
  substyDBdr s
  substyNBdr s x
  aO ← access substActionRebindL ^∘ view subst_SAL ^$ ask
  case aO of
    None → skip
    Some ID_RA → skip
    Some AllNameless_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ nintroSubst s x 1
        , nbindSubst s x $ mkVar $ D_SVar $ DVar 0
        ]
    Some AllNamed_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ dintroSubst s 1
        , dbindSubst s $ mkVar $ svar_Name x
        ]

-- TRICKY: 
-- it should always be the case that `(mkVar n $ svarLevel x) ≈ x`
-- i.e., if `x` is an `NVar`, then `mkVar` should create named variables with
-- the same name as `x`, just with a new level.
substySVarG ∷ ∀ s e. (Ord s,Ord e,Substy s e e) ⇒ (DVar → e) → s → SVar → SubstyM s e e
substySVarG mkVar s x = do
  let xName = svarName x
      xLevel = svarLevel x
  γ ← ask
  case γ of
    FreeVars_SA a → do
      let -- `m` is the number of binders we are underneath
          m ∷ ℕ64
          m = ifNone 0 $ freeVarsActionScope a ⋕? (s :* xName)
      -- when `xLevel ≥ m` it is a free variable
      when (unDVar xLevel ≥ m) $ \ () → do
        let -- create the free variable to accumulate, whose variable level
            -- must be recalculated to be the found variable's level minus `m`
            y = uvar_SVar $ mkSVar xName $ DVar $ unDVar xLevel - m
        -- only accumulate the free variable when it passes the filter
        when (freeVarsActionFilter a s y) $ \ () →
          tell $ s ↦ single y
      -- return the variable we found unchanged
      return $ mkVar xLevel
    Subst_SA a → do
      let 𝓈Ss ∷ (s ∧ SName) ⇰ SubstScoped (s ∧ SName) e
          𝓈Ss = substSpacedScoped $ unSubst $ substActionSubst a
      case 𝓈Ss ⋕? (s :* xName) of
        None → 
          -- there is no substitution for this scope and name
          -- return the variable we found unchanged
          return $ mkVar xLevel
        Some 𝓈 → case lookupSubstScoped 𝓈 xLevel of
          Var_SSE xLevel' → 
            -- rename the found variable to same name but new level
            return $ mkVar xLevel'
          Trm_SSE (SubstElem ιs eO) → 
            -- substitute the found variable for expression `eO` with delayed
            -- increment `ιs`
            failEff $ subst (Subst $ introSubstSpaced ιs) *$ eO
    MetaSubst_SA _ → 
      -- the substitution is only looking for meta-variables, so return the
      -- found variable unchanged
      return $ mkVar xLevel

substyDVar ∷ (Ord s,Ord e,Substy s e e) ⇒ (DVar → e) → s → DVar → SubstyM s e e
substyDVar mkVar s = substySVarG mkVar s ∘ D_SVar

substyNVar ∷ (Ord s,Ord e,Substy s e e) ⇒ (NVar → e) → s → NVar → SubstyM s e e
substyNVar mkVar s x = substySVarG (\ n → mkVar $ NVar n $ nvarName x) s $ N_SVar x

substyGVar ∷ ∀ s e. (Ord s,Ord e,Substy s e e) ⇒ (GVar → e) → s → GVar → SubstyM s e e
substyGVar mkVar s x = do
  γ ← ask
  case γ of
    FreeVars_SA a → do
      -- global variables are always free...
      -- create the free variable to accumulate
      let y = G_UVar  x
      -- only accumulate the free variable when it passes the filter
      when (freeVarsActionFilter a s y) $ \ () →
        tell $ s ↦ single y
      return $ mkVar x
    Subst_SA 𝓈A → do
      let 𝓈Gs ∷ (s ∧ Name) ⇰ SubstElem (s ∧ SName) e
          𝓈Gs =  substSpacedUnscoped $ unSubst $ substActionSubst 𝓈A
      case 𝓈Gs ⋕? (s :* unGVar x) of
        None → return $ mkVar x
        Some (SubstElem ιs eO) → failEff $ subst (Subst $ introSubstSpaced ιs) *$ eO
    MetaSubst_SA _ → return $ mkVar x

substySVar ∷ (Ord s,Ord e,Substy s e e) ⇒ (SVar → e) → s → SVar → SubstyM s e e
substySVar mkVar s x = substySVarG (mkVar ∘ mkSVar (svarName x)) s x

substyVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ (Var → e) → s → Var → SubstyM s e e
substyVar mkVar s = \case
  D_Var x → substyDVar (mkVar ∘ D_Var) s x
  N_Var x → substyNVar (mkVar ∘ N_Var) s x
  G_Var x → substyGVar (mkVar ∘ G_Var) s x

substyMVar ∷ ∀ s e. (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ (MVar s e → e) → s → MVar s e → SubstyM s e e
substyMVar mkVar s x = do
  γ ← ask
  case γ of
    FreeVars_SA a → do
      -- meta variables are always free...
      -- create the free variable to accumulate
      let y = M_UVar x
      -- only accumulate the free variable when it passes the filter
      when (freeVarsActionFilter a s y) $ \ () →
        tell $ s ↦ single y
      return $ mkVar x
    Subst_SA 𝓈A → do
      let 𝓈 ∷ Subst s e
          𝓈 = substActionSubst 𝓈A
      return $ mkVar $ substMVar 𝓈 x
    MetaSubst_SA (MetaSubst 𝓈M) →
      case 𝓈M ⋕? (s :* mvarName x) of
        None → return $ mkVar x
        Some (SubstElem ιs eO) →
          failEff $ subst (mvarSubst x ⧺ Subst (introSubstSpaced ιs)) *$ eO

substyUVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ (UVar s e → e) → s → UVar s e → SubstyM s e e
substyUVar mkVar s = \case
  D_UVar x → substyDVar (mkVar ∘ D_UVar) s x
  N_UVar x → substyNVar (mkVar ∘ N_UVar) s x
  G_UVar x → substyGVar (mkVar ∘ G_UVar) s x
  M_UVar x → substyMVar (mkVar ∘ M_UVar) s x

-------------
-- PARSING --
-------------

syntaxSubst ∷ LexerWSBasicSyntax
syntaxSubst = concat
  [ syntaxVarInf
  , null { lexerWSBasicSyntaxPuns = pow 
             [ ",","...","…"
             , "{","}","[","]"
             , "|->","↦"
             , "==","≡","+"
             ] }
  ]

syntaxMVar ∷ LexerWSBasicSyntax
syntaxMVar = concat
  [ syntaxSubst
  , null { lexerWSBasicSyntaxPuns = pow 
             [ ":m"
             ] }
  ]

syntaxUVar ∷ LexerWSBasicSyntax
syntaxUVar = concat
  [ syntaxVar
  , syntaxMVar
  ]

data ParseSubstAction e = ParseSubstAction
  { parseSubstActionShfts ∷ 𝐼 ℕ64          -- x:0…x:n ↦ [≡]
  , parseSubstActionElems ∷ 𝑂 DVar ⇰ 𝐼 e   -- x:n     ↦ e
  , parseSubstActionIncrs ∷ 𝐼 (ℕ64 ∧ ℤ64)  -- x:n…x:∞ ↦ i
  } deriving (Eq,Ord,Show)
makeLenses ''ParseSubstAction

parseSubstActionShft ∷ ℕ64 → ParseSubstAction e
parseSubstActionShft n = null { parseSubstActionShfts = single n }

parseSubstActionElem ∷ 𝑂 DVar → e → ParseSubstAction e
parseSubstActionElem nO e = null { parseSubstActionElems = nO ↦ single e }

parseSubstActionIncr ∷ ℕ64 → ℤ64 → ParseSubstAction e
parseSubstActionIncr n i = null { parseSubstActionIncrs = single $ n :* i }

instance Null (ParseSubstAction e) where
  null = ParseSubstAction null null null
instance Append (ParseSubstAction e) where
  ParseSubstAction shfts₁ elems₁ incrs₁ ⧺ ParseSubstAction shfts₂ elems₂ incrs₂ =
    ParseSubstAction (shfts₁ ⧺ shfts₂) (elems₁ ⧺ elems₂) $ incrs₁ ⧺ incrs₂
instance Monoid (ParseSubstAction e)

type ParseSubstActions e = SGName ⇰ ParseSubstAction e

pSubst ∷ ∀ e. (Eq e,Substy () e e) ⇒ (() → Parser TokenWSBasic e) → Parser TokenWSBasic (Subst () e)
pSubst pE = pNewContext "subst" $ do
  let pSubstIncr ∷ Var → Parser TokenWSBasic (ParseSubstActions e)
      pSubstIncr x₁ = do
        pTokSyntaxAny ["...","…"]
        x₂ ← pErr "parsing varinf" pVarInf
        pTokSyntaxAny ["|->","↦"]
        pTokSyntax "["
        i ← pErr "valid subst shift/incr update" $ concat
          [ do pTokSyntaxAny ["==","≡"]
               return 0
          , do i ← pTokInt64
               pGuard $ i < 0
               return i
          , do pTokSyntax "+"
               i ← pTokInt64
               pGuard $ i > 0
               return i
          ]
        a ← pErr "valid subst shift/incr range" $ case (x₁,x₂) of
          (D_Var(DVar n)         ,D_VarInf(Var_DVI(DVar n'))             ) | n≡0,i≡0      → return $ D_SGName    ↦ parseSubstActionShft (n' + 1)
          (N_Var(NVar (DVar n) x),N_VarInf(NVarInf(Var_DVI (DVar n')) x')) | n≡0,i≡0,x≡x' → return $ N_SGName x' ↦ parseSubstActionShft (n' + 1)
          (D_Var(DVar n)         ,D_VarInf Inf_DVI                       )                → return $ D_SGName    ↦ parseSubstActionIncr n i
          (N_Var(NVar (DVar n) x),N_VarInf(NVarInf Inf_DVI x')           ) |         x≡x' → return $ N_SGName x' ↦ parseSubstActionIncr n i
          _ → pDie
        pTokSyntax "]"
        return a
      pSubstElem ∷ Var → Parser TokenWSBasic (ParseSubstActions e)
      pSubstElem x₀ = do
        pTokSyntaxAny ["|->","↦"]
        e ← pE ()
        return $ case x₀ of
          D_Var n          → D_SGName   ↦ parseSubstActionElem (Some n) e
          N_Var (NVar n x) → N_SGName x ↦ parseSubstActionElem (Some n) e
          G_Var (GVar x)   → G_SGName x ↦ parseSubstActionElem None     e
  pTokSyntax "{"
  xas ← concat ^$ pManySepBy (pTokSyntax ",") $ do
    x ← pVar
    concat 
      [ pSubstIncr x
      , pSubstElem x
      ]
  𝓈 ← pErr "all subst actions valid" $
   concat ^$ mapMOn (iter xas) $ \ (wbO :* ParseSubstAction shfts elemss incrs) → do
    let doScoped = do 
          -- should only have zero or one shift
          nShft ∷ ℕ64  
                ← pErr "zero or one shift actions" $ pFailEff $ tries
            [ do view empty𝐼L shfts ; return 0
            , view single𝐼L shfts
            ]
          -- elems should map names to only one element
          elems ∷ 𝑂 DVar ⇰ e
                ← pErr "one bind per name (scoped)" $ pFailEff $ mapMOn elemss $ view single𝐼L
          -- all names of element bindings should have an index
          elemsKeys ∷ 𝐼 DVar
                    ← pErr "all variables must have index" $ pFailEff $ exchange $ iter $ dkeys elems
          let elemsVals ∷ 𝕍 e
              elemsVals = vec $ dvals elems
          -- should only have zero or one increment
          nIncr :* iIncr ∷ ℕ64 ∧ ℤ64
                         ← pErr "zero or one incr actions" $ pFailEff $ tries
            [ do view empty𝐼L incrs ; return $ (nShft + csize elemsVals) :* 0
            , view single𝐼L incrs
            ]
          -- element bindings should fill gap between shift and incr
          pErr "elements should fill gap" $ pGuard $ map unDVar elemsKeys ≡ range nShft nIncr
          -- biding N elements creates a -N incr
          -- target incr I = -N + E for extra incr E
          -- so E = I+N
          -- target incr I shouldn't be less than -N
          -- so E should be nonnegative
          -- let numElems = nIncr - nShft
          when (iIncr < neg (intΩ64 $ csize elemsVals)) $ \ () →
            pErr "incr cannot be less than number of substitution elems" pDie
          let elemsVals' ∷ 𝕍 (SSubstElem s e)
              elemsVals' = mapOn elemsVals $ Trm_SSE ∘ SubstElem null ∘ Some
          return $ nShft :* elemsVals' :* iIncr
    case wbO of
      -- nameless
      D_SGName → do
        nShft :* elemsVals :* incr  ← doScoped
        return $ Subst $ SubstSpaced null $ (() :* D_SName) ↦ SubstScoped nShft elemsVals incr
      -- named
      N_SGName x → do
        nShft :* elemsVals :* incr ← doScoped
        return $ Subst $ SubstSpaced null $ (() :* N_SName x) ↦ SubstScoped nShft elemsVals incr
      -- global
      G_SGName x → do
        -- global can't have shifts
        pErr "global vars can't have shifts" $ pGuard $ isEmpty shfts
        -- global can't have incrs
        pErr "global vars can't have incrs" $ pGuard $ isEmpty incrs
        -- should only map each name to one element
        elems ← pErr "one bind per name (scoped)" $ pFailEff $ mapMOn elemss $ view single𝐼L
        wes ← assoc𝐷 ^$ mapMOn (iter elems) $ \ (nO :* e) → do
          -- having an index for the name doesn't make sense
          pErr "global vars can't have index" $ pGuard $ shape noneL nO
          return $ (:*) (() :* x) $ SubstElem null $ Some e
        return $ Subst $ SubstSpaced wes null
  pTokSyntax "}"
  return 𝓈

pMVarTail ∷ (Eq e,Substy () e e) ⇒ (() → Parser TokenWSBasic e) → Name → Parser TokenWSBasic (MVar () e)
pMVarTail pE x = do
  pTokSyntax ":m"
  𝓈 ← ifNone null ^$ pOptional $ pSubst pE
  return $ MVar 𝓈 x

pMVar ∷ (Eq e,Substy () e e) ⇒ (() → Parser TokenWSBasic e) → Parser TokenWSBasic (MVar () e)
pMVar pE = do
  x ← pName
  pMVarTail pE x

pUVar ∷ (Eq e,Substy () e e) ⇒ (() → Parser TokenWSBasic e) → Parser TokenWSBasic (UVar () e)
pUVar pE = concat
  [ do x ← pDVar
       return $ D_UVar x
  , do x ← pName
       concat
         [ N_UVar ^$ pNVarTail x
         , G_UVar ^$ pGVarTail x
         , M_UVar ^$ pMVarTail pE x
         ]
  ]

instance (Ord s,Shrinky e) ⇒ Shrinky (UVar s e) where
  shrink = \case
    D_UVar x → D_UVar ^$ shrink x
    N_UVar x → N_UVar ^$ shrink x
    G_UVar x → G_UVar ^$ shrink x
    M_UVar x → M_UVar ^$ shrink x
