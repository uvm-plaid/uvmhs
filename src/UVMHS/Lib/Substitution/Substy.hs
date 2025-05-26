module UVMHS.Lib.Substitution.Substy where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Shrinky

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
  , freeVarsActionScope  ∷ s ∧ 𝑂 Name ⇰ ℕ64
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

fvsSWith ∷ (Substy s e a) ⇒ (s → 𝕐 s e → 𝔹) → a → s ⇰ 𝑃 (𝕐 s e)
fvsSWith f = fst ∘ evalSubstM (FreeVars_SA $ FreeVarsAction f null) ∘ substy

fvsWith ∷ (Ord s,Substy s e a) ⇒ s → (𝕐 s e → 𝔹) → a → 𝑃 (𝕐 s e)
fvsWith s f = ifNone null ∘ lup s ∘ fvsSWith (\ s' x → s ≡ s' ⩓ f x)

fvsS ∷ (Substy s e a) ⇒ a → s ⇰ 𝑃 (𝕐 s e)
fvsS = fvsSWith $ const $ const True

fvs ∷ (Ord s,Substy s e a) ⇒ s → a → 𝑃 (𝕐 s e)
fvs s = fvsWith s $ const True

fvsSMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ a → s ⇰ 𝑃 (Name ∧ Subst s e)
fvsSMetas = map (pow ∘ filterMap (view m_UVarL) ∘ iter) ∘ fvsSWith (\ _s y → shape m_UVarL y)

fvsMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ s → a → 𝑃 (Name ∧ Subst s e)
fvsMetas s = ifNone pø ∘ lup s ∘ fvsSMetas

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
        None → introDSSubst s n
        Some x → introNSSubst s x n
  in canonSubstWith (curry svarScopeL) introE canonE 𝓈

canonUVar ∷ (Ord s,Eq e,Substy s e e) ⇒ (e → e) → 𝕐 s e → 𝕐 s e
canonUVar canonE = \case
  S_UVar x → S_UVar x
  M_UVar x 𝓈 → M_UVar x $ canonSubst canonE 𝓈

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
  [ alter subst_SAL $ alter substActionSubstL $ shiftDSSubst s 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* None) ↦ 1
  ]

substyNBdr ∷ (Ord s,Ord e) ⇒ s → Name → SubstyM s e ()
substyNBdr s x = umodifyEnv $ compose
  [ alter subst_SAL $ alter substActionSubstL $ shiftNSSubst s x 1
  , alter freeVars_SAL $ alter freeVarsActionScopeL $ (⧺) $ (s :* Some x) ↦ 1
  ]

substyBdr ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕐 s e' → e) → Name → SubstyM s e ()
substyBdr s mkVar x = do
  substyDBdr s
  substyNBdr s x
  aO ← access substActionRebindL ^∘ view subst_SAL ^$ ask
  case aO of
    None → skip
    Some ID_RA → skip
    Some AllNameless_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ introNSSubst s x 1
        , bindNSSubst s x $ mkVar $ duvar 0
        ]
    Some AllNamed_RA → 
      umodifyEnv $ alter subst_SAL $ alter substActionSubstL $ flip (⧺) $ concat
        [ introDSSubst s 1
        , bindDSSubst s $ mkVar $ znuvar x
        ]

-- ℕ64 parameter `n` is the de bruijn level/number
substyVar ∷ (Ord s,Ord e,Substy s e e) ⇒ 𝑂 Name → s → (ℕ64 → e) → ℕ64 → SubstyM s e e
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

substyNVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → Name → ℕ64 → SubstyM s e e
substyNVar s mkVar x = substyVar (Some x) s mkVar

substyGVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (Name → e) → Name → SubstyM s e e
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

substyMVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (Name → Subst s e → e) → Name → Subst s e → SubstyM s e e
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
      return $ mkVar x $ 𝓈 ⧺ 𝓈₀
    MetaSubst_SA (MetaSubst gs) →
      case gs ⋕? (s :* x) of
        None → return $ mkVar x 𝓈₀
        Some (SubstElem ιs eO) →
          failEff $ subst (𝓈₀ ⧺ Subst (introSubstSpaced ιs)) *$ eO

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
  , syntaxSVar
  , null { lexerBasicSyntaxPuns = pow 
             [ ",","...","…"
             , "{","}","[","]"
             , "|->","↦"
             , ":",":g",":m"
             , "==","≡","+"
             ] }
  ]

data ParseSubstAction e = ParseSubstAction
  { parseSubstActionShfts ∷ 𝐼 ℕ64          -- x^0…x^n ↦ [≡]
  , parseSubstActionElems ∷ 𝑂 ℕ64 ⇰ 𝐼 e    -- x^n     ↦ e
  , parseSubstActionIncrs ∷ 𝐼 (ℕ64 ∧ ℤ64)  -- x^n…x^∞ ↦ i
  } deriving (Eq,Ord,Show)
makeLenses ''ParseSubstAction

parseSubstActionShft ∷ ℕ64 → ParseSubstAction e
parseSubstActionShft n = null { parseSubstActionShfts = single n }

parseSubstActionElem ∷ 𝑂 ℕ64 → e → ParseSubstAction e
parseSubstActionElem nO e = null { parseSubstActionElems = nO ↦ single e }

parseSubstActionIncr ∷ ℕ64 → ℤ64 → ParseSubstAction e
parseSubstActionIncr n i = null { parseSubstActionIncrs = single $ n :* i }

instance Null (ParseSubstAction e) where
  null = ParseSubstAction null null null
instance Append (ParseSubstAction e) where
  ParseSubstAction shfts₁ elems₁ incrs₁ ⧺ ParseSubstAction shfts₂ elems₂ incrs₂ =
    ParseSubstAction (shfts₁ ⧺ shfts₂) (elems₁ ⧺ elems₂) $ incrs₁ ⧺ incrs₂
instance Monoid (ParseSubstAction e)

type ParseSubstActions e = 𝑂 (Name ∧ 𝔹) ⇰ ParseSubstAction e

cpSubst ∷ ∀ e. (Eq e,Substy () e e) ⇒ (() → CParser TokenBasic e) → CParser TokenBasic (Subst () e)
cpSubst pE = cpNewContext "subst" $ do
  let pSubstIncr ∷ 𝕏 → CParser TokenBasic (ParseSubstActions e)
      pSubstIncr x₁ = do
        void $ concat $ map cpSyntax ["...","…"]
        xxw₂ ← cpSVarInf
        void $ concat $ map cpSyntax ["|->","↦"]
        void $ concat $ map cpSyntax ["["]
        i ← cpErr "valid subst shift/incr update" $ concat
          [ do void $ concat $ map cpSyntax ["==","≡"]
               return 0
          , do i ← cpInt64
               cpGuard $ i < 0
               return i
          , do void $ cpSyntax "+"
               i ← cpInt64
               cpGuard $ i > 0
               return i
          ]
        a ← cpErr "valid subst shift/incr range" $ case (x₁,xxw₂) of
          (D_SVar n  ,Inl (D_SVar n')   ) |      n≡0,i≡0 → return $ None               ↦ parseSubstActionShft (n' + 1)
          (N_SVar n w,Inl (N_SVar n' w')) | w≡w',n≡0,i≡0 → return $ Some (w' :* False) ↦ parseSubstActionShft (n' + 1)
          (D_SVar n  ,Inr None          )                → return $ None               ↦ parseSubstActionIncr n i
          (N_SVar n w,Inr (Some w')     ) | w≡w'         → return $ Some (w  :* False) ↦ parseSubstActionIncr n i
          _ → cpDie
        void $ concat $ map cpSyntax ["]"]
        return a
      pSubstElem ∷ 𝕏 → CParser TokenBasic (ParseSubstActions e)
      pSubstElem x = do
        void $ concat $ map cpSyntax ["|->","↦"]
        e ← pE ()
        return $ case x of
          D_SVar n   → None              ↦ parseSubstActionElem (Some n) e
          N_SVar n w → Some (w :* False) ↦ parseSubstActionElem (Some n) e
          G_SVar   w → Some (w :* True ) ↦ parseSubstActionElem None     e
  void $ cpSyntax "{"
  xas ← concat ^$ cpManySepBy (void $ cpSyntax ",") $ do
    x ← cpSVar
    concat 
      [ pSubstIncr x
      , pSubstElem x
      ]
  𝓈 ← cpErr "all subst actions valid" $
   concat ^$ mapMOn (iter xas) $ \ (wbO :* ParseSubstAction shfts elemss incrs) → do
    let doScoped = do 
          -- should only have zero or one shift
          nShft ← cpErr "zero or one shift actions" $ cpFailEff $ tries
            [ do view empty𝐼L shfts ; return 0
            , view single𝐼L shfts
            ]
          -- elems should map names to only one element
          elems ← cpErr "one bind per name (scoped)" $ cpFailEff $ mapMOn elemss $ view single𝐼L
          -- all names of element bindings should have an index
          elemsKeys ← cpErr "all variables must have index" $ cpFailEff $ exchange $ iter $ dkeys elems
          let elemsVals = vec $ dvals elems
          -- should only have zero or one increment
          nIncr :* iIncr ← cpErr "zero or one incr actions" $ cpFailEff $ tries
            [ do view empty𝐼L incrs ; return $ (nShft + csize elemsVals) :* 0
            , view single𝐼L incrs
            ]
          -- element bindings should fill gap between shift and incr
          cpErr "elements should fill gap" $ cpGuard $ elemsKeys ≡ range nShft nIncr
          -- biding N elements creates a -N incr
          -- target incr I = -N + E for extra incr E
          -- so E = I+N
          -- target incr I shouldn't be less than -N
          -- so E should be nonnegative
          -- let numElems = nIncr - nShft
          when (iIncr < neg (intΩ64 $ csize elemsVals)) $ \ () →
            cpErr "incr cannot be less than number of substitution elems" cpDie
          let elemsVals' = mapOn elemsVals $ Trm_SSE ∘ SubstElem null ∘ Some
          return $ nShft :* elemsVals' :* iIncr
    case wbO of
      -- nameless
      None → do
        nShft :* elemsVals :* incr  ← doScoped
        return $ Subst $ SubstSpaced null $ (() :* None) ↦ SubstScoped nShft elemsVals incr
      -- named
      Some (w :* False) → do
        nShft :* elemsVals :* incr ← doScoped
        return $ Subst $ SubstSpaced null $ (() :* Some w) ↦ SubstScoped nShft elemsVals incr
      -- global
      Some (w :* True) → do
        -- global can't have shifts
        cpErr "global vars can't have shifts" $ cpGuard $ isEmpty shfts
        -- global can't have incrs
        cpErr "global vars can't have incrs" $ cpGuard $ isEmpty incrs
        -- should only map each name to one element
        elems ← cpErr "one bind per name (scoped)" $ cpFailEff $ mapMOn elemss $ view single𝐼L
        wes ← assoc𝐷 ^$ mapMOn (iter elems) $ \ (nO :* e) → do
          -- having an index for the name doesn't make sense
          cpErr "global vars can't have index" $ cpGuard $ shape noneL nO
          return $ (:*) (() :* w) $ SubstElem null $ Some e
        return $ Subst $ SubstSpaced wes null
  void $ cpSyntax "}"
  return 𝓈

cpUVarNGMVar ∷ ∀ e. (Eq e,Substy () e e) ⇒ (() → CParser TokenBasic e) → CParser TokenBasic (𝕐 () e)
cpUVarNGMVar pE = do
  w ← cpVar
  concat
    [ do nO ← cpSVarNGVarTail
         return $ case nO of
           Some n → nuvar n w
           None   → guvar w
    , do void $ cpSyntax ":m"
         s ← ifNone null ^$ cpOptional $ cpSubst pE
         return $ M_UVar w s
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

instance (Ord s,Shrinky e) ⇒ Shrinky (𝕐 s e) where
  shrink = \case
    S_UVar x → S_UVar ^$ shrink x
    M_UVar x 𝓈 → do
      (x',𝓈') ← shrink (x,𝓈)
      return $ M_UVar x' 𝓈'
