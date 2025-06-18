module UVMHS.Lib.Substitution.SubstScoped where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.Var

-- ============================== --
-- SCOPED (NAMELESS) SUBSTITUTION --
-- ============================== --

--------------------------------------------------------------------------------
-- Scoped (nameless) substitutions are substitutions over nameless
-- variables—i.e., natural numbers, otherwise known as De Bruijn indices—and
-- which are scope aware—i.e., they support operations for reinterpreting the
-- substitution when moving underneath new binders.
--------------------------------------------------------------------------------

-- 𝓈 ⩴ ⟨ρ,es,ι⟩
-- INVARIANT: |es| + ι ≥ 0
data SubstScoped s e = SubstScoped
  { substScopedShift ∷ ℕ64
  -- ^ ρ: De Bruijn indices lower than this number will be untouched by this
  --      substitution. Think of it as a substitution working over all natural
  --      numbers being shifted to the right to ignore this many first indices.
  , substScopeElems ∷ 𝕍 (SSubstElem s e)
  -- ^ es: Instantiates as many of the first indices (post-shift by ρ) as the
  --       length of this vector with the values in the vector.
  , substScopeIntro ∷ ℤ64
  -- ^ ι: Starting at the nameless variable index after all the shifts (ρ) and all
  --      the instantiations (es), simulate an introduction of this many new
  --      nameless variables by bumping all subsequent indices by this much.
  } deriving (Eq,Ord,Show)

makeLenses ''SubstScoped

-- 𝓈 ≜ ⟨ρ,es,ι⟩
-- 𝔰 ≜ |es|
-- 𝓈(i) ≜
--   cases (disjoint):
--     |       i < ρ   ⇒ i
--     |   ρ ≤ i < ρ+𝔰 ⇒ es[i-ρ]
--     | ρ+𝔰 ≤ i       ⇒ i+ι
-- 𝓈(i) ≜
--   cases (sequential):
--     | i < ρ   ⇒ i
--     | i < ρ+𝔰 ⇒ es[i-ρ]
--     | ⊤       ⇒ i+ι
-- e.g.,
-- 𝓈 = ⟨2,[e],-1⟩
-- 𝓈 is logically equivalent to the (infinite) substitution vector
-- [  0 ↦ ⌊0⌋    | ≡
-- ,  1 ↦ ⌊1⌋    |
-- ---------------
-- ,  2 ↦  e     | [e]
-- ---------------
-- ,  3 ↦ ⌊2⌋    | -1
-- ,  4 ↦ ⌊3⌋    |
-- , …
-- ]
lookupSubstScoped ∷ SubstScoped s e → DVar → SSubstElem s e
lookupSubstScoped (SubstScoped ρ es ι) (DVar n) =
  let 𝔰̇  = csize es
  in
  if | n < ρ     → Var_SSE $ DVar n
     | n < 𝔰̇+ρ   → es ⋕! (n-ρ)
     | otherwise → Var_SSE $ DVar $ natΩ64 $ intΩ64 n+ι

interpSubstScoped ∷ e ⌲ DVar → (s ⇰ ℕ64 → e → 𝑂 e) → SubstScoped s e → DVar → 𝑂 e
interpSubstScoped ℓvar substE 𝓈 n = interpSSubstElem ℓvar substE $ lookupSubstScoped 𝓈 n

wfSubstScoped ∷ SubstScoped s e → 𝔹
wfSubstScoped (SubstScoped _ρ es ι) = ι ≥ neg (intΩ64 $ csize es)

canonSubstScoped ∷ ∀ s e. (Eq s,Eq e) ⇒ e ⌲ DVar → (s ⇰ ℕ64 → e → 𝑂 e) → (e → e) → SubstScoped s e → SubstScoped s e
canonSubstScoped ℓvar substE canonE = collapseNullShift ∘ expandIncs ∘ expandShifts ∘ canonElems
  where
    expandShiftsM ∷ RWS (SubstScoped s e) () ℕ64 ()
    expandShiftsM = do
      SubstScoped ρ es _ι ← ask
      n ← get
      if n ≡ csize es
      then skip
      else 
        if es ⋕! n ≡ Var_SSE (DVar $ ρ+n)
        then do bump ; expandShiftsM
        else skip
    expandShifts ∷ SubstScoped s e → SubstScoped s e
    expandShifts 𝓈@(SubstScoped ρ es ι) =
      let n = fst $ fst $ runRWS 𝓈 0 expandShiftsM
      in SubstScoped (ρ + n) (vec $ dropN n $ iter es) ι
    expandIncsM ∷ RWS (SubstScoped s e) () ℕ64 ()
    expandIncsM = do
      SubstScoped ρ es ι ← ask
      n ← get
      let 𝔰 = csize es
      if n ≡ 𝔰
      then skip
      else
        let i = 𝔰 - 1 - n
            i' = intΩ64 ρ + intΩ64 i + ι
        in
        if i' ≥ 0 ⩓ es ⋕! i ≡ Var_SSE (DVar $ natΩ64 i')
        then do bump ; expandIncsM
        else skip
    expandIncs ∷ SubstScoped s e → SubstScoped s e
    expandIncs 𝓈@(SubstScoped ρ es ι) =
      let n = fst $ fst $ runRWS 𝓈 0 expandIncsM
      in SubstScoped ρ (vec $ reverse $ dropN n $ reverse $ iter es) ι
    collapseNullShift ∷ SubstScoped s e → SubstScoped s e
    collapseNullShift 𝓈@(SubstScoped _ρ es ι) =
      if csize es ≡ 0 ⩓ ι ≡ 0
      then SubstScoped 0 null 0
      else 𝓈
    canonElems ∷ SubstScoped s e → SubstScoped s e
    canonElems (SubstScoped ρ es ι) = SubstScoped ρ (map (canonSSubstElem ℓvar substE canonE) es) ι
        
isNullSubstScoped ∷ SubstScoped s e → 𝔹
isNullSubstScoped (SubstScoped _ρ es ι) = csize es ≡ 0 ⩓ ι ≡ 0

introSubstScoped ∷ ℕ64 → SubstScoped s e
introSubstScoped = SubstScoped 0 null ∘ intΩ64

shiftSubstScoped ∷ (Ord s) ⇒ s ⇰ ℕ64 → s → SubstScoped s e → SubstScoped s e
shiftSubstScoped ιs s (SubstScoped ρ es ι) = 
  let ρ'  = (+) ρ $ ifNone (const 0) $ ιs ⋕? s
      es' = mapOn es $ introSSubstElem s ιs
  in SubstScoped ρ' es' ι

bindSubstScoped ∷ 𝕍 e → SubstScoped s e
bindSubstScoped es = 
  let es' = map (Trm_SSE ∘ SubstElem null ∘ Some) es
      ι = neg $ intΩ64 $ csize es
  in SubstScoped null es' ι

substSubstScoped ∷ e ⌲ DVar → (s ⇰ ℕ64 → e → 𝑂 e) → SubstScoped s e → SubstScoped s e
substSubstScoped ℓvar substE (SubstScoped ρ es ι) = 
  let es' = map (substSSubstElem ℓvar substE) es
  in SubstScoped ρ es' ι

-- -- | If we get a `SubstScoped` where some `dsubstElems` elements are merely emulating what happens under
-- -- a shift, or under an intro, we simplify it to instead use those, making the vector of elements
-- -- shorter.
-- --
-- -- For instance, consider:
-- --   SubstScoped 3 [3, 4, 1, 1, 9, 10] 2
-- -- supposedly, it:
-- --   * keeps the first 3 indices protected (0 ↦ 0, 1 ↦ 1, 2 ↦ 2)
-- --   * then maps indices [3,4,5,6,7,8] to [3,4,1,1,9,10]
-- --   * then maps indices [9,10,11,…] to [11,12,13,‥]
-- -- but this could be better expressed as:
-- --   SubstScoped 5 [1, 1] 2
-- --   * keeps the first 5 indices protected, i.e. [0,1,2,3,4] ↦ [0,1,2,3,4]
-- --   * then [5,6] ↦ [1, 1]
-- --   * then [7,8,9,10,11,…] ↦ [9,10,11,12,13,…]
-- simplifySubstScoped ∷ SubstScoped s e → SubstScoped s e
-- simplifySubstScoped (SubstScoped s es i) =
--   let
--     (shifts :* intermediate) = peelPrefix s (list es)
--     elems = peelReverseSuffix shifts (list $ reverse intermediate) i
--   in SubstScoped shifts elems i
--   where
--     peelPrefix ∷ ℕ64 → 𝐿 (SSubstElem s e) → (ℕ64 ∧ 𝐿 (SSubstElem s e))
--     peelPrefix shifts (Var_SSE h :& t) | h ≡ s = peelPrefix (shifts + 1) t
--     peelPrefix shifts elems = shifts :* elems
-- 
--     -- Note: technically we could pre-add shifts and intros, but this is a bit more readable
--     peelReverseSuffix ∷ ℕ64 → 𝐿 (SSubstElem s e) → ℤ64 → 𝕍 (SSubstElem s e)
--     peelReverseSuffix shifts (Var_SSE h :& t) intros
--       | intΩ64 h ≡ intΩ64 (shifts + count t) + intros
--       = peelReverseSuffix shifts t intros
--     peelReverseSuffix _ revElems _ = vec (reverse revElems)

-- instance (Eq e, Eq s) ⇒ Eq (SubstScoped s e) where
--   ds1 == ds2 =
--     let
--       SubstScoped s1 es1 i1 = simplifySubstScoped ds1
--       SubstScoped s2 es2 i2 = simplifySubstScoped ds2
--       in meets [s1 ≡ s2, es1 ≡ es2, i1 ≡ i2]

-- Note: SubstScoped tend to be quite verbose under makePrettyRecord, so this instance tries to make them
-- print more concisely.
--
-- ⊘ means the identity substitution
--
-- Otherwise the pattern is: `/n{...}↑o` where:
-- - `/n` represents `n` shifts,
-- - `{...}` is the vector of de Bruijn instantiations,
-- - `↑o` represents `o` introductions,
-- Each of these subparts is optional if it's zero/zero-length.

---------------------
-- PRETTY PRINTING --
---------------------

ppSubstScopedWith ∷ (Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → (DVarInf → Doc) → SubstScoped s e → 𝐼 (Doc ∧ Doc)
ppSubstScopedWith ιD xD (SubstScoped ρ es ι) = 
  let kvs = concat
        [ if ρ ≡ 0 then null else single $
            let k = concat [xD $ Var_DVI $ DVar 0,ppPun "…",xD $ Var_DVI $ DVar $ ρ - 1] 
                v = ppLit "[≡]"
            in k :* v
        , mapOn (withIndex @ℕ64 es) $ \ (n :* e) →
            let k = concat [xD $ Var_DVI $ DVar $ ρ + n]
                v = ppSSubstElemNamed ιD xD e
            in k :* v
        , if ι ≡ 0 then null else single $ 
            let k = concat
                  [ xD $ Var_DVI $ DVar $ ρ + csize es
                  , ppPun "…"
                  , xD Inf_DVI
                  ]
                v = ppLit $ concat 
                  [ "["
                  , case ι ⋚ 0 of
                      LT → show𝕊 ι 
                      -- EQ pattern should never happen due to enclosing if condition
                      EQ → "≡"    
                      GT → concat ["+",show𝕊 ι]
                  , "]"
                  ]
            in k :* v
        ]
  in
  kvs

-- ppSubstScopedNamed ∷ (Pretty s,Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → Name → SubstScoped s e → 𝐼 (Doc ∧ Doc)
-- ppSubstScopedNamed ιD x = ppSubstScoped ιD $ \ n → pretty $ NVarInf n x

instance (Pretty e, Pretty s) ⇒ Pretty (SubstScoped s e) where
  pretty = ppDict ∘ ppSubstScopedWith pretty pretty

-------------
-- FUNCTOR --
-------------

instance Functor (SubstScoped s) where
  map f (SubstScoped ρ es ι) = SubstScoped ρ (mapp f es) ι

-------------
-- FUZZING --
-------------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SubstScoped s e) where
  fuzzy = do
    ρ ← fuzzy
    𝔰 ← fuzzyDepth
    es ← mapMOn (vecF 𝔰 id) $ \ _ → fuzzyRec
    ι ← randr (neg $ intΩ64 𝔰) $ intΩ64 𝔰
    return $ SubstScoped ρ es ι

instance (Ord s,Shrinky e) ⇒ Shrinky (SubstScoped s e) where
  shrink (SubstScoped ρ es ι) = do
    (ρ',es',ι') ← shrink (ρ,es,ι)
    mzeroIfNot $ ι' ≥ neg (intΩ64 $ csize es')
    return $ SubstScoped ρ' es' ι'
