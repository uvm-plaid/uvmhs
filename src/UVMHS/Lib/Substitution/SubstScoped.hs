module UVMHS.Lib.Substitution.SubstScoped where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand

import UVMHS.Lib.Substitution.SubstElem

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
lookupSubstScoped ∷ SubstScoped s e → ℕ64 → SSubstElem s e
lookupSubstScoped (SubstScoped ρ es ι) n =
  let 𝔰̇  = csize es
  in
  if | n < ρ     → Var_SSE n
     | n < 𝔰̇+ρ   → es ⋕! (n-ρ)
     | otherwise → Var_SSE $ natΩ64 $ intΩ64 n+ι

introSubstScoped ∷ (Ord s) ⇒ s ⇰ ℕ64 → s → SubstScoped s e → SubstScoped s e
introSubstScoped ιs s (SubstScoped ρ es ι) = 
  let ρ'  = ρ + ifNone 0 (ιs ⋕? s)
      es' = mapOn es $ introSSubstElem s ιs
  -- TODO: why isn't ι incremented??
  in SubstScoped ρ' es' ι

isNullSubstScoped ∷ SubstScoped s e → 𝔹
isNullSubstScoped (SubstScoped _ρ es ι) = csize es ≡ 0 ⩓ ι ≡ 0

interpSubstScoped ∷ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SubstScoped s e → ℕ64 → 𝑂 e
interpSubstScoped mkVar intro 𝓈 n = interpSSubstElem mkVar intro $ lookupSubstScoped 𝓈 n

canonSubstScoped ∷ ∀ s e. (Eq e) ⇒ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SubstScoped s e → SubstScoped s e
canonSubstScoped mkVar intro = canonElems ∘ collapseNullShift ∘ expandIncs ∘ expandShifts
  where
    expandShiftsM ∷ RWS (SubstScoped s e) () ℕ64 ()
    expandShiftsM = do
      SubstScoped ρ es _ι ← ask
      n ← get
      let 𝔰 = csize es
      if 𝔰 ≡ 0
      then skip
      else 
        if interpSSubstElem mkVar intro (es ⋕! n) ≡ Some (mkVar (ρ+n+1))
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
      if (𝔰 - n) ≡ 0
      then skip
      else
        if interpSSubstElem mkVar intro (es ⋕! (𝔰 - n - 1)) ≡ Some (mkVar (natΩ64 (intΩ64 (ρ + (𝔰 - n - 1)) + ι)))
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
    canonElems (SubstScoped ρ es ι) = SubstScoped ρ (map (canonSSubstElem mkVar intro) es) ι
        


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

ppSubstScoped ∷ (Pretty s,Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → (𝕊 → Doc) → SubstScoped s e → Doc
ppSubstScoped ιD xD (SubstScoped ρ es ι) = 
  let kvs = concat
        [ if ρ ≡ null then null else single $
            let k = concat [xD "0",ppPun "…",xD $ show𝕊 ρ] 
                v = ppLit "[≡]"
            in k :* v
        , mapOn (withIndex @ℕ64 es) $ \ (n :* e) →
            let k = concat [xD $ show𝕊 $ ρ + n]
                v = ppSSubstElemNamed ιD e
            in k :* v
        , single $ 
            let k = concat
                  [ xD $ show𝕊 $ ρ + csize es
                  , ppPun "…"
                  , xD "∞" 
                  ]
                v = ppLit $ concat 
                  [ "["
                  , case ι ⋚ 0 of
                      LT → show𝕊 ι 
                      EQ → "≡"
                      GT → concat ["+",show𝕊 ι]
                  , "]"
                  ]
            in k :* v
        ]
  in
  ppDict kvs

ppSubstScopedNamed ∷ (Pretty s,Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → 𝕊 → SubstScoped s e → Doc
ppSubstScopedNamed ιD x = ppSubstScoped ιD $ ppBdr ∘ ((⧺) x)

instance (Pretty e, Pretty s) ⇒ Pretty (SubstScoped s e) where
  pretty = ppSubstScopedNamed pretty ""

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
    𝔰 ← fuzzy
    es ← mapMOn (vecF 𝔰 id) $ const fuzzy
    ι ← randr (neg $ intΩ64 𝔰) $ intΩ64 𝔰
    return $ SubstScoped ρ es ι
