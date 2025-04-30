{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Test.QuickCheck (Arbitrary, arbitrary, shrink)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as V

type M = UContT (RWS ℕ64 ℕ64 ℕ64)

instance Functor QC.Gen where
  map = HS.fmap
instance Return QC.Gen where
  return = HS.return
instance Bind QC.Gen where
  (≫=) = (HS.>>=)
instance Monad QC.Gen where

instance (Arbitrary a, Arbitrary b) ⇒ Arbitrary (a ∧ b) where
  arbitrary = return (:*) ⊡ arbitrary ⊡ arbitrary

instance (Arbitrary a, Arbitrary b, Ord a) ⇒ Arbitrary (a ⇰ b) where
  arbitrary = assoc ^$ arbitrary @[_]

instance Arbitrary a ⇒ Arbitrary (𝑂 a) where
  arbitrary = QC.oneof [return None, Some ^$ arbitrary]
  shrink = QC.genericShrink

instance (Arbitrary s, Arbitrary e, Eq e, Ord s) ⇒ Arbitrary (SubstElem s e) where
  arbitrary = return SubstElem ⊡ arbitrary ⊡ arbitrary
  shrink (SubstElem is v) =
    (if v () ≡ None then [] else [ SubstElem null (\() → None) ])
    ⧺ (if is ≡ null then [] else [ SubstElem null v])
    ⧺ [SubstElem is' v' | is' <- shrink is, v' <- shrink v]

instance (Arbitrary s, Arbitrary e, Eq e, Ord s) ⇒ Arbitrary (SSubstElem s e) where
  arbitrary = QC.oneof [Var_SSE ^$ QC.arbitrarySizedBoundedIntegral, Trm_SSE ^$ arbitrary]
  shrink (Var_SSE 0) = []
  shrink (Var_SSE n) = [Var_SSE 0, Var_SSE (n `HS.div` 2)]
  shrink (Trm_SSE s) = [Var_SSE 0] ⧺ (Trm_SSE ^$ shrink s)

instance Arbitrary a ⇒ Arbitrary (𝕍 a) where
  arbitrary = 𝕍 ∘ V.fromList ^$ arbitrary @[_]

instance (Arbitrary s, Arbitrary e, Eq e, Ord s) ⇒ Arbitrary (SubstNameless s e) where
  arbitrary = do
    es ← QC.sized $ \ s → do
      mapMOn (vecF (fromInteger $ HS.toInteger s) id) (\_ → arbitrary @(SSubstElem s e))
    return SubstNameless ⊡ QC.arbitrarySizedBoundedIntegral ⊡ return es ⊡ QC.arbitrarySizedBoundedIntegral
  shrink (SubstNameless s es i) =
    (if s ≡ 0 then [] else [SubstNameless 0 es i, SubstNameless 0 es (i `HS.div` 2)])
    ⧺ [SubstNameless s es' i | es' <- shrink es]
    ⧺ (if i ≡ 0 then [] else [SubstNameless s es 0, SubstNameless s es (i `HS.div` 2)]) -- No MonadFail [] instance!?

instance (Arbitrary s1, Arbitrary s2, Arbitrary e, Eq e, Ord s1, Ord s2) ⇒ Arbitrary (GSubst s1 s2 e) where
  arbitrary = return GSubst ⊡ arbitrary ⊡ arbitrary

instance (Arbitrary s, Arbitrary e, Eq e, Ord s) ⇒ Arbitrary (Subst s e) where
  arbitrary = return Subst ⊡ arbitrary

instance (Arbitrary s, Arbitrary e, Eq e, Ord s) ⇒ Arbitrary (𝕐 s e) where
  arbitrary =
    QC.sized $ \ s → QC.frequency
      [ (1, DVar ^$ arbitrary)
      , (1, return NVar ⊡ arbitrary ⊡ arbitrary)
      , (1, GVar ^$ arbitrary)
      , (s, return MVar ⊡ arbitrary ⊡ QC.scale (flip (HS.-) 1) arbitrary)
      ]
  shrink (DVar 0) = []
  shrink (DVar n) = [DVar 0, DVar (n `HS.div` 2)]
  shrink (NVar 0 x) = [DVar 0] ⧺ (NVar 0 ^$ shrink x)
  shrink (NVar n x) = [DVar 0, NVar 0 x, NVar (n `HS.div` 2) x] ⧺ (NVar n ^$ shrink x)
  shrink (GVar x) = [DVar 0] ⧺ (GVar ^$ shrink x)
  shrink (MVar x s) = [DVar 0, MVar x null] ⧺ [MVar x' s' | x' <- shrink x, s' <- shrink s]

instance Arbitrary 𝕎 where
  arbitrary = return 𝕎 ⊡ arbitrary ⊡ return "x"

-- instance (Arbitrary a, Arbitrary c) ⇒ Arbitrary (𝐴 c a) where
--   arbitrary = return 𝐴 ⊡ arbitrary ⊡ arbitrary
--   shrink = QC.genericShrink

instance Null c ⇒ Arbitrary (ULCExp_R c) where
  arbitrary =
    QC.sized $ \size →
      QC.frequency
        [ (1, Var_ULC ^$ arbitrary)
        , (size, QC.oneof
            [ return Lam_ULC ⊡ arbitrary ⊡ QC.scale (flip (HS.-) 1) arbitrary
            , return App_ULC ⊡ QC.scale (flip (HS.-) 1) arbitrary ⊡ QC.scale (flip (HS.-) 1) arbitrary
            ])
        ]
  shrink (Var_ULC v) = Var_ULC ^$ shrink v
  shrink (Lam_ULC bdr body@(ULCExp (aval → bodyRaw))) =
    [bodyRaw]
    ⧺ [Lam_ULC bdr' body' | bdr' <- shrink bdr, body' <- shrink body]
  shrink (App_ULC a@(ULCExp (aval → aRaw)) b@(ULCExp (aval → bRaw))) =
    [aRaw, bRaw]
    ⧺ [App_ULC a' b' | a' <- shrink a, b' <- shrink b]

instance Null c ⇒ Arbitrary (ULCExp c) where
  arbitrary = ULCExp ∘ 𝐴 null ^$ arbitrary
  shrink (ULCExp (𝐴 _ e)) = ULCExp ∘ 𝐴 null ^$ shrink e

prop_todbr_tonmd ∷ ULCExpRaw → QC.Property
prop_todbr_tonmd e =
  let
    de = viewΩ someL $ todbr e
    dne = viewΩ someL $ todbr *$ tonmd e
    _ = pptrace $ ppVertical
        [ ppString "COUNTER-EXAMPLE"
        , ppGA $ ppHorizontal [ppString "               e:", pretty e]
        , ppGA $ ppHorizontal [ppString "         todbr e:", pretty de]
        , ppGA $ ppHorizontal [ppString "todbr *$ tonmd e:", pretty dne]
        ]
  in
  QC.counterexample
    (Text.unpack $ ppshow $
      ppVertical
        [ ppString "COUNTER-EXAMPLE"
        , ppGA $ ppHorizontal [ppString "               e:", pretty e]
        , ppGA $ ppHorizontal [ppString "         todbr e:", pretty de]
        , ppGA $ ppHorizontal [ppString "todbr *$ tonmd e:", pretty dne]
        ]
    )
    $ equivULCExp de dne

broken ∷ ULCExp SrcCxt → QC.Property
broken e = do
  let
    de = todbr e
    dne = todbr *$ tonmd e
  QC.counterexample
    (Text.unpack $ ppshow $
      ppVertical
        [ ppString "COUNTER-EXAMPLE"
        , ppGA $ ppHorizontal [ppString "               e:", pretty e]
        , ppGA $ ppHorizontal [ppString "         todbr e:", pretty de]
        , ppGA $ ppHorizontal [ppString "todbr *$ tonmd e:", pretty dne]
        ]
    )
    $ de ≡ dne

testThisExpression ∷ Monad m ⇒ ULCExp SrcCxt -> m ()
testThisExpression e = do
  let
    de = viewΩ someL $ todbr e
    ne = viewΩ someL $ tonmd e
    dne = viewΩ someL $ todbr ne
  pptraceM $
      ppVertical
        [ ppGA $ ppHorizontal [ppString "               e:", ppGA $ pretty e]
        , ppGA $ ppHorizontal [ppString "         todbr e:", ppGA $ pretty de]
        , ppGA $ ppHorizontal [ppString "         tonmd e:", ppGA $ pretty ne]
        , ppGA $ ppHorizontal [ppString "todbr *$ tonmd e:", ppGA $ pretty dne]
        , ppGA $ ppHorizontal [ppString "           equal?", ppGA $ pretty (equivULCExp de dne)]
        ]

prop_simplify_SubstElem ∷ ULCExp () → ℕ64 → QC.Property
prop_simplify_SubstElem e _shifts = do
  let
    𝓈₁ = SubstNameless @(() ∧ 𝑂 𝕎) @(ULCExp ()) 0 (vec Nil) 1
    𝓈₂ = SubstNameless @(() ∧ 𝑂 𝕎) @(ULCExp ()) 0 (vec [Var_SSE 1]) 1
    a = viewΩ someL $ subst (Subst (GSubst null ((() :* None) ↦ 𝓈₁))) e
    b = viewΩ someL $ subst (Subst (GSubst null ((() :* None) ↦ 𝓈₂))) e
    _ = pptrace $ ppVertical
      [ ppGA $ ppHorizontal [ppString "𝓈₁:", ppGA $ pretty 𝓈₁]
      , ppGA $ ppHorizontal [ppString "𝓈₂:", ppGA $ pretty 𝓈₂]
      , ppGA $ ppHorizontal [ppString " e:", ppGA $ pretty e]
      , ppGA $ ppHorizontal [ppString " a:", ppGA $ pretty a]
      , ppGA $ ppHorizontal [ppString " b:", ppGA $ pretty b]
      ]
  QC.counterexample
    (Text.unpack $ ppshow $ ppVertical
      [ ppGA $ ppHorizontal [ppString "𝓈₁:", ppGA $ pretty 𝓈₁]
      , ppGA $ ppHorizontal [ppString "𝓈₂:", ppGA $ pretty 𝓈₂]
      , ppGA $ ppHorizontal [ppString " e:", ppGA $ pretty e]
      , ppGA $ ppHorizontal [ppString " a:", ppGA $ pretty a]
      , ppGA $ ppHorizontal [ppString " b:", ppGA $ pretty b]
      ])
    $ equivULCExp a b

-- This does not work well, as it does not get picked up by recursive calls.
--
-- {-# LANGUAGE OverlappingInstances #-}
-- instance {-# OVERLAPS #-} (Eq s) ⇒ Eq (SubstNameless s (ULCExp SrcCxt)) where
--   ds1 == ds2 = error "yes"
--     -- let
--     --   SubstNameless s1 es1 i1 = simplifySubstNamelessULC ds1
--     --   SubstNameless s2 es2 i2 = simplifySubstNamelessULC ds2
--     --   in meets [s1 ≡ s2, es1 ≡ es2, i1 ≡ i2]

equivULCSubstNameless ∷ Eq s ⇒ Pretty s ⇒ SubstNameless s (ULCExp 𝒸) → SubstNameless s (ULCExp 𝒸) → 𝔹
equivULCSubstNameless d1 d2 =
  let
    s1 = simplifySubstNamelessULC d1
    s2 = simplifySubstNamelessULC d2
    -- _ = pptrace (ppVertical [ ppString "equiv", pretty d1, pretty s1, pretty d2, pretty s2, pretty (s1 ≡ s2), ppString "-----"])
  in
  s1 ≡ s2

compare𝐷 ∷ (v → v → 𝔹) → (k ⇰ v) → (k ⇰ v) → 𝔹
compare𝐷 compareElement d1 d2 =
  meets $ zipWith (compareElement `on` HS.snd)
    (Map.toAscList (un𝐷 d1)) (Map.toAscList (un𝐷 d2))

equivULCSubstElem ∷ Eq s ⇒ SubstElem s (ULCExp 𝒸) → SubstElem s (ULCExp 𝒸) → 𝔹
equivULCSubstElem (SubstElem i1 mkE1) (SubstElem i2 mkE2) = meets [i1 ≡ i2, elemsEqual]
  where
    elemsEqual =
      case (mkE1 (), mkE2 ()) of
        (Some e1, Some e2) → equivULCExp e1 e2
        (None, None) → True
        _ → False

-- GSubst sometimes look different even though they are morally equivalent.
-- For instance, when the substitution contains a mapping to a substitution that is equivalent to no
-- substitution at all.
simplifyGSubst ∷ Eq s ⇒ GSubst (s ∧ 𝕎) (s ∧ 𝑂 𝕎) (ULCExp 𝒸) → GSubst (s ∧ 𝕎) (s ∧ 𝑂 𝕎) (ULCExp 𝒸)
simplifyGSubst (GSubst gs s) = GSubst gs' s'
  where
    gs' = gs -- TODO: I think technically a SubstElem that only has 0 intros, and None value, is null
    -- Keep only those values that don't simplify to the empty substitution
    s' = 𝐷 (Map.filter ((≢ SubstNameless 0 (vec Nil) 0) ∘ simplifySubstNamelessULC) (un𝐷 s))

equivULCGSubst ∷
  Eq s ⇒ Pretty s ⇒ GSubst (s ∧ 𝕎) (s ∧ 𝑂 𝕎) (ULCExp 𝒸) → GSubst (s ∧ 𝕎) (s ∧ 𝑂 𝕎) (ULCExp 𝒸) → 𝔹
equivULCGSubst (simplifyGSubst → GSubst gs1 s1) (simplifyGSubst → GSubst gs2 s2) =
  meets
    [ compare𝐷 equivULCSubstElem gs1 gs2
    , compare𝐷 equivULCSubstNameless s1 s2
    ]

equivULCSubst ∷ Eq s ⇒ Pretty s ⇒ Subst s (ULCExp 𝒸) → Subst s (ULCExp 𝒸) → 𝔹
equivULCSubst (Subst s1) (Subst s2) =
  equivULCGSubst s1 s2

equivULCExp ∷ ULCExp 𝒸 → ULCExp 𝒸 → 𝔹
equivULCExp (unULCExp → aval → e1) (unULCExp → aval → e2) =
  case (e1, e2) of
    (Var_ULC (MVar v1 s1), Var_ULC (MVar v2 s2)) → meets [v1 ≡ v2, equivULCSubst s1 s2]
    (Var_ULC v1, Var_ULC v2) → v1 ≡ v2
    (Lam_ULC o1 b1, Lam_ULC o2 b2) → meets [o1 ≡ o2, equivULCExp b1 b2]
    (App_ULC l1 r1, App_ULC l2 r2) → meets [equivULCExp l1 l2, equivULCExp r1 r2]
    (_, _) → False

simplifySubstNamelessULC ∷ Eq s ⇒ SubstNameless s (ULCExp 𝒸) → SubstNameless s (ULCExp 𝒸)
simplifySubstNamelessULC (SubstNameless s es i) =
  let
    es' = map replaceDVarTermsWithVars es
    (shifts :* intermediate) = peelPrefix s (list es')
    elems = peelReverseSuffix shifts (list $ reverse intermediate) i
    -- shifting then doing nothing is the same as not shifting at all
    shifts' = if count elems + i ≡ 0 then 0 else shifts
  in
    SubstNameless shifts' elems i
  where
    replaceDVarTermsWithVars
      (Trm_SSE (SubstElem intros (($ ()) → Some (ULCExp (aval → Var_ULC (DVar d))))))
      | intros ≡ null = Var_SSE d -- technically we should also detect when the intro map is non-empty but all zeroes...
    replaceDVarTermsWithVars e = e

    peelPrefix ∷ ℕ64 → 𝐿 (SSubstElem s (ULCExp 𝒸)) → (ℕ64 ∧ 𝐿 (SSubstElem s (ULCExp 𝒸)))
    peelPrefix shifts (Var_SSE h :& t) | h ≡ shifts = peelPrefix (shifts + 1) t
    -- I think seeing `None` here means that the value will be unchanged, which is the same as shift?
    peelPrefix shifts (Trm_SSE (SubstElem _ (($ ()) → None)) :& t) = peelPrefix (shifts + 1) t
    peelPrefix shifts elems = shifts :* elems

    -- Note: technically we could pre-add shifts and intros, but this is a bit more readable
    peelReverseSuffix ∷ ℕ64 → 𝐿 (SSubstElem s (ULCExp 𝒸)) → ℤ64 → 𝕍 (SSubstElem s (ULCExp 𝒸))
    peelReverseSuffix shifts (Var_SSE h :& t) intros
      | intΩ64 h ≡ intΩ64 (shifts + count t) + intros
      = peelReverseSuffix shifts t intros
    peelReverseSuffix _ revElems _ = vec (reverse revElems)

test_equiv_01 ∷ 𝔹
test_equiv_01 =
  let
    -- [] [0,0] [1,2,3,…]
    d1 = SubstNameless 0 (vec [Var_SSE 0, Var_SSE 0]) ((HS.-) 0 1)
    -- [0] [] [0,1,2,3,…]
    d2 = SubstNameless 1 (vec []) ((HS.-) 0 1)
  in equivULCSubstNameless @() d1 d2

test_equiv_02 ∷ 𝔹
test_equiv_02 =
  let
    -- [] [1] [1,2,3,…]
    d1 = SubstNameless 0 (vec [Var_SSE 1]) 0
    -- [] [1,1] [2,3,…]
    d2 = SubstNameless 0 (vec
      [Trm_SSE (SubstElem null $ \ () → Some $ ULCExp $ 𝐴 () $ Var_ULC (DVar 1))
      ,Trm_SSE (SubstElem null $ \ () → Some $ ULCExp $ 𝐴 () $ Var_ULC (DVar 1))
      ]) 0
  in equivULCSubstNameless @() d1 d2

instance Null SrcCxt where null = srcCxt₀

main ∷ IO ()
main = cleanExit $ do
  -- pprint *$ QC.generate $ QC.resize 1 $ arbitrary @ULCExpRaw
  -- pprint *$ QC.generate $ QC.resize 1 $ arbitrary @ULCExpRaw
  -- testThisExpression [ulc| λ a → λ b → λ c → 𝔪:[1]x |]
  -- testThisExpression [ulc| λ a → 𝔪:[1]x |]
  -- QC.quickCheck prop_simplify_SubstElem
  QC.quickCheck prop_todbr_tonmd

  -- pprint $ ppHeader "COLOR TEST"
  -- pprint colorsDemo
  -- $$(testModules False
  --   -- [ "UVMHS.Tests.Core"
  --   [ "UVMHS.Tests.Substitution"
  --   ])
  -- pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"
  -- out "HI"
  -- e ← TH.runQ $ TH.examineCode $ TH.liftTyped (\ () → 𝕟64 5)
  -- shout $ TH.unType e
  -- out "BYE"
