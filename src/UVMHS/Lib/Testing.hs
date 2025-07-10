{-# LANGUAGE CPP #-}
module UVMHS.Lib.Testing
  ( module UVMHS.Lib.Testing
  ) where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Shrinky
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.TreeNested
import UVMHS.Lib.THLiftInstances ()

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

data FuzzParams = FuzzParams
  { fuzzParamsRadiusMax ∷ ℕ64
  , fuzzParamsRadiusStep ∷ ℕ64
  , fuzzParamsDepthMax ∷ ℕ64
  , fuzzParamsDepthStep ∷ ℕ64
  , fuzzParamsSpread ∷ ℕ64
  } deriving (Eq,Ord,Show,TH.Lift)

fuzzParams₀ ∷ FuzzParams
fuzzParams₀ = FuzzParams 10 1 10 1 1

fuzzParamsTny ∷ ℕ64 → FuzzParams
fuzzParamsTny = FuzzParams 2 1 2 1

fuzzParamsSml ∷ ℕ64 → FuzzParams
fuzzParamsSml = FuzzParams 4 1 4 1

fuzzParamsMed ∷ ℕ64 → FuzzParams
fuzzParamsMed = FuzzParams 8 1 8 1

fuzzParamsLrg ∷ ℕ64 → FuzzParams
fuzzParamsLrg = FuzzParams 16 1 16 1

data Test = Test
  { testSrcLoc ∷ Doc
  , testResult ∷ () → ErrorT (() → 𝑇A Doc) (RWST FuzzParams ℕ64 () IO) ()
  }

eqTest ∷ (Eq a,Pretty a) ⇒ 𝐿 𝕊 → 𝕊 → a → a → 𝑇D Test
eqTest tags lS x y = keys𝑇D tags $ val𝑇D $ Test (ppString lS) $ \ () →
  if x ≡ y
  then tell one
  else throw $ \ () → concat
    [ 𝐤 "L" $ 𝐯 $ pretty x
    , 𝐤 "R" $ 𝐯 $ pretty y
    ]

propTest ∷ (Pretty a) ⇒ 𝐿 𝕊 → 𝕊 → a → (a → 𝔹) → (a → Doc) → 𝑇D Test
propTest tags lS x p xD = keys𝑇D tags $ val𝑇D $ Test (ppString lS) $ \ () → do
  if p x
  then tell one
  else throw $ \ () → 𝐤 "V" $ 𝐯 $ xD x

fuzzTest ∷ (Shrinky a) ⇒ 𝐿 𝕊 → 𝕊 → FuzzyM a → (a → 𝔹) → (a → Doc) → 𝑇D Test
fuzzTest tags lS xM p xD = keys𝑇D tags $ val𝑇D $ Test (ppString lS) $ \ () → do
  FuzzParams radiusMax radiusStep depthMax depthStep spread ← ask
  eachOn (uptoStep radiusMax radiusStep) $ \ r →
    eachOn (uptoStep depthMax depthStep) $ \ d →
      eachOn (upto spread) $ \ _i → do
        x ← io $ rng $ runFuzzyMRG (FuzzyEnv r d) xM
        if p x 
        then tell one
        else
          let n :* x' = shrunk (not ∘ p) x
              errD () = concat
                -- fuzzing radius
                [ 𝐤 "R" $ 𝐯 $ pretty r
                -- fuzzing depth
                , 𝐤 "D" $ 𝐯 $ pretty d
                -- number of shrinking steps
                , 𝐤 "N" $ 𝐯 $ pretty n
                -- counterexample value
                , 𝐤 "V" $ 𝐯 $ xD x'
                ]
          in throw errD

data TestsOut = TestsOut
  { testsOutFailures ∷ 𝐿 𝕊 ⇰ 𝐼 (Doc ∧ (() → 𝑇A Doc))
  , testsOutMetrics ∷ 𝐿 𝕊 ⇰ ℕ64 ∧ ℕ64
  }

testsOutFailure ∷ 𝐿 𝕊 → Doc → (() → 𝑇A Doc) → TestsOut
testsOutFailure tag lD errD = TestsOut ((↦) tag $ single $ lD :* errD) null

testsOutMetricPass ∷ 𝐿 𝕊 → ℕ64 → TestsOut
testsOutMetricPass tags n = TestsOut null $ tags ↦ n :* zero

testsOutMetricFail ∷ 𝐿 𝕊 → ℕ64 → TestsOut
testsOutMetricFail tags n = TestsOut null $ tags ↦ zero :* n

instance Null TestsOut where
  null = TestsOut null null
instance Append TestsOut where
  TestsOut f₁ m₁ ⧺ TestsOut f₂ m₂ = TestsOut (f₁ ⧺ f₂) $ m₁ ⧺ m₂
instance Monoid TestsOut

runTests ∷ 𝔹 → FuzzParams → 𝑇D Test → IO ()
runTests noisy γ tests = do
  when  noisy $ \ () → do
    pprint $ ppComment "running tests…"
  oflush
  let fₙ ∷ 𝕊 → MU (RWST (𝐼 𝕊) TestsOut () IO) → MU (RWST (𝐼 𝕊) TestsOut () IO)
      fₙ gr = onMU $ mapEnv $ pospend $ single gr
      fₗ ∷ 𝐼 Test → MU (RWST (𝐼 𝕊) TestsOut () IO)
      fₗ ts = MU $ eachOn ts $ \ (Test lD uM) → do
        tags ← list ^$ ask
        () :* nPass :* ueE ← io $ runRWST γ () $ unErrorT $ uM ()
        case ueE of
          Inl errD → do
            tell $ testsOutMetricFail tags 1
            tell $ testsOutFailure tags lD errD
          Inr () → do
            tell $ testsOutMetricPass tags nPass
  o ← evalRWST null () $ retOut $ unMU $ fold𝑇DOn tests fₗ fₙ
  when noisy $ \ () → do
    pprint $ ppVertical
      [ ppHeader "TEST METRICS"
      , ppVertical $ mapOn (iter $ testsOutMetrics o) $ \ (tags :* (p :* f)) →
          let src = concat $ inbetween ":" tags
          in ppVertical $ concat
            [ if p ≡ 0 then null𝐼 else single $
                ppHorizontal
                  [ ppFG green $ ppString "PASSED"
                  , ppBD $ ppFG green $ ppString $ alignRight 4 $ show𝕊 p
                  , ppPun $ concat ["» ",src]
                  ]
            , if f ≡ 0 then null else single $
                ppHorizontal
                  [ ppFG red $ ppString "FAILED"
                  , ppBD $ ppFG red $ ppString $ alignRight 4 $ show𝕊 f
                  , ppPun $ concat ["» ",src]
                  ]
            ]

      ]
  when (not $ isEmpty $ iter $ testsOutFailures o) $ \ () → do
    pprint $ ppVertical
      [ ppHeader "FAILED TESTS"
      , pretty $ concat $ mapOn (iter $ testsOutFailures o) $ \ (tags :* lDerrDs) →
          concat $ mapOn lDerrDs $ \ (lD :* errD) → 
            𝐤 (concat $ inbetween ":" tags) $ concat
              [ 𝐤 "loc" $ 𝐯 $ ppFG grayDark lD
              , errD ()
              ]
      ]
    abortIO

newtype TestSection = TestSection { unTestSection ∷ 𝕊 }

testSection ∷ 𝕊 → TH.DecsQ
testSection s = do
  TH.putQ $ TestSection s
  return []

testSectionTags ∷ QIO (𝐿 𝕊)
testSectionTags = do
  sO ← map unTestSection ∘ frhs ^$ TH.getQ
  return $ list $ splitOn𝕊 ":" $ ifNone (const "<toplevel>") sO

test ∷ TH.ExpQ → TH.ExpQ → TH.DecsQ
#ifdef NO_UVMHS_TESTS
test _ _ = return []
#else
test xEQ yEQ = testT @() (TH.unsafeCodeCoerce xEQ) (TH.unsafeCodeCoerce yEQ)
#endif

testT ∷ (Eq a,Pretty a) ⇒ TH.CodeQ a → TH.CodeQ a → TH.DecsQ
testT xE yE = do
  l ← TH.location
  let lS = concat [frhsChars $ TH.loc_module l,":",show𝕊 $ fst $ frhs $ TH.loc_start l]
  tags ← testSectionTags
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  let t = [|| eqTest tags lS $$xE $$yE ||]
      tests' = tests ⧺ single t
  TH.putQ @(𝐼 (TH.CodeQ (𝑇D Test))) tests'
  return []

prop ∷ TH.ExpQ → TH.ExpQ → TH.ExpQ → TH.DecsQ
#ifdef NO_UVMHS_TESTS
prop _ _ _ = return []
#else
prop xIO p xD = propT @() (TH.unsafeCodeCoerce xIO) (TH.unsafeCodeCoerce p) $ TH.unsafeCodeCoerce xD
#endif

propT ∷ (Pretty a) ⇒ TH.CodeQ a → TH.CodeQ (a → 𝔹) → TH.CodeQ (a → Doc) → TH.DecsQ
propT xIOE pE xDE = do
  l ← TH.location
  let lS = concat
        [ frhsChars $ TH.loc_module l
        , ":"
        , show𝕊 $ fst $ frhs $ TH.loc_start l
        ]
  tags ← testSectionTags
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  let t' = [|| propTest tags lS $$xIOE $$pE $$xDE ||]
      tests' = tests ⧺ single t'
  TH.putQ @(𝐼 (TH.CodeQ (𝑇D Test))) tests'
  return []

fuzz ∷ TH.ExpQ → TH.ExpQ → TH.ExpQ → TH.DecsQ
#ifdef NO_UVMHS_TESTS
fuzz _ _ _ = return []
#else
fuzz xIO p xD = fuzzT @() (TH.unsafeCodeCoerce xIO) (TH.unsafeCodeCoerce p) $ TH.unsafeCodeCoerce xD
#endif

fuzzT ∷ (Pretty a,Shrinky a) ⇒ TH.CodeQ (FuzzyM a) → TH.CodeQ (a → 𝔹) → TH.CodeQ (a → Doc) → TH.DecsQ
fuzzT xIOE pE xDE = do
  l ← TH.location
  let lS = concat
        [ frhsChars $ TH.loc_module l
        , ":"
        , show𝕊 $ fst $ frhs $ TH.loc_start l
        ]
  tags ← testSectionTags
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  let t' = [|| fuzzTest tags lS $$xIOE $$pE $$xDE ||]
      tests' = tests ⧺ single t'
  TH.putQ @(𝐼 (TH.CodeQ (𝑇D Test))) tests'
  return []

buildTests ∷ TH.DecsQ
buildTests = do
  testEQs ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  l ← TH.location
  let modNameS = frhsChars $ TH.loc_module l
      testsNameS = "g__TESTS__" ⧺ replace𝕊 "." "__" modNameS
      testsName = TH.mkName $ tohsChars testsNameS
      testEQs' ∷ TH.CodeQ [𝑇D Test]
      testEQs' = TH.Code $ TH.TExp ^$ TH.listE $ lazyList $ map TH.unTypeCode testEQs
      testsEQ ∷ TH.CodeQ (𝑇D Test)
      testsEQ = [|| concat $$testEQs' ||]
  concat ^$ exchange $
    [ single ^$ TH.sigD testsName [t| 𝑇D Test |]
    , [d| $(TH.varP testsName) = $(TH.unTypeCode testsEQ) |]
    ]

testModules ∷ 𝔹 → FuzzParams → [𝕊] → TH.CodeQ (IO ())
testModules noisy γ nsS =
  let nss = map (splitOn𝕊 ":") nsS
      testsNamesS = mapOn nss $ \ ns →
        concat $ inbetween "." $ mapLastOn ns $ \ n → "g__TESTS__" ⧺ replace𝕊 "." "__" n
      testsNames = mapOn testsNamesS $ \ testsNameS → TH.mkName $ tohsChars testsNameS
      testNamesE = mapOn testsNames $ \ testsName → TH.varE testsName
      testsEQ ∷ TH.CodeQ [𝑇D Test]
      testsEQ = TH.Code $ TH.TExp ^$ TH.listE $ lazyList testNamesE
  in
  [|| runTests noisy γ $ concat $$testsEQ ||]
