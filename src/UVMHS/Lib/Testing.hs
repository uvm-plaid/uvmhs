{-# LANGUAGE CPP #-}
module UVMHS.Lib.Testing
  ( module UVMHS.Lib.Testing
  ) where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.TreeNested
import UVMHS.Lib.THLiftInstances ()

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

data Test = Test
  { testSrcLoc ∷ Doc
  , testValIter ∷ ℕ64
  , testResult ∷ IO ((() → Doc) ∧ (() → 𝔹))
  }

eqTest ∷ (Eq a,Pretty a) ⇒ 𝐿 𝕊 → 𝕊 → a → a → 𝑇D Test
eqTest tags lS x y =
  let lD = ppString lS
      valD x' y' = pretty $ ppVertical
        [ ppCxt "L" $ pretty x'
        , ppCxt "R" $ pretty y'
        ]
  in keys𝑇D tags $ val𝑇D $ Test lD 1 $ return $ (\ () → valD x y) :* (\ () → x ≡ y)

fuzzTest ∷ (Pretty a) ⇒ 𝐿 𝕊 → 𝕊 → ℕ64 → IO a → (a → 𝔹) → 𝑇D Test
fuzzTest tags lS k xM p = do
  let lD = ppString lS
      valD x = pretty $ concat
        [ key𝑇D "X" $ val𝑇D $ pretty x
        ]
  keys𝑇D tags $ val𝑇D $ Test lD k $ do
    x ← xM
    return $ (\ () → valD x) :* (\ () → p x)

data TestsOut = TestsOut
  { testsOutFailures ∷ 𝐿 𝕊 ⇰ 𝐼 (Doc ∧ Doc)
  , testsOutMetrics ∷ 𝐿 𝕊 ⇰ ℕ ∧ ℕ
  }

instance Null TestsOut where
  null = TestsOut null null
instance Append TestsOut where
  TestsOut f₁ m₁ ⧺ TestsOut f₂ m₂ = TestsOut (f₁ ⧺ f₂) $ m₁ ⧺ m₂
instance Monoid TestsOut

runTests ∷ 𝔹 → 𝑂 ℕ64 → 𝔻 → 𝑇D Test → IO ()
runTests verb ceilO scale tests = do
  pprint $ ppComment "running tests…"
  oflush
  let fₗ ts = MU $ eachOn ts $ \ (Test lD k valdpIO) → do
        let k' = elim𝑂 (const id) (⊓) ceilO $ natΩ64 $ floor $ dbl k × scale
        eachOn (upto k') $ const $ do
          valD :* p ← io valdpIO
          let b = p ()
          tags ← list ∘ reverse ^$ ask
          if b
          then do
            when verb $ \ () →
              io $ pprint $ ppHorizontal
                [ ppFG teal $ ppBD $ ppString $ concat $ inbetween ":" tags
                , ppFG green $ ppString "PASS"
                , ppFG grayDark lD
                ]
            tell $ TestsOut null $ tags ↦ (one :* zero)
          else do
            when verb $ \ () →
              io $ pprint $ ppHorizontal
                [ ppFG teal $ ppBD $ ppString $ concat $ inbetween ":" tags
                , ppFG red $ ppString "FAIL"
                , ppFG grayDark lD
                ]
            tell $ TestsOut (tags ↦ single (lD :* valD ())) $ tags ↦ (zero :* one)
      fₙ gr uM = MU $ mapEnv (gr :&) $ unMU uM
  o ← evalWriterT $ runReaderT Nil $ retOut $ unMU $ fold𝑇DOn tests fₗ fₙ
  pprint $ ppVertical
    [ ppHeader "TEST METRICS"
    , ppVertical $ mapOn (iter $ testsOutMetrics o) $ \ (tags :* (p :* f)) →
        let src = concat $ inbetween ":" tags
        in ppVertical $ concat
          [ if p ≡ 0 then null𝐼 else single $
              ppHorizontal
                [ ppFG green $ ppString "PASSED"
                , ppBD $ ppFG green $ ppString $ alignRight (𝕟 3) $ show𝕊 p
                , ppPun $ concat ["» ",src]
                ]
          , if f ≡ 0 then null else single $
              ppHorizontal
                [ ppFG red $ ppString "FAILED"
                , ppBD $ ppFG red $ ppString $ alignRight (𝕟 3) $ show𝕊 f
                , ppPun $ concat ["» ",src]
                ]
          ]

    ]
  when (not $ isEmpty $ iter $ testsOutFailures o) $ \ () →
    pprint $ ppVertical
      [ ppHeader "FAILED TESTS"
      , pretty $ concat $ mapOn (iter $ testsOutFailures o) $ \ (tags :* lsds) →
          concat $ mapOn lsds $ \ (lD :* valD) →
            key𝑇A (concat $ inbetween ":" tags) $ concat
              [ key𝑇A "loc" $ val𝑇A $ ppFG grayDark lD
              , key𝑇A "val" $ val𝑇A valD
              ]
      ]

𝔱 ∷ 𝕊 → TH.ExpQ → TH.ExpQ → TH.Q [TH.Dec]
#ifdef UVMHS_TESTS
𝔱 tag xEQ yEQ = 𝔱T @() tag (TH.unsafeCodeCoerce xEQ) (TH.unsafeCodeCoerce yEQ)
#else
𝔱 _ _ _ = return []
#endif

𝔱T ∷ (Eq a,Pretty a) ⇒ 𝕊 → TH.CodeQ a → TH.CodeQ a → TH.Q [TH.Dec]
𝔱T tag xE yE = do
  l ← TH.location
  let lS = concat [frhsChars $ TH.loc_module l,":",show𝕊 $ fst $ frhs $ TH.loc_start l]
  let tags = list $ splitOn𝕊 ":" tag
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  let t = [|| eqTest tags lS $$xE $$yE ||]
      tests' = tests ⧺ single t
  TH.putQ @(𝐼 (TH.CodeQ (𝑇D Test))) tests'
  return []

𝔣 ∷ 𝕊 → ℕ64 → TH.ExpQ → TH.ExpQ → TH.Q [TH.Dec]
#ifdef UVMHS_TESTS
𝔣 tag k xIO p = 𝔣T @() tag k (TH.unsafeCodeCoerce xIO) (TH.unsafeCodeCoerce p)
#else
𝔣 _ _ _ _ = return []
#endif

𝔣T ∷ (Pretty a) ⇒ 𝕊 → ℕ64 → TH.CodeQ (IO a) → TH.CodeQ (a → 𝔹) → TH.Q [TH.Dec]
𝔣T tag k xIOE pE = do
  l ← TH.location
  let lS = concat
        [ frhsChars $ TH.loc_module l
        , ":"
        , show𝕊 $ fst $ frhs $ TH.loc_start l
        ]
  let tags = list $ splitOn𝕊 ":" tag
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.CodeQ (𝑇D Test)))
  let t' = [|| fuzzTest tags lS k $$xIOE $$pE ||]
      tests' = tests ⧺ single t'
  TH.putQ @(𝐼 (TH.CodeQ (𝑇D Test))) tests'
  return []

buildTests ∷ TH.Q [TH.Dec]
buildTests = do
  testEQs ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @(𝐼 (TH.Code TH.Q (𝑇D Test)))
  l ← TH.location
  let modNameS = frhsChars $ TH.loc_module l
      testsNameS = "g__TESTS__" ⧺ replace𝕊 "." "__" modNameS
      testsName = TH.mkName $ tohsChars testsNameS
      testEQs' ∷ TH.Code TH.Q [𝑇D Test]
      testEQs' = TH.Code $ TH.TExp ^$ TH.listE $ lazyList $ map TH.unTypeCode testEQs
      testsEQ ∷ TH.Code TH.Q (𝑇D Test)
      testsEQ = [|| concat $$testEQs' ||]
  concat ^$ exchange $
    [ single ^$ TH.sigD testsName [t| 𝑇D Test |]
    , [d| $(TH.varP testsName) = $(TH.unTypeCode testsEQ) |]
    ]

testModules ∷ 𝔹 → 𝑂 ℕ64 → 𝔻 → [𝕊] → TH.Code TH.Q (IO ())
testModules verb ceilO scale nsS =
  let nss = map (splitOn𝕊 ":") nsS
      testsNamesS = mapOn nss $ \ ns →
        concat $ inbetween "." $ mapLastOn ns $ \ n → "g__TESTS__" ⧺ replace𝕊 "." "__" n
      testsNames = mapOn testsNamesS $ \ testsNameS → TH.mkName $ tohsChars testsNameS
      testNamesE = mapOn testsNames $ \ testsName → TH.varE testsName
      testsEQ ∷ TH.Code TH.Q [𝑇D Test]
      testsEQ = TH.Code $ TH.TExp ^$ TH.listE $ lazyList testNamesE
  in
  [|| runTests verb ceilO scale $ concat $$testsEQ ||]
