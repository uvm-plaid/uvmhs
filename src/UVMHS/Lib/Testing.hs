module UVMHS.Lib.Testing 
  ( module UVMHS.Lib.Testing
  , module Data.String
  ) where

import UVMHS.Core hiding (fromString)
import UVMHS.Lib.Pretty
import UVMHS.Lib.GTree
import UVMHS.Lib.DTree

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Data.String (fromString)

data Test = Test
  { testSourceLoc ∷ Doc
  , testSourceShow ∷ Doc
  , testComparison ∷ Doc
  , testResult ∷ () → IO 𝔹
  }

eqTest ∷ (Eq a,Pretty a) ⇒ 𝐿 𝕊 → 𝕊 → 𝕊 → 𝕊 → a → a → GTree Test
eqTest grs lS xS yS y x =
  let l = ppString lS
      s = pretty $ concat
        [ dtk "L" $ dtv $ ppString xS
        , dtk "R" $ dtv $ ppString yS
        ]
      d = pretty $ concat
        [ dtk "L" $ dtv $ pretty x
        , dtk "R" $ dtv $ pretty y
        ]
      bMF () = return $ x ≡ y
  in gtks grs $ gtv $ Test l s d bMF 

data TestsOut = TestsOut
  { testsOutFailures ∷ 𝐿 𝕊 ⇰ 𝐼 (Doc ∧ Doc ∧ Doc)
  , testsOutMetrics ∷ 𝐿 𝕊 ⇰ ℕ ∧ ℕ
  }

instance Null TestsOut where
  null = TestsOut null null
instance Append TestsOut where
  TestsOut f₁ m₁ ⧺ TestsOut f₂ m₂ = TestsOut (f₁ ⧺ f₂) $ m₁ ⧺ m₂
instance Monoid TestsOut

runTests ∷ 𝔹 → GTree Test → IO ()
runTests verb tests = do
  o ← evalWriterT $ runReaderT Nil $ retOut $ unMU $ foldGTreeOn tests fₗ fₙ
  pprint $ ppVertical
    [ ppHeader "TEST METRICS"
    , pretty $ concat $ mapOn (iter $ testsOutMetrics o) $ \ (grs :* (p :* f)) →
        dtk (concat $ inbetween ":" grs) $ dtv $ ppVertical $ concat
          [ if p ≡ 0 then null𝐼 else single $
              ppHorizontal 
                [ ppFG green $ ppString "PASSED"
                , ppBD $ ppFG green $ ppString $ show𝕊 p
                ]
          , if f ≡ 0 then null else single $ 
              ppHorizontal 
                [ ppFG red $ ppString "FAILED"
                , ppBD $ ppFG red $ ppString $ show𝕊 f
                ]
          ]
       
    ]
  when (not $ isEmpty $ iter $ testsOutFailures o) $
    pprint $ ppVertical
      [ ppHeader "FAILED TESTS"
      , pretty $ concat $ mapOn (iter $ testsOutFailures o) $ \ (grs :* lsds) → 
          concat $ mapOn lsds $ \ (l :* s :* v) → 
            dtk (concat $ inbetween ":" grs) $ concat
              [ dtk "loc" $ dtv $ ppFG grayDark l
              , dtk "src" $ dtv s
              , dtk "val" $ dtv v
              ]
      ]
  where
    fₗ ts = MU $ eachOn ts $ \ (Test l s d bMF) → do
      b ← io $ bMF ()
      grs ← list ∘ reverse ^$ ask
      if b 
         then do
           when verb $
             io $ pprint $ ppHorizontal
               [ ppFG teal $ ppBD $ ppString $ concat $ inbetween ":" grs
               , ppFG green $ ppString "PASS" 
               , ppFG grayDark l
               ]
           tell $ TestsOut null $ grs ↦ (one :* zero)
         else do
           when verb $
             io $ pprint $ ppHorizontal
               [ ppFG teal $ ppBD $ ppString $ concat $ inbetween ":" grs
               , ppFG red $ ppString "FAIL"
               , ppFG grayDark l
               ]
           tell $ TestsOut (grs ↦ single (l :* s :* d)) $ grs ↦ (zero :* one)
    fₙ gr uM = MU $ mapEnv (gr :&) $ unMU uM

data EqTest = EqTest
  { eqTestGroup ∷ 𝐿 𝕊
  , eqTestLoc ∷ 𝕊
  , eqTestLHSShow ∷ 𝕊
  , eqTestRHSShow ∷ 𝕊
  , eqTestLHS ∷ TH.Exp
  , eqTestRHS ∷ TH.Exp
  }

𝔱 ∷ 𝕊 → TH.Q TH.Exp → TH.Q TH.Exp → TH.Q [TH.Dec]
𝔱 grsS xEQ yEQ = do
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @ (𝐿 EqTest)
  l ← TH.location
  let lS = concat
        [ frhsChars $ TH.loc_module l
        , ":"
        , show𝕊 $ fst $ frhs $ TH.loc_start l
        ]
  xE ← xEQ
  yE ← yEQ
  let grs = splitOn𝕊 ":" grsS
      xS = frhsChars $ TH.pprint xE
      yS = frhsChars $ TH.pprint yE
      t = EqTest (list grs) lS xS yS xE yE
  TH.putQ @ (𝐿 EqTest) $ t :& tests
  [d| |]

buildTests ∷ TH.Q [TH.Dec]
buildTests = do
  tests ← ifNone null ∘ frhs𝑂 ^$ TH.getQ @ (𝐿 EqTest)
  l ← TH.location
  let modNameS = frhsChars $ TH.loc_module l 
      testsNameS = "g__TESTS__" ⧺ replace𝕊 "." "__" modNameS
      testEs = mapOn (list $ reverse tests) $ \ (EqTest grp loc xS yS xE yE) → 
        [| eqTest grp loc xS yS $(return xE) $(return yE) |]
      testsName = TH.mkName $ tohsChars testsNameS
  concat ^$ exchange $
    [ single ^$ TH.sigD testsName [t|GTree Test|]
    , [d| $(TH.varP testsName) = concat $(TH.listE $ tohs testEs) |]
    ]
    
testModules ∷ 𝔹 → [𝕊] → TH.Q TH.Exp
testModules verb nsS = do
  let nss = map (splitOn𝕊 ":") nsS
      testsNamesS = mapOn nss $ \ ns → 
        concat $ inbetween "." $ mapLastOn ns $ \ n → "g__TESTS__" ⧺ replace𝕊 "." "__" n
      testsNames = mapOn testsNamesS $ \ testsNameS → TH.mkName $ tohsChars testsNameS
      testNamesE = mapOn testsNames $ \ testsName → TH.varE testsName
  [| runTests verb $ concat $(TH.listE $ tohs testNamesE) |]
