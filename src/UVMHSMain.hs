{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution
import UVMHS.Tests.Deriving
import UVMHS.Tests.Lexer
import UVMHS.Tests.Pretty

import qualified Prelude as HS

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

main ∷ IO ()
main = out "<UVMHS>"

mainTest ∷ IO ()
mainTest = do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  rngSeed 0
  $$(testModules True (fuzzParamsSml 10)
    [ "UVMHS.Tests.Core"
    , "UVMHS.Tests.Substitution"
    , "UVMHS.Tests.Deriving"
    , "UVMHS.Tests.Lexer"
    , "UVMHS.Tests.Pretty"
    ])
  -- eachOn (upto 100) $ \ s → do
  --   rngSeed s
  --   pprint $ 𝐤 "SEED" $ 𝐯 $ pretty s
  --   $$(testModules False fuzzParamsTny
  --     [ "UVMHS.Tests.Core"
  --     , "UVMHS.Tests.Substitution"
  --     , "UVMHS.Tests.Deriving"
  --     , "UVMHS.Tests.Lexer"
  --     ])

mainDev ∷ IO ()
mainDev = cleanExit $ do
  noExit mainTest
  -- let d₁ = concat
  --       [ ppForceBreak
  --       , ppSpaces 2
  --       ]
  --     d₁' = ppSpaces 2
  --     d₂ = concat
  --       [ ppA $ concat
  --           [ ppString "A"
  --           , ppNewline
  --           , ppString "B"
  --           ]
  --       , ppSpacesIfBreak 2
  --       ]
  --     d₃ = concat [d₁,d₂]
  -- --     d₃ = ppA ppNewline ⧺ d₂
  -- --     d₄ = ppString "AAA" ⧺ d₃
  --     doit d' = do
  --       pprintNoFmtNarrow d'
  --       out $ show𝕊 $ docShape d'
  --       out $ show𝕊 $ concat $ iter $ summaryIContents $ staticDocA $ execDoc d'
  --       out $ show𝕊 $ concat $ iter $ execDocA $ execDoc d'
  -- out "<d₁>"
  -- doit d₁
  -- out "<d₁'>"
  -- doit d₁'
  -- out "<d₂>"
  -- doit d₂
  -- out "<d₃>"
  -- doit d₃
  -- out "--------------------------"
  -- pprint d₃
  -- out "<d₂ = ppNewline ⧺ d₁>"
  -- doit d₂
  -- out "<ppA ppNewline>"
  -- doit $ ppA ppNewline
  -- out "<d₃ = ppA ppNewline ⧺ d₂>"
  -- doit d₃
  -- out "<ppString \"AAA\">"
  -- doit $ ppString "AAA"
  -- out "<d₄ = ppString \"AAA\" ⧺ d₃>"
  -- doit d₄
  -- out "<end>"
  -- FUTURE
  -- out $(thShowDecs ds₁)
  -- out $(thShowDecs ds₂)
  -- shout $ $(thShowDecs ds₁) ≡ $(thShowDecs ds₂)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance ["a"] [] ''(∨))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance [] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance ["a"] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''(∧))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''(∨))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''𝑆)
