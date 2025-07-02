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

test ∷ IO ()
test = do
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

dev ∷ IO ()
dev = cleanExit $ do
  noExit test

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
