{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Lib.Substitution.Tests

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as V

import UVMHS.Lang.ULC

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  rngSeed 0
  $$(testModules True (fuzzParamsSml 10)
    [ "UVMHS.Tests.Core"
    , "UVMHS.Lib.Substitution.Tests"
    ])
  -- eachOn (upto 100) $ \ s → do
  --   rngSeed s
  --   pprint $ 𝐤 "SEED" $ 𝐯 $ pretty s
  --   $$(testModules False fuzzParamsTny
  --     [ "UVMHS.Tests.Core"
  --     , "UVMHS.Tests.Substitution"
  --     ])
