{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution
import UVMHS.Tests.Deriving

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as V

import UVMHS.Lang.ULC

import qualified Control.Monad.Logic as L

import qualified GHC.IsList as IsList

import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

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
    ])
  -- eachOn (upto 100) $ \ s → do
  --   rngSeed s
  --   pprint $ 𝐤 "SEED" $ 𝐯 $ pretty s
  --   $$(testModules False fuzzParamsTny
  --     [ "UVMHS.Tests.Core"
  --     , "UVMHS.Tests.Substitution"
  --     ])

dev ∷ IO ()
dev = cleanExit $ do
  test
  -- FUTURE
  -- out $(thShowDecs ds₁)
  -- out $(thShowDecs ds₂)
  -- shout $ $(thShowDecs ds₁) ≡ $(thShowDecs ds₂)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance [] ''𝐿)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance [] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance ["a"] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createMonoidInstance ''(∧))
