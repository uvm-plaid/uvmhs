{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  $$(testModules False
    [ "UVMHS.Tests.Core"
    , "UVMHS.Tests.Substitution"
    ])
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"
