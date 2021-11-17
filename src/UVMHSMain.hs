{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS hiding (fromString)

import Data.String (fromString)

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Lib.Testing

import qualified UVMHS.Lib.VariablesNew as V

𝔱 "core:iter" [| isEmpty []         |] [| True  |]
𝔱 "core:iter" [| isEmpty [1]        |] [| False |]
𝔱 "core:iter" [| isEmpty Nil        |] [| True  |]
𝔱 "core:iter" [| isEmpty (1 :& Nil) |] [| False |]
buildTests

main ∷ IO ()
main = cleanExit $ do
  $(testModules False
    [ "UVMHSMain"
    , "V:UVMHS.Lib.VariablesNew"
    ])

