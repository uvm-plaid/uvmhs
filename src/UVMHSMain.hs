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

import UVMHS.Lang.ULCD

𝔱 "core:iter" [| isEmpty []           |] [| True  |]
𝔱 "core:iter" [| isEmpty [𝕟 1]        |] [| False |]
𝔱 "core:iter" [| isEmpty Nil          |] [| True  |]
𝔱 "core:iter" [| isEmpty (𝕟 1 :& Nil) |] [| False |]

𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 0) |] [| list [] |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 1) |] [| list [] |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 1) |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 3) |] [| list [𝕟 1,𝕟 2] |]

𝔱 "core:iter" [| list $ upTo (𝕟 0) |] [| list []  |]
𝔱 "core:iter" [| list $ upTo (𝕟 1) |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ upTo (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ keepN (𝕟 0) [𝕟 0,𝕟 1] |] [| list [] |]
𝔱 "core:iter" [| list $ keepN (𝕟 1) [𝕟 0,𝕟 1] |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ keepN (𝕟 2) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ keepN (𝕟 3) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ replicate (𝕟 0) $ 𝕟 42 |] [| list [] |]
𝔱 "core:iter" [| list $ replicate (𝕟 2) $ 𝕟 42 |] [| list [𝕟 42,𝕟 42] |]

buildTests

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  $$(testModules False
    [ "UVMHSMain"
    , "V:UVMHS.Lib.VariablesNew"
    ])
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"

  -- pprint *$ mapMOn (upTo $ 𝕟64 10) $ const $ rand @ ULCDExpR (𝕟64 5) $ 𝕟64 5
  -- pprint *$ mapMOn (upTo $ 𝕟64 10) $ const $ rand @ (V.Subst ULCDExpR) (𝕟64 5) $ 𝕟64 5
  -- pprint *$ mapMOn (upTo $ 𝕟64 10) $ const $ rwchoose [ 𝕟64 1 :* 'a', 𝕟64 2 :* 'b' ]

