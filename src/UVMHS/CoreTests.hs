module UVMHS.CoreTests where

import UVMHS.Core
import UVMHS.Lib.Testing

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

𝔱 "core:dict" [| dict [𝕟 1 ↦ 𝕟 2,𝕟 1 ↦ 𝕟 3] |] [| dict [𝕟 1 ↦ 𝕟 2] |]

𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("x" ↦ 𝕟 1) |] [| "x" ↦ 𝕟 2 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("y" ↦ 𝕟 1) |] [| "y" ↦ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) (dict ["x" ↦ 𝕟 10,"y" ↦ 𝕟 20]) |] 
              [| dict ["x" ↦ 𝕟 11,"y" ↦ 𝕟 20] |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("x" ↦ 𝕟 1) |] [| dø |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("y" ↦ 𝕟 1) |] [| "y" ↦ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) (dict ["x" ↦ 𝕟 10,"y" ↦ 𝕟 20]) |] 
              [| dict ["y" ↦ 𝕟 20] |]

buildTests
