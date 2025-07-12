module UVMHS.Tests.Pretty (g__TESTS__UVMHS__Tests__Pretty) where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Rand

import qualified UVMHS.Future.TH.Deriving as Future

Future.createFuzzyInstance [] [] ''ShapeM
Future.createFuzzyInstance [] [] ''Shape
Future.createFuzzyInstance [] [] ''ShapeA

Future.createShrinkyInstance ''ShapeM
Future.createShrinkyInstance ''Shape
Future.createShrinkyInstance ''ShapeA

makePrettySum ''ShapeM
makePrettySum ''Shape
makePrettySum ''ShapeA

testSection "pretty:shape:⧺:lunit"

fuzz
  [| do s ← fuzzy @Shape
        return s
  |]
  [| \ s → null ⧺ s ≡ s |]
  [| \ s → pretty s |]

testSection "pretty:shape:⧺:runit"

fuzz
  [| do s ← fuzzy @Shape
        return s
  |]
  [| \ s → s ⧺ null ≡ s |]
  [| \ s → pretty s |]

testSection "pretty:shape:⧺:assoc"

fuzz
  [| do s₁ ← fuzzy @Shape
        s₂ ← fuzzy @Shape
        s₃ ← fuzzy @Shape
        return (s₁,s₂,s₃)
  |]
  [| \ (s₁,s₂,s₃) → (s₁ ⧺ s₂) ⧺ s₃ ≡ s₁ ⧺ (s₂ ⧺ s₃) |]
  [| \ (s₁,s₂,s₃) → ppVertical
       [ ppCxt "s₁" $ pretty s₁
       , ppCxt "s₂" $ pretty s₂
       , ppCxt "s₃" $ pretty s₃
       ]
  |]

ppColon ∷ Doc → Doc → Doc
ppColon = ppInf 5 $ ppString ":"

ppApp ∷ Doc → Doc → Doc
ppApp = ppAppMLL 10

ppApps ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppApps = ppAppsMLL 10

ppAppOp ∷ Doc → Doc → Doc
ppAppOp = ppInfl 10 $ ppString "@"

ppAppOpTight ∷ Doc → Doc → Doc
ppAppOpTight = ppInflTight 10 $ ppString "@"

ppApp' ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppApp' = ppAppCL 10

ppField ∷ Doc → Doc → Doc
ppField = ppInflTight 20 $ ppPun "."

testit ∷ (𝕊,𝕊) → 𝔹
testit (x,y) = x ≡ y

showit ∷ (𝕊,𝕊) → Doc
showit (x,y) = ppVertical
  [ ppCxt "LHS" $ ppString x
  , ppCxt "RHS" $ ppString y
  ]

testSection "pretty:infix:nested"

prop 
  [| ( ppRenderNoFmtWide $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f x (g y) (h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 15 15 $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f x (g y) (h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 14 14 $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , "(g y)"
         , "(h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , "(g y)"
         , "(h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , "(g"
         , " y)"
         , "(h"
         , " z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $
         ppApp 
           (ppApp 
              (ppApp (ppString "f") $ ppString "x") $ 
              ppApp (ppString "g") $ ppString "y") $
           ppApp (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , "(g"
         , " y)"
         , "(h"
         , " z)"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:exp:space"

prop 
  [| ( ppRenderNoFmtWide $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ": g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ": g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ":"
         , "g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 3 3 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ":"
         , "g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , ":"
         , "g"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , ":"
         , "g"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:space:small"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 3 3 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppColon (ppApp (ppString "f") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:space:large"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 11 11 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 10 10 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppColon (ppApp (ppString "fff") $ ppString "x") $
                 ppApp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:exp:op"

prop 
  [| ( ppRenderNoFmtWide $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 13 13 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 12 12 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ": g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ": g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ":"
         , "g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ":"
         , "g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@ x"
         , ":"
         , "g"
         , "@ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 3 3 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@ x"
         , ":"
         , "g"
         , "@ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@"
         , "x"
         , ":"
         , "g"
         , "@"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNoFmtNarrow $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@"
         , "x"
         , ":"
         , "g"
         , "@"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:op:small"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 13 13 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 12 12 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  :"
         , "  g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  :"
         , "  g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  :"
         , "  g"
         , "  @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  :"
         , "  g"
         , "  @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:op:large"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 15 15 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 14 14 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x"
         , "  :"
         , "  g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff @ x"
         , "  :"
         , "  g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  @ x"
         , "  :"
         , "  g"
         , "  @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  @ x"
         , "  :"
         , "  g"
         , "  @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppColon (ppAppOp (ppString "fff") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "fff"
         , "  @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:exp:narrow"

prop 
  [| ( ppRenderNoFmtWide $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ": g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ": g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ":"
         , "g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 3 3 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ":"
         , "g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@x"
         , ":"
         , "g"
         , "@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 1 1 $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@"
         , "x"
         , ":"
         , "g"
         , "@"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "@"
         , "x"
         , ":"
         , "g"
         , "@"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:narrow"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g"
         , "  @y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 3 3 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppColon (ppAppOpTight (ppString "f") $ ppString "x") $
                 ppAppOpTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @"
         , "  x"
         , "  :"
         , "  g"
         , "  @"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:collection:exp"

prop 
  [| ( ppRenderNoFmtWide $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x"
         , ", g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x"
         , ", g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f"
         , "  x"
         , ", g"
         , "  y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f"
         , "  x"
         , ", g"
         , "  y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:collection:cmd"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x ,"
         , "  g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x ,"
         , "  g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f"
         , "  x ,"
         , "  g"
         , "  y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f"
         , "  x ,"
         , "  g"
         , "  y ]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:collection:exp:bulky"

prop 
  [| ( ppRenderNoFmtWide $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 11 11 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 10 10 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x"
         , " , g y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x"
         , " , g y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f"
         , "   x"
         , " , g"
         , "   y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f"
         , "   x"
         , " , g"
         , "   y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:collection:cmd:bulky"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 11 11 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 10 10 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g"
         , "   y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f"
         , "   x ,"
         , "   g"
         , "   y |]"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApp (ppString "f") $ ppString "x"
           , ppApp (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f"
         , "   x ,"
         , "   g"
         , "   y |]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:record:exp"

prop 
  [| ( ppRenderNoFmtWide $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 17 17 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 16 16 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x"
         , ", c d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 11 11 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x"
         , ", c d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 10 10 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b = f x"
         , ", c"
         , "  d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b = f x"
         , ", c"
         , "  d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f x"
         , ", c"
         , "  d ="
         , "    g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f x"
         , ", c"
         , "  d ="
         , "    g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f"
         , "    x"
         , ", c"
         , "  d ="
         , "    g"
         , "    y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f"
         , "    x"
         , ", c"
         , "  d ="
         , "    g"
         , "    y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:record:cmd"

prop 
  [| ( ppRenderNoFmtWide $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 17 17 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 16 16 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x ,"
         , "  c d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 13 13 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x ,"
         , "  c d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 12 12 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b = f x ,"
         , "  c"
         , "  d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 11 11 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b = f x ,"
         , "  c"
         , "  d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 10 10 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f x ,"
         , "  c"
         , "  d ="
         , "    g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f x ,"
         , "  c"
         , "  d ="
         , "    g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f"
         , "    x ,"
         , "  c"
         , "  d ="
         , "    g"
         , "    y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApp (ppString "a") (ppString "b") :* ppApp (ppString "f") (ppString "x")
           , ppApp (ppString "c") (ppString "d") :* ppApp (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a"
         , "  b ="
         , "    f"
         , "    x ,"
         , "  c"
         , "  d ="
         , "    g"
         , "    y ]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:block"

prop 
  [| ( ppRenderNoFmtWide $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x y"
         , "  g z"
         , "  [a,b,c]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 9 9 $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x y"
         , "  g z"
         , "  [a,b,c]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x y"
         , "  g z"
         , "  [ a ,"
         , "    b ,"
         , "    c ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x y"
         , "  g z"
         , "  [ a ,"
         , "    b ,"
         , "    c ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 6 6 $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x"
         , "    y"
         , "  g z"
         , "  [ a ,"
         , "    b ,"
         , "    c ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x"
         , "    y"
         , "  g z"
         , "  [ a ,"
         , "    b ,"
         , "    c ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "do"
         , "  f x"
         , "    y"
         , "  g z"
         , "  [ a ,"
         , "    b ,"
         , "    c ]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:appc:nested:large"

prop 
  [| ( ppRenderNoFmtWide $
         ppApp' (ppString "f")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f(x,g(y),h(z))"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWide $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b(x,g(y),h(z))"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 18 18 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b(x,g(y),h(z))"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 17 17 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  (x,g(y),h(z))"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 15 15 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  (x,g(y),h(z))"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 14 14 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  ( x"
         , "  , g(y)"
         , "  , h(z)"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 8 8 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  ( x"
         , "  , g(y)"
         , "  , h(z)"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 7 7 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 5 5 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f.a.b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 4 4 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f"
         , ".a"
         , ".b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 2 2 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f"
         , ".a"
         , ".b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 1 1 $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f"
         , "."
         , "a"
         , "."
         , "b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $
         ppApp' (ppField (ppField (ppString "f") $ ppString "a") $ ppString "b")
           [ ppString "x"
           , ppApp' (ppString "g") [ppString "y"]
           , ppApp' (ppString "h") [ppString "z"]
           ]
     , concat $ inbetween "\n"
         [ "f"
         , "."
         , "a"
         , "."
         , "b"
         , "  ( x"
         , "  , g( y"
         , "     )"
         , "  , h( z"
         , "     )"
         , "  )"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:block:empty"

prop 
  [| ( ppRenderNoFmtNarrow $ ppParens $
         ppBlock (ppString "do")
           [ ppApps (ppString "f") [ppString "x",ppString "y"]
           , ppApps (ppString "g") [ppString "z"]
           , ppList [ppString "a",ppString "b",ppString "c"]
           ]
     , concat $ inbetween "\n"
         [ "(do"
         , "   f x"
         , "     y"
         , "   g z"
         , "   [ a ,"
         , "     b ,"
         , "     c ])"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ ppParens $
         ppBlock (ppString "do") []
     , concat $ inbetween "\n"
         [ "(do)"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:block:break"

prop 
  [| ( ppRenderNoFmtWide $ 
         ppList [ppBlock (ppString "do") [ppString "x"],ppString "y"]
     , concat $ inbetween "\n"
         [ "[ do"
         , "    x"
         , ", y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ 
         ppList [ppBlock (ppString "do") [ppString "x"],ppString "y"]
     , concat $ inbetween "\n"
         [ "[ do"
         , "    x"
         , ", y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:tricky"

prop 
  [| ( ppRenderNoFmtNarrow $ 
         concat
           [ ppString "AAA"
           , ppA ppNewline
           , ppNewline
           , ppA $ concat [ppString "X",ppNewline,ppString "X"]
           ]
     , concat $ inbetween "\n"
         [ "AAA"
         , "   "
         , "X"
         , "X"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtNarrow $ 
         concat
           [ ppForceBreak
           , ppSpaces 2
           , ppA $ concat
               [ ppString "A"
               , ppNewline
               , ppString "B"
               ]
           , ppSpacesIfBreak 2
           , ppString "C"
           ]
     , concat $ inbetween "\n"
         [ "  A"
         , "  B  C"
         ]
     )
  |] [| testit |] [| showit |]

buildTests
