module UVMHS.Tests.Pretty (g__TESTS__UVMHS__Tests__Pretty) where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing

ppColon ∷ Doc → Doc → Doc
ppColon = ppInf 5 $ ppString ":"

ppApply ∷ Doc → Doc → Doc
ppApply = ppInflSpace 10

ppAppOp ∷ Doc → Doc → Doc
ppAppOp = ppInfl 10 $ ppString "@"

ppAppTight ∷ Doc → Doc → Doc
ppAppTight = ppInflTight 10 $ ppString "@"


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
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f x (g y) (h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 15 15 $
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
     , concat $ inbetween "\n"
         [ "f x (g y) (h z)"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderNoFmtWidth 14 14 $
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
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
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
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
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
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
         ppApply 
           (ppApply 
              (ppApply (ppString "f") $ ppString "x") $ 
              ppApply (ppString "g") $ ppString "y") $
           ppApply (ppString "h") $ ppString "z"
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
  [| ( ppRenderWide $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ": g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ": g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 4 4 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ":"
         , "g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 3 3 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , ":"
         , "g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 2 2 $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "x"
         , ":"
         , "g"
         , "y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:cmd:space"

prop 
  [| ( ppRenderWide $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 7 7 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  : g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 6 6 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 4 4 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 3 3 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 2 2 $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "  x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]


prop 
  [| ( ppRenderNarrow $ ppC $
         ppColon (ppApply (ppString "f") $ ppString "x") $
                 ppApply (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "  x"
         , "  :"
         , "  g"
         , "  y"
         ]
     )
  |] [| testit |] [| showit |]

testSection "pretty:infix:exp:op"

prop 
  [| ( ppRenderWide $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 13 13 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 12 12 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ": g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 7 7 $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , ": g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 6 6 $
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
  [| ( ppRenderWidth 5 5 $
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
  [| ( ppRenderWidth 4 4 $
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
  [| ( ppRenderWidth 3 3 $
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
  [| ( ppRenderWidth 2 2 $
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
  [| ( ppRenderNarrow $
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

testSection "pretty:infix:cmd:op"

prop 
  [| ( ppRenderWide $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 13 13 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 12 12 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f @ x"
         , "  : g @ y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ ppC $
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
  [| ( ppRenderWidth 7 7 $ ppC $
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
  [| ( ppRenderWidth 6 6 $ ppC $
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
  [| ( ppRenderWidth 5 5 $ ppC $
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
  [| ( ppRenderWidth 4 4 $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
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
  [| ( ppRenderNarrow $ ppC $
         ppColon (ppAppOp (ppString "f") $ ppString "x") $
                 ppAppOp (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
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
  [| ( ppRenderWide $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ": g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ": g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 4 4 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ":"
         , "g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 3 3 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , ":"
         , "g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 2 2 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
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
  [| ( ppRenderWidth 1 1 $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
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
  [| ( ppRenderWide $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 7 7 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  : g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 6 6 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g@y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 4 4 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f@x"
         , "  :"
         , "  g"
         , "  @y"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 3 3 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
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
  [| ( ppRenderWidth 2 2 $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
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
  [| ( ppRenderNarrow $ ppC $
         ppColon (ppAppTight (ppString "f") $ ppString "x") $
                 ppAppTight (ppString "g") $ ppString "y"
     , concat $ inbetween "\n"
         [ "f"
         , "  @"
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
  [| ( ppRenderWide $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x"
         , ", g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x"
         , ", g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 4 4 $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $ 
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderWide $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[f x,g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x ,"
         , "  g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 7 7 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[ f x ,"
         , "  g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 6 6 $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $ ppC $
         ppCollection (ppString "[") (ppString "]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderWide $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 11 11 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 10 10 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x"
         , " , g y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 6 6 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x"
         , " , g y"
         , "|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 5 5 $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $ 
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderWide $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 11 11 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[|f x,g y|]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 10 10 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 9 9 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 8 8 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
           ]
     , concat $ inbetween "\n"
         [ "[| f x ,"
         , "   g"
         , "   y |]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 7 7 $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderNarrow $ ppC $
         ppCollection (ppString "[|") (ppString "|]") (ppString ",") 
           [ ppApply (ppString "f") $ ppString "x"
           , ppApply (ppString "g") $ ppString "y"
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
  [| ( ppRenderWide $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 17 17 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 16 16 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x"
         , ", c d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 11 11 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x"
         , ", c d = g y"
         , "]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 10 10 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 9 9 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 8 8 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 7 7 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 6 6 $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderNarrow $ 
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWide $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 17 17 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[a b=f x,c d=g y]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 16 16 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x ,"
         , "  c d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 13 13 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
           ]
     , concat $ inbetween "\n"
         [ "[ a b = f x ,"
         , "  c d = g y ]"
         ]
     )
  |] [| testit |] [| showit |]

prop 
  [| ( ppRenderWidth 12 12 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 11 11 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 10 10 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 9 9 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderWidth 8 8 $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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
  [| ( ppRenderNarrow $ ppC $
         ppCollectionRec (ppString "[") (ppString "]") (ppString ",") (ppString "=")
           [ ppApply (ppString "a") (ppString "b") :* ppApply (ppString "f") (ppString "x")
           , ppApply (ppString "c") (ppString "d") :* ppApply (ppString "g") (ppString "y")
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

buildTests
