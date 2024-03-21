module UVMHS.Lib.Pretty.Examples where

import UVMHS.Core

import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Class
import UVMHS.Lib.Pretty.Core

-- Tests

testPrettyFormats ∷ Doc
testPrettyFormats = ppVertical $ list
  [ ppPun "punctuation"
  , ppKeyPun "keyword punctuation"
  , ppKey "keyword"
  , ppCon "constructor"
  , ppOp "operator"
  , ppBdr "binder"
  , ppLit "literal"
  , ppHl "highlighted"
  , ppHeader "header"
  , ppErr "error"
  ]

testPrettyNesting ∷ Doc
testPrettyNesting = ppLineNumbers $ ppVertical $ list
  [ pretty $ list
      [ dict [ 111 ↦ pow [10000,11111,22222,33333,44444,55555,66666,77777]
             , 222 ↦ pow [10000,11111,22222,33333,44444,55555,66666,77777,88888]
             , 333 ↦ pow [10000,11111,22222,33333,44444,55555,66666,77777,88888,99999]
             ]
      ]
  ]

testPrettyUndertags ∷ Doc
testPrettyUndertags = ppVertical $ list
  [ ppText "not undertagged"
  , ppUT '~' green $ ppText "undertagged green"
  , ppUT '^' blue $ ppVertical $ list
      [ ppText "multiline"
      , ppText "undertagged"
      , ppFG darkPink $ ppText "with color inside"
      ]
  ]

testPrettyLineNumbers ∷ Doc
testPrettyLineNumbers = ppVertical $ list
  [ ppLineNumbers $ ppText "show lines"
  , ppText "don't show lines"
  , ppLineNumbers $ ppVertical $ list
      [ ppText "multiline"
      , ppText "show lines"
      ]
  ]

testPrettyBlinders ∷ Doc
testPrettyBlinders =
  let lines ∷ 𝐿 Doc
      lines = list $ map (\ (i :* p) → ppHorizontal $ list [p,ppNoFormat $ pretty i]) $ withIndex $ repeat 30 (ppText "line number")
  in ppLineNumbers $ ppBlinders 10 20 $ ppVertical $ lines

testGrouping ∷ Doc
testGrouping = applyN 70 null $ \ d → ppGroup $ concat [ppText "x",ppIfFlat null ppNewline,d]
