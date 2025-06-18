module UVMHS.Tests.Lexer (g__TESTS__UVMHS__Tests__Lexer) where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing

syntax ∷ Syntax
syntax = concat
  [ syntaxBrks $ dict
      [ "(" ↦ [","] :* [")"]
      , "[" ↦ [";"] :* ["]"]
      ]
  , syntaxBlks ["local"]
  ]

lexerArgs ∷ 𝔹 → LexerArgs
lexerArgs anchorTL = LexerArgs anchorTL syntax

lexer ∷ 𝔹 → Lexer
lexer = mkLexer ∘ lexerArgs

lexerTest ∷ 𝔹 → 𝕊 → 𝕊
lexerTest anchorTL s = elimChoice ppshow (ppshow ∘ renderParserTokens) $ lex (lexer anchorTL) "<>" s

lexerTestA ∷ 𝕊 → 𝕊
lexerTestA = lexerTest True

lexerTestU ∷ 𝕊 → 𝕊
lexerTestU = lexerTest False

-- ======== --
-- ANCHORED --
-- ======== --

-----------------------
-- top level newline --
-----------------------

𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "c d;"
       , "e f;" 
       , "g h" 
       ]
  |]
-- TODO: how do we want this one to lex? as written? or error?
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "  a b"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "  a b;"
       , "c d;"
       , "e f;" 
       , "g h"
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "  c d"
       , "e f" 
       , "g h" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "  c d;"
       , "e f;" 
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "  e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "c d"
       , "  e f;" 
       , "g h"
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{};"
       , "c d;"
       , "e f;" 
       , "g h"
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "c d;"
       , "e f;" 
       , "g h;"
       , "local{}"
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       , "  i"
       , "  local" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "c d;"
       , "e f;" 
       , "g h;"
       , "local{"
       , "  i;"
       , "  local{}}"
       ]
  |]
𝔱 "lexer:anchored:top-level-newline" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       , "  local" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "c d;"
       , "e f;" 
       , "g h;"
       , "local{"
       , "  local{}}"
       ]
  |]

----------------------
-- block same line  --
----------------------

𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d};"
       , "e f;"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d}"
       , "  e f;"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "      e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d;"
       , "      e f};"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "        e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d"
       , "        e f};"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "      e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d;"
       , "      e f}"
       , "  g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{ c d};"
       , "e f"
       , "  g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    local c d"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    local{ c d}}"
       , "  e f;"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-same-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    local"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    local{}}"
       , "  e f;"
       , "g h" 
       ]
  |]

---------------------
-- block next line --
---------------------

𝔱 "lexer:anchored:block-next-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    c d};"
       , "e f;"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-next-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    c d;"
       , "    e f};"
       , "g h" 
       ]
  |]
𝔱 "lexer:anchored:block-next-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    c d;"
       , "    e f}"
       , "  g h" 
       ]
  |]
𝔱 "lexer:anchored:block-next-line" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "      g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b;"
       , "local{"
       , "    c d;"
       , "    e f"
       , "      g h}" 
       ]
  |]

-- corner cases --

𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "a"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "a"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "  a"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  a"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "a"
       , "b"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "a;"
       , "b"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "  a"
       , "b"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  a;"
       , "b"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "local{}"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "  local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  local{}"
       ]
  |]
𝔱 "lexer:anchored:corner-cases" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ ""
       , "  local"
       , "local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  local{};"
       , "local{}"
       ]
  |]

-- parens --

𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "()"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "()"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{})"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "local(local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "local{(local{})}"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(local(local))"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{(local{})})"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(local"
       , "   local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{"
       , "   local{}})"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(local"
       , "   local"
       , "     local"
       , "       a"
       , "       b)c"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{"
       , "   local{"
       , "     local{"
       , "       a;"
       , "       b}}})c"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "( local "
       , "    local a"
       , "    (b) c"
       , "    (d"
       , "     e)"
       , "  , d"
       , "  )"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "( local{ "
       , "    local{ a};"
       , "    (b) c;"
       , "    (d"
       , "     e)}"
       , "  , d"
       , "  )"
       ]
  |]
𝔱 "lexer:anchored:parens" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(local local,local local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{ local{}},local{ local{}})"
       ]
  |]

-- ========== --
-- UNANCHORED --
-- ========== --

-----------------------
-- top level newline --
-----------------------

𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "c d"
       , "e f" 
       , "g h" 
       ]
  |]
-- TODO: how do we want this one to lex? as written? or error?
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "  a b"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "  a b"
       , "c d"
       , "e f" 
       , "g h"
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "  c d"
       , "e f" 
       , "g h" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "  c d"
       , "e f" 
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "  e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "c d"
       , "  e f" 
       , "g h"
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{}"
       , "c d"
       , "e f" 
       , "g h"
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local{}"
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       , "  i"
       , "  local" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local{"
       , "  i;"
       , "  local{}}"
       ]
  |]
𝔱 "lexer:unanchored:top-level-newline" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local"
       , "  local" 
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       , "local{"
       , "  local{}}"
       ]
  |]

----------------------
-- block same line  --
----------------------

𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "e f" 
       , "g h"
       ] 
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d}"
       , "e f"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d}"
       , "  e f"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "      e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d;"
       , "      e f}"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "        e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d"
       , "        e f}"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "      e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d;"
       , "      e f}"
       , "  g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local c d"
       , "e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{ c d}"
       , "e f"
       , "  g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    local c d"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    local{ c d}}"
       , "  e f"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-same-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    local"
       , "  e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    local{}}"
       , "  e f"
       , "g h" 
       ]
  |]

---------------------
-- block next line --
---------------------

𝔱 "lexer:unanchored:block-next-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    c d}"
       , "e f"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-next-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    c d;"
       , "    e f}"
       , "g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-next-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "  g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    c d;"
       , "    e f}"
       , "  g h" 
       ]
  |]
𝔱 "lexer:unanchored:block-next-line" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "a b"
       , "local"
       , "    c d"
       , "    e f"
       , "      g h" 
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "a b"
       , "local{"
       , "    c d;"
       , "    e f"
       , "      g h}" 
       ]
  |]

-- corner cases --

𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "a"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "a"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "  a"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  a"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "a"
       , "b"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "a"
       , "b"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "  a"
       , "b"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  a"
       , "b"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "local{}"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "  local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  local{}"
       ]
  |]
𝔱 "lexer:unanchored:corner-cases" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ ""
       , "  local"
       , "local"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ ""
       , "  local{}"
       , "local{}"
       ]
  |]

-- parens --

𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "()"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "()"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "(local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{})"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "local(local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "local{(local{})}"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "(local(local))"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{(local{})})"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "(local"
       , "   local)"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{"
       , "   local{}})"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "(local"
       , "   local"
       , "     local"
       , "       a"
       , "       b)c"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "(local{"
       , "   local{"
       , "     local{"
       , "       a;"
       , "       b}}})c"
       ]
  |]
𝔱 "lexer:unanchored:parens" 
  [| lexerTestU $ concat $ inbetween "\n"
       [ "( local "
       , "    local a"
       , "    (b) c"
       , "    (d"
       , "     e)"
       , "  , d"
       , "  )"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "( local{ "
       , "    local{ a};"
       , "    (b) c;"
       , "    (d"
       , "     e)}"
       , "  , d"
       , "  )"
       ]
  |]

-- ======= --
-- FAILURE --
-- ======= --

𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "+ c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "a b"
       , "+ c d"
       , "^"
       , "Expected <token>"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , ") c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "a b;"
       , ") c d"
       , "^"
       , "Expected matching bracket OPEN ‹(› before this bracket CLOSE"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "( c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:4"
       , "One of:"
       , "a b;"
       , "( c dEOF"
       , "     ^^^"
       , "Expected bracket CLOSE ‹)› before END OF INPUT"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , ", c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "a b;"
       , ", c d"
       , "^"
       , "Expected matching bracket OPEN ‹(› before this bracket SEP"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(a b"
       , ") c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "(a b;"
       , ") c d"
       , "^"
       , "Expected bracket CLOSE ‹)› before block SEP triggered by this TOKEN"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "(a b"
       , "c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "(a b;"
       , "c d"
       , "^"
       , "Expected bracket CLOSE ‹)› before block SEP triggered by this TOKEN"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a local (a b"
       , ") c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "a local{ (a b}"
       , ") c d"
       , "^"
       , "Expected bracket CLOSE ‹)› before block CLOSE triggered by this TOKEN"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a local (a b"
       , "c d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:1"
       , "One of:"
       , "a local{ (a b}"
       , "c d"
       , "^"
       , "Expected bracket CLOSE ‹)› before block CLOSE triggered by this TOKEN"
       ]
  |]
𝔱 "lexer:anchored:failure" 
  [| lexerTestA $ concat $ inbetween "\n"
       [ "a b"
       , "  ( c] d"
       ]
  |] 
  [| concat $ inbetween "\n" 
       [ "Parse Failure"
       , "Source:"
       , "> <>"
       , "> line:2 column:6"
       , "One of:"
       , "a b"
       , "  ( c] d"
       , "     ^"
       , "Expected matching bracket CLOSE ‹)› before this bracket CLOSE"
       ]
  |]

buildTests
