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
      , "|" ↦ [":"] :* ["|"]
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

testSection "lexer:anchored:top-level-newline"

test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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

testSection "lexer:anchored:block-same-line"
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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

testSection "lexer:anchored:block-next-line"

test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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

testSection "lexer:anchored:corner-cases"

test [| lexerTestA $ concat $ inbetween "\n"
          [ ""
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ ""
          , "a"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "a"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ ""
          , "  a"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "  a"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
          [ ""
          , "local"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "local{}"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ ""
          , "  local"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "  local{}"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
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

testSection "lexer:anchored:parens"

test [| lexerTestA $ concat $ inbetween "\n"
          [ "()"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "()"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "(local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{})"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "local(local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "local{(local{})}"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "(local(local))"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{(local{})})"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "(local"
          , "   local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{"
          , "   local{}})"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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

testSection "lexer:unanchored:top-level-newline"

test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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

testSection "lexer:unanchored:block-same-line"
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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

testSection "lexer:unanchored:block-next-line"

test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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

testSection "lexer:unanchored:corner-cases"

test [| lexerTestU $ concat $ inbetween "\n"
          [ ""
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ ""
          , "a"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "a"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ ""
          , "  a"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "  a"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
          [ ""
          , "local"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "local{}"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ ""
          , "  local"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ ""
          , "  local{}"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
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

testSection "lexer:unanchored:parens"

test [| lexerTestU $ concat $ inbetween "\n"
          [ "()"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "()"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ "(local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{})"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ "local(local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "local{(local{})}"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ "(local(local))"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{(local{})})"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
          [ "(local"
          , "   local)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(local{"
          , "   local{}})"
          ]
     |]
test [| lexerTestU $ concat $ inbetween "\n"
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
test [| lexerTestU $ concat $ inbetween "\n"
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

testSection "lexer:anchored:failure"

test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
          [ "a b"
          , "()) c d"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "Parse Failure"
          , "Source:"
          , "> <>"
          , "> line:2 column:3"
          , "One of:"
          , "a b;"
          , "()) c d"
          , "  ^"
          , "Expected matching bracket OPEN ‹(› before this bracket CLOSE"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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
test [| lexerTestA $ concat $ inbetween "\n"
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

-- ========= --
-- PIPE-LIKE --
-- ========= --

testSection "lexer:pipe-like"

test [| lexerTestA $ concat $ inbetween "\n"
          [ "|local|"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "|local{}|"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "(|a(|local|)|)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(|a(|local{}|)|)"
          ]
     |]
test [| lexerTestA $ concat $ inbetween "\n"
          [ "(|a[]|)"
          ]
     |] 
     [| concat $ inbetween "\n" 
          [ "(|a[]|)"
          ]
     |]

testSection "lexer:unanchored:brackets"

test [| lexerTestU $ concat $ inbetween "\n"
          [ "a b [ c"
          , "d ] e f"
          ]
     |]
     [| concat $ inbetween "\n"
          [ "a b [ c"
          , "d ] e f"
          ]
     |]

test [| lexerTestU $ concat $ inbetween "\n"
          [ "a b c"
          , "local"
          , "e ["
          , "f ] g h"
          ]
     |]
     [| concat $ inbetween "\n"
          [ "a b c"
          , "local{}"
          , "e ["
          , "f ] g h"
          ]
     |]

buildTests
