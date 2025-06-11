module UVMHS.Tests.Lexer (lexer,g__TESTS__UVMHS__Tests__Lexer) where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing

syntax ∷ LexerWSBasicSyntax
syntax = concat
  [ lexerWSBasicSyntaxPunsMk   $ pow ["(",")"]
  , lexerWSBasicSyntaxBlocksMk $ pow ["local"]
  ]

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

lexerTestA ∷ 𝕊 → 𝕊
lexerTestA s = ppshow $ viewΩ inrL $ map renderParserTokens $ tokenizeWSAnchored lexer "" $ tokens s

lexerTestU ∷ 𝕊 → 𝕊
lexerTestU s = ppshow $ viewΩ inrL $ map renderParserTokens $ tokenizeWSUnanchored lexer "" $ tokens s

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

buildTests
