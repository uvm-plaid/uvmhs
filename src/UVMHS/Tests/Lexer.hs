module UVMHS.Tests.Lexer (blockifyArgs,lexer,g__TESTS__UVMHS__Tests__Lexer) where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing
import UVMHS.Lib.Parser.Blockify
import UVMHS.Lib.Parser.Regex (mkIndentTokenWSBasic,blockTWSBasicL)

syntax ∷ LexerWSBasicSyntax
syntax = concat
  [ lexerWSBasicSyntaxPunsMk   $ pow ["(",")",","]
  , lexerWSBasicSyntaxBlocksMk $ pow ["local"]
  ]

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

blockifyArgs ∷ 𝕊 → 𝔹 → 𝑆 (PreParserToken TokenWSBasic) → BlockifyArgs TokenWSBasic
blockifyArgs so anchorTL = BlockifyArgs so anchorTL mkIndentTokenWSBasic (NewlineTWSBasic "\n") (shape blockTWSBasicL) isBracket closeBracket
  where
    isBracket t = (∈♭) t $ pow $ map SyntaxTWSBasic ["(",",",")"]
    closeBracket = SyntaxTWSBasic "(" ↦ BlockifyBracket (single $ SyntaxTWSBasic ",") (single $ SyntaxTWSBasic ")")

lexerTestAOld ∷ 𝕊 → 𝕊
lexerTestAOld s = ppshow $ viewΩ inrL $ map renderParserTokens $ tokenizeWSAnchored lexer "" $ tokens s

lexerTestUOld ∷ 𝕊 → 𝕊
lexerTestUOld s = ppshow $ viewΩ inrL $ map renderParserTokens $ tokenizeWSUnanchored lexer "" $ tokens s

lexerTestANew ∷ 𝕊 → 𝕊
lexerTestANew s = ppshow $ viewΩ inrL $ do
  ts ← tokenize lexer "<>" $ tokens s
  ts' ← blockify $ blockifyArgs "<>" True $ stream ts
  return $ renderParserTokens $ finalizeTokens $ vec ts'

lexerTestANewDebug ∷ 𝕊 → 𝕊
lexerTestANewDebug s = ppshow $ do
  ts ← tokenize lexer "<>" $ tokens s
  ts' ← blockify $ blockifyArgs "<>" True $ stream ts
  return $ renderParserTokens $ finalizeTokens $ vec ts'

lexerTestUNew ∷ 𝕊 → 𝕊
lexerTestUNew s = ppshow $ viewΩ inrL $ do
  ts ← tokenize lexer "<>" $ tokens s
  ts' ← blockify $ blockifyArgs "<>" False $ stream ts
  return $ renderParserTokens $ finalizeTokens $ vec ts'

lexerTestUNewDebug ∷ 𝕊 → 𝕊
lexerTestUNewDebug s = ppshow $ do
  ts ← tokenize lexer "<>" $ tokens s
  ts' ← blockify $ blockifyArgs "<>" False $ stream ts
  return $ renderParserTokens $ finalizeTokens $ vec ts'

lexerTestA ∷ 𝕊 → 𝕊
lexerTestA = lexerTestANew

lexerTestU ∷ 𝕊 → 𝕊
lexerTestU = lexerTestUNew

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

buildTests
