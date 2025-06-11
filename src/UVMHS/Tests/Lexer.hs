module UVMHS.Tests.Lexer (lexer,g__TESTS__UVMHS__Tests__Lexer) where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Testing

syntax ∷ LexerWSBasicSyntax
syntax = concat
  [ lexerWSBasicSyntaxPunsMk   $ pow ["(",")"]
  , lexerWSBasicSyntaxOprsMk   $ pow ["+"]
  , lexerWSBasicSyntaxBlocksMk $ pow ["local"]
  ]

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

lexerTest ∷ 𝕊 → 𝕊
lexerTest s = ppshow $ viewΩ inrL $ map renderParserTokens $ tokenizeWSAnchored lexer "" $ tokens s

𝔱 "lexer" 
  [| lexerTest "a\nb\nc" |] 
  [| concat $ inbetween "\n" 
       [ "a"
       , "‣b"
       , "‣c" 
       ]
  |]

buildTests
