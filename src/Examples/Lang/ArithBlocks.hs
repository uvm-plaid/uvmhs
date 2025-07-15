module Examples.Lang.ArithBlocks where

import UVMHS

syntax ∷ Syntax
syntax = concat
  [ syntaxBrks $ dict [ "(" ↦ [] :* [")"] ]
  , syntaxOprs ["==","+","*","-","^","!"]
  , syntaxBlks ["local"]
  ]

lExp ∷ Lexer
lExp = mkLexer $ LexerArgs True syntax

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess = lexIOMain lExp "" $ concat $ inbetween "\n"
  [ "1 -- blah"
  , "2"
  , "3 4"
  , "  5"
  , "local 6"
  , "7"
  , "local 8"
  , "      9"
  , "local"
  , "10"
  , "local local "
  , "      local"
  ]

data Lit =
    IntegerL ℤ
  | DoubleL 𝔻
  | StringL 𝕊
makePrettySum ''Lit

data Atom =
    LitA Lit
  | NameA 𝕊
makePrettySum ''Atom

type Exp = 𝐴 SrcCxt ExpPre
data ExpPre =
    AtomE Atom
  | PlusE Exp Exp
  | TimesE Exp Exp
  | ExpoE Exp Exp
  | FactE Exp
  | NegateE Exp
  | EqualE Exp Exp
  | BlockE (𝐿 Exp)
makePrisms ''ExpPre
makePrettySum ''ExpPre

cpLit ∷ Parser Lit
cpLit = tries
  [ IntegerL ^$ pTokInt
  , DoubleL ^$ pTokDouble
  , StringL ^$ pTokString
  ]

cpAtom ∷ Parser Atom
cpAtom = pNewContext "atom" $ tries
  [ LitA ^$ cpLit
  , NameA ^$ pTokName
  ]

cpBlock ∷ Parser (𝐿 Exp)
cpBlock = pNewContext "block" $ do
  pTokBlock "local"
  pTokOpen
  es ← cpExpList
  pTokClose
  return es

cpExp ∷ Parser Exp
cpExp = mixfix "exp" id $ concat
  [ mixTerminal $ do
      pTokSyntax "("
      e ← cpExp
      pTokSyntax ")"
      return $ extract e
  , mixTerminal       $ AtomE         ^$ cpAtom
  , mixInfix   pCMP   $ const (return ∘∘ EqualE ) ^$ pTokSyntax "=="
  , mixInfixR  pPLUS  $ const (return ∘∘ PlusE  ) ^$ pTokSyntax "+"
  , mixInfixR  pTIMES $ const (return ∘∘ TimesE ) ^$ pTokSyntax "*"
  , mixPrefix  pNEG   $ const (return ∘  NegateE) ^$ pTokSyntax "-"
  , mixInfixL  pPOW   $ const (return ∘∘ ExpoE  ) ^$ pTokSyntax "^"
  , mixPostfix pFAC   $ const (return ∘  FactE  ) ^$ pTokSyntax "!"
  , mixTerminal $ BlockE ^$ cpBlock
  ]

cpExpList ∷ Parser (𝐿 Exp)
cpExpList = manySepBy pTokSep cpExp

testParserSuccess ∷ IO ()
testParserSuccess = lexParseIOMain lExp cpExpList "<>" $ concat $ inbetween "\n"
  [ "(- 1) + 2"
  , "local 2 + 3"
  , "      local - 2 + 3"
  , "        * 4 ^ 5 ^ 6 !"
  ]

testParserFailure ∷ IO ()
testParserFailure = lexParseIOMain lExp cpExpList "<>" $ concat $ inbetween "\n"
  [ "(- 1) + 2"
  , "local 2 + 3 + 4"
  , "      local - 2 + 3"
  , "      + 4 ^ 5 ^ 6 !"
  ]
