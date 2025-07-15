module Examples.Lang.Arith where

import UVMHS

syntax ∷ Syntax
syntax = concat
  [ syntaxPuns ["(",")"]
  , syntaxOprs ["==","+","*","-","^","!"]
  ]

lExp ∷ Lexer
lExp = mkLexer $ LexerArgs False syntax

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess = lexIOMain lExp "<>" "1 + 2 - 3 * 4 ^ 5 ! == 1 \n -- blah blah \n {- ml {{- ml --}-} -- blah\nb"

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
makePrisms ''ExpPre
makePrettySum ''ExpPre

pLit ∷ Parser Lit
pLit = tries
  [ IntegerL ^$ pTokInt
  , DoubleL ^$ pTokDouble
  , StringL ^$ pTokString
  ]

pAtom ∷ Parser Atom
pAtom = pNewContext "atom" $ tries
  [ LitA ^$ pLit
  , NameA ^$ pTokName
  ]

pExp ∷ Parser Exp
pExp = mixfix "exp" id $ concat
  [ mixTerminal $ do
      pTokSyntax "("
      e ← pExp
      pTokSyntax "("
      return $ extract e
  , mixTerminal       $ AtomE         ^$ pAtom
  , mixInfix   pCMP   $ const (return ∘∘ EqualE ) ^$ pTokSyntax "=="
  , mixInfixR  pPLUS  $ const (return ∘∘ PlusE  ) ^$ pTokSyntax "+"
  , mixInfixR  pTIMES $ const (return ∘∘ TimesE ) ^$ pTokSyntax "*"
  , mixPrefix  pNEG   $ const (return ∘  NegateE) ^$ pTokSyntax "-"
  , mixInfixL  pPOW   $ const (return ∘∘ ExpoE  ) ^$ pTokSyntax "^"
  , mixPostfix pFAC   $ const (return ∘  FactE  ) ^$ pTokSyntax "!"
  ]

testParserSuccess ∷ IO ()
testParserSuccess = lexParseIOMain lExp pExp "<>" "(- 1) + - 2 + 3 * 4 ^ 5 ^ 6 !"

testParserFailure1 ∷ IO ()
testParserFailure1 = lexParseIOMain lExp pExp "<>" "((9 == ((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParserFailure2 ∷ IO ()
testParserFailure2 = lexParseIOMain lExp pExp "<>" "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ ! == 0))"
