module Lang.SimpleArith where

import UVMHS

-- Sample Expression Language

data ExpToken =
    ETWhitespace 𝕊
  | ETLParen
  | ETRParen
  | ETSymbol 𝕊
  | ETNatural ℕ
  | ETPlus     -- infixr   5
  | ETTimes    -- infixr   6
  | ETPower    -- infixl   7
  | ETFact     -- postfix  7
  | ETNegative -- prefix   8
  | ETEqual    -- infix    5
makePrisms ''ExpToken
makePrettySum ''ExpToken

tokExp ∷ 𝐿 (Parser ℂ ExpToken)
tokExp =
  list
  [ construct eTWhitespaceL ∘ string ^$ pOneOrMore $ pSatisfies "space" isSpace 
  , construct eTLParenL ^$ void $ pLit '('
  , construct eTRParenL ^$ void $ pLit ')'
  , construct eTSymbolL ∘ string ^$ pOneOrMore $ pSatisfies "letter" isLetter
  , construct eTNaturalL ^$ pNatural -- 𝕤read ∘ 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "digit" isDigit
  , construct eTPlusL ^$ void $ pLit '+'
  , construct eTTimesL ^$ void $ pLit '*'
  , construct eTPowerL ^$ void $ pLit '^'
  , construct eTFactL ^$ void $ pLit '!'
  , construct eTNegativeL ^$ void $ pLit '-'
  , construct eTEqualL ^$ void $ pLit '='
  ]

testTokExpSuccess ∷ IO ()
testTokExpSuccess = tokenizeIOMain tokExp $ tokens "1 + 2 - 3 * 4 ^ 5 ! = 1"

data Atom =
    ASymbol 𝕊
  | ANatural ℕ
makePrettySum ''Atom

data Exp =
    EAtom Atom
  | ESum Exp Exp
  | EProduct Exp Exp
  | EExpo Exp Exp
  | EFact Exp
  | ENegate Exp
  | EEquality Exp Exp
makePrisms ''Exp
makePrettySum ''Exp

parseExp ∷ Parser ExpToken Exp 
parseExp = pNew "exp" $ mixfixParser $ concat
  [ mix $ Terminal $ do
      void $ pSatisfies "lparen" $ shape eTLParenL
      x ← parseExp
      void $ pSatisfies "rparen" $ shape eTRParenL
      return x
  , mix $ Terminal $ EAtom ∘ ASymbol ^$ pShaped "symbol" $ view eTSymbolL
  , mix $ Terminal $ EAtom ∘ ANatural ^$ pShaped "natural" $ view eTNaturalL
  , mix $ Infr 5 $ const ESum ^$ surroundWhitespace $ pShaped "plus" $ view eTPlusL
  , mix $ Infr 6 $ const EProduct ^$ surroundWhitespace $ pShaped "times" $ view eTTimesL
  , mix $ Infl 7 $ const EExpo ^$ surroundWhitespace $ pShaped "power" $ view eTPowerL
  , mix $ Post 7 $ const EFact ^$ preWhitespace $ pShaped "fact" $ view eTFactL
  , mix $ Pre  8 $ const ENegate ^$ postWhitespace $ pShaped "neg" $ view eTNegativeL
  , mix $ Inf  5 $ const EEquality ^$ surroundWhitespace $ pShaped "equal" $ view eTEqualL
  ]
  where
    surroundWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    surroundWhitespace xM = do
      void $ pOptional $ pSatisfies "whitespace" $ shape eTWhitespaceL
      x ← xM
      void $ pOptional $ pSatisfies "whitespace" $ shape eTWhitespaceL
      return x
    preWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    preWhitespace xM = do
      void $ pOptional $ pSatisfies "whitespace" $ shape eTWhitespaceL
      xM
    postWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    postWhitespace xM = do
      x ← xM
      void $ pOptional $ pSatisfies "whitespace" $ shape eTWhitespaceL
      return x

testParseExpSuccess ∷ IO ()
testParseExpSuccess = parseIOMain parseExp ∘ stream *$ tokenizeIO tokExp $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParseExpFailure1 ∷ IO ()
testParseExpFailure1 = parseIOMain parseExp ∘ stream *$ tokenizeIO tokExp $ tokens "((9 = ((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParseExpFailure2 ∷ IO ()
testParseExpFailure2 = parseIOMain parseExp ∘ stream *$ tokenizeIO tokExp $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ ! = 0))"
