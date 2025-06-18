module UVMHS.Lib.Parser.StdParser where

import UVMHS.Core

import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.Blockify
import UVMHS.Lib.Parser.GenParser
import UVMHS.Lib.Parser.GenLexer
import UVMHS.Lib.Parser.Mixfix
import UVMHS.Lib.Parser.Regex
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Annotated

-----------
-- REGEX --
-----------

data StdCharClass = 
    Letter_SCC 
  | Number_SCC 
  | Space_SCC 
  | Newline_SCC 
  | Other_SCC
  deriving (Eq,Ord,Show)
makePrettySum ''StdCharClass

instance All StdCharClass where 
  all = iter [Letter_SCC,Number_SCC,Space_SCC,Newline_SCC,Other_SCC]

instance Classified StdCharClass ℂ where
  classify c
    | isLetter c = Letter_SCC
    | isNumber c = Number_SCC
    | isSpace c ⩓ c ≢ '\n' = Space_SCC
    | c ≡ '\n' = Newline_SCC
    | otherwise = Other_SCC

lWord ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ 𝕊 → Regex StdCharClass ℂ o u
lWord = fold eps $ \ c r → r ▷ tokRegex c

lSpaceOrNL ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lSpaceOrNL = oom $ classRegex Space_SCC ⧺ classRegex Newline_SCC

lSpace ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lSpace = oom $ classRegex Space_SCC

lNL ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lNL = oom $ classRegex Newline_SCC

lName ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex StdCharClass ℂ o u
lName =
  let begTok = concat
        [ classRegex Letter_SCC
        , concat $ map tokRegex $ iter $ 𝕤 "_'′″‴"
        ]
      endTok = concat
        [ begTok
        , classRegex Number_SCC
        ]
      midTok = begTok ⧺ endTok ⧺ tokRegex '-'
  in
  sequence
    [ begTok
    , opt $ sequence
        [ star midTok
        , endTok
        ]
    ]

lNatPre ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex StdCharClass ℂ o u
lNatPre = sequence
  [ concat $ map tokRegex ['0'..'9']
  , star $ concat
      [ concat $ map tokRegex ['0'..'9']
      , tokRegex '_'
      ]
  , fepsRegex $ formats [FG red]
  ]

lNat ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex StdCharClass ℂ o u
lNat = sequence
  [ lNatPre
  , lepsRegex $ 𝕟64 102
  ]

lNatCoded ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex StdCharClass ℂ o u
lNatCoded = sequence
  [ lNatPre
  , tokRegex 'n'
  , lepsRegex $ 𝕟64 102
  ]

lIntPre ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lIntPre = sequence
  [ opt $ tokRegex '-'
  , lNatPre
  ]

lInt ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lInt = sequence
  [ lIntPre
  , lepsRegex $ 𝕟64 101
  ]

lDbl ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lDbl = sequence
  [ lIntPre
  , opt $ sequence
    [ tokRegex '.'
    , lNatPre
    ]
  , opt $ sequence
    [ tokRegex 'e'
    , lIntPre
    ]
  , lepsRegex $ 𝕟64 100
  ]

lString ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex StdCharClass ℂ o u
lString = sequence
  [ tokRegex '"'
  , star $ concat
      [ ntokRegex $ pow ['\\','"']
      , lWord "\\\\"
      , lWord "\\\""
      , lWord "\\n"
      ]
  , tokRegex '"'
  , fepsRegex $ formats [FG red]
  ]

lComment ∷ (Ord o) ⇒ Regex StdCharClass ℂ o ℤ64
lComment = sequence
  [ lWord "--"
  , star $ ntokRegex $ single '\n'
  -- , opt $ tokRegex '\n'
  , fepsRegex $ formats [IT,FG grayLight]
  , lepsRegex $ 𝕟64 100
  ]

lCommentMLOpen ∷ (Ord o) ⇒ Regex StdCharClass ℂ o ℤ64
lCommentMLOpen = sequence
  [ lWord "{-"
  , uepsRegex one
  , fepsRegex $ formats [IT,FG grayLight]
  , lepsRegex $ 𝕟64 100
  ]

lCommentMLBodyOpen ∷ (Ord o) ⇒ Regex StdCharClass ℂ o ℤ64
lCommentMLBodyOpen = sequence
  [ oom (tokRegex '{') ▷ tokRegex '-'
  , uepsRegex one
  ]

lCommentMLBodyClose ∷ (Ord o) ⇒ Regex StdCharClass ℂ o ℤ64
lCommentMLBodyClose = sequence
  [ oom (tokRegex '-') ▷ tokRegex '}'
  , uepsRegex $ neg one
  ]

lCommentMLBody ∷ (Ord o) ⇒ Regex StdCharClass ℂ o ℤ64
lCommentMLBody = sequence
  [ star $ concat
      [ ntokRegex $ pow ['-','{']
      , oom (tokRegex '-') ▷ ntokRegex (pow ['-','}'])
      , oom (tokRegex '{') ▷ ntokRegex (pow ['{','-'])
      ]
  , lCommentMLBodyOpen ⧺ lCommentMLBodyClose
  , fepsRegex $ formats [IT,FG grayLight]
  ]

-----------
-- LEXER --
-----------

data TokenClass =
    Space_TC
  | Newline_TC
  | Comment_TC
  | Block_TC
  | String_TC
  | Name_TC
  | Nat_TC
  | Int_TC
  | Dbl_TC
  | Char_TC
  | Syntax_TC
  deriving (Eq,Ord,Show)
makePrisms ''TokenClass
makePrettySum ''TokenClass

data Token =
    Space_T 𝕊
  | Newline_T 𝕊
  | Comment_T 𝕊
  | Block_T 𝕊
  | String_T 𝕊
  | Name_T 𝕊
  | Nat_T ℕ
  | Int_T ℤ
  | Dbl_T 𝔻
  | Char_T ℂ
  | Syntax_T 𝕊
  | BlockOpen_T
  | BlockClose_T
  | BlockSep_T
  deriving (Eq,Ord,Show)
makePrisms ''Token
makePrettySum ''Token

mkToken ∷ 𝐼C ℂ → 𝑂 TokenClass → 𝔹 ∧ Token
mkToken cs = \case
  None → error "no token class"
  Some Space_TC → (:*) True $ Space_T $ stringCS cs
  Some Newline_TC → (:*) True $ Newline_T $ stringCS cs
  Some Comment_TC → (:*) True $ Comment_T $ stringCS cs
  Some Block_TC → (:*) False $ Block_T $ stringCS cs
  Some String_TC → (:*) False $ String_T $ read𝕊 $ stringCS cs
  Some Name_TC → (:*) False $ Name_T $ stringCS cs
  Some Nat_TC → (:*) False $ Nat_T $ read𝕊 $ string $ filter (\ c → c ∉ pow𝑃 ['_','n']) cs
  Some Int_TC → (:*) False $ Int_T $ read𝕊 $ string $ filter ((≢) '_') cs
  Some Dbl_TC → (:*) False $ Dbl_T $ read𝕊 $ string $ filter ((≢) '_') cs
  Some Char_TC → (:*) False $ Char_T $ read𝕊 $ stringCS cs
  Some Syntax_TC → (:*) False $ Syntax_T $ stringCS cs

data Syntax = Syntax
  { syntaxBrkss ∷ 𝕊 ⇰ 𝑃 𝕊 ∧ 𝑃 𝕊  -- ^ brackets       (default color gray)
  , syntaxPunss ∷ 𝑃 𝕊            -- ^ punctuation    (default color gray)
  , syntaxKeyss ∷ 𝑃 𝕊            -- ^ keywords       (default color bold yellow)
  , syntaxPrmss ∷ 𝑃 𝕊            -- ^ primitives     (default color blue)
  , syntaxOprss ∷ 𝑃 𝕊            -- ^ operators      (default color teal)
  , syntaxBlkss ∷ 𝑃 𝕊            -- ^ block keywords (default color bold yellow)
  } deriving (Eq,Ord,Show)
makeLenses ''Syntax

instance Null Syntax where 
  null = Syntax null null null null null null
instance Append Syntax where 
  Syntax brks₁ puns₁ keys₁ prms₁ oprs₁ blks₁ ⧺ Syntax brks₂ puns₂ keys₂ prms₂ oprs₂ blks₂ =
    Syntax (brks₁ ⧺ brks₂) (puns₁ ⧺ puns₂) (keys₁ ⧺ keys₂) (prms₁ ⧺ prms₂) (oprs₁ ⧺ oprs₂) $ blks₁ ⧺ blks₂
instance Monoid Syntax

syntaxBrks ∷ (ToIter 𝕊 t) ⇒ 𝕊 ⇰ t ∧ t → Syntax
syntaxBrks blks = null { syntaxBrkss = map (mapPair pow pow) blks }

syntaxPuns ∷ (ToIter 𝕊 t) ⇒ t → Syntax
syntaxPuns puns = null { syntaxPunss = pow puns }

syntaxKeys ∷ (ToIter 𝕊 t) ⇒ t → Syntax
syntaxKeys keys = null { syntaxKeyss = pow keys }

syntaxPrms ∷ (ToIter 𝕊 t) ⇒ t → Syntax
syntaxPrms prms = null { syntaxPrmss = pow prms }

syntaxOprs ∷ (ToIter 𝕊 t) ⇒ t → Syntax
syntaxOprs oprs = null { syntaxOprss = pow oprs }

syntaxBlks ∷ (ToIter 𝕊 t) ⇒ t → Syntax
syntaxBlks blks = null { syntaxBlkss = pow blks }

regexSyntax ∷ (Ord u,Additive u) ⇒ Syntax → Regex StdCharClass ℂ TokenClass u
regexSyntax (Syntax brks puns keys prms oprs blks) = 
  concat
  -- punctuation and brackets
  [ sequence
    [ concat $ map lWord $ concat
        [ iter puns
        , iter $ unions $ mapOn (iter brks) $ uncurry $ \ open (seps :* closes) → 
            unions [single open,seps,closes]
        ]
    , fepsRegex $ formats [FG grayDark]
    , oepsRegex Syntax_TC
    ]
  -- keywords
  , sequence
    [ concat $ map lWord $ iter keys
    , fepsRegex $ formats [FG yellow,BD]
    , oepsRegex Syntax_TC
    ]
  -- primitives
  , sequence
    [ concat $ map lWord $ iter prms
    , fepsRegex $ formats [FG blue]
    , oepsRegex Syntax_TC
    ]
  -- operators
  , sequence
    [ concat $ map lWord $ iter oprs
    , fepsRegex $ formats [FG teal]
    , oepsRegex Syntax_TC
    ]
  -- blocks
  , sequence
    [ concat $ map lWord $ iter blks
    , fepsRegex $ formats [BG white,FG yellow,BD]
    , oepsRegex Block_TC
    ]
  ]

lToken ∷ Syntax → Regex StdCharClass ℂ TokenClass ℤ64
lToken syntax = 
  concat
  [ lNatCoded         ▷ oepsRegex Nat_TC
  , lInt              ▷ oepsRegex Int_TC
  , lDbl              ▷ oepsRegex Dbl_TC
  , lString           ▷ oepsRegex String_TC
  , lName             ▷ oepsRegex Name_TC
  , lSpace            ▷ oepsRegex Space_TC
  , lNL               ▷ oepsRegex Newline_TC
  , lComment          ▷ oepsRegex Comment_TC
  , lCommentMLOpen    ▷ oepsRegex Comment_TC
  , regexSyntax syntax
  ]

lTokenCommentMLBody ∷ Regex StdCharClass ℂ TokenClass ℤ64
lTokenCommentMLBody = lCommentMLBody ▷ oepsRegex Comment_TC

dfaSyntax ∷ Syntax → ℤ64 → DFA StdCharClass ℂ TokenClass ℤ64
dfaSyntax syntax =
  let dfaTokenBasic = compileRegex $ lToken syntax
      dfaCommentMLBasic = compileRegex lTokenCommentMLBody
      dfa n | n ≡ 0 = dfaTokenBasic
            | n > 0 = dfaCommentMLBasic
            | otherwise = error "impossible"
  in dfa

data LexerArgs = LexerArgs
  { lexerArgsAnchorTL ∷ 𝔹
  , lexerArgsSyntax ∷ Syntax
  }

data Lexer = Lexer 
  { lexerLexerArgs ∷ LexerArgs
  , lexerGenLexer ∷ GenLexer StdCharClass ℂ TokenClass ℤ64 Token 
  }

mkLexer ∷ LexerArgs → Lexer
mkLexer ρ = Lexer ρ $ GenLexer (dfaSyntax $ lexerArgsSyntax ρ) mkToken zero

mkBlockifyToken ∷ BlockifyToken → Token
mkBlockifyToken = \case
  Open_BT → BlockOpen_T
  Close_BT → BlockClose_T
  Sep_BT → BlockSep_T

blockifyArgsLexer ∷ LexerArgs → BlockifyArgs Token
blockifyArgsLexer ρ = BlockifyArgs
  { blockifyArgsAnchorTL = lexerArgsAnchorTL ρ
  , blockifyArgsMkBlockifyToken = mkBlockifyToken
  , blockifyArgsNewlineToken = Newline_T "\n"
  , blockifyArgsIsBlock = shape block_TL
  , blockifyArgsBrackets = 
      dict𝐷 $ 
      mapOn (iter $ syntaxBrkss $ lexerArgsSyntax ρ) $ \ (open :* (seps :* closes)) → 
        let seps' = dict𝐷 $ mapOn (iter seps) $ \ sep → Syntax_T sep ↦ sep
            closes' = dict𝐷 $ mapOn (iter closes) $ \ close → Syntax_T close ↦ close
        in Syntax_T open ↦ (open :* seps' :* closes')
  }

lex ∷ Lexer → 𝕊 → 𝕊 → Doc ∨ 𝕍 (ParserToken Token)
lex l so s = blockify (blockifyArgsLexer $ lexerLexerArgs l) so *$ glex (lexerGenLexer l) so $ tokens s

lexIO ∷ Lexer → 𝕊 → 𝕊 → IO (𝕍 (ParserToken Token))
lexIO l so ts = case lex l so ts of
  Inl err → do 
    pprint $ ppVertical
      [ ppErr "Failure"
      , pretty err
      ]
    abortIO
  Inr ts' → return ts'

lexIOMain ∷ Lexer → 𝕊 → 𝕊 → IO ()
lexIOMain l so ts = do
  xs ← lexIO l so ts
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty $ renderParserTokens xs
    ]

----------------------------
-- Basic Language Parsing --
----------------------------

newtype Parser a = Parser { unParser ∷ GenParser Token a }
  deriving 
  ( Return,Bind,Functor,Monad 
  , MonadFail
  , Null,Append,Monoid
  )

onParser ∷ (GenParser Token a → GenParser Token b) → Parser a → Parser b
onParser f = Parser ∘ f ∘ unParser

deriving instance Eps (Parser ())
deriving instance Seq (Parser ())
deriving instance Seqoid (Parser ())

pRender ∷ Formats → Parser a → Parser a
pRender = onParser ∘ gpRender

pErr ∷ 𝕊 → Parser a → Parser a
pErr = onParser ∘ gpErr

pFinal ∷ Parser a → Parser a
pFinal = onParser gpFinal

pTok ∷ Token → Parser ()
pTok = Parser ∘ gpTok

pTokAny ∷ (ToIter Token t) ⇒ t → Parser ()
pTokAny = Parser ∘ gpTokAny

pTokRet ∷ Token → Parser Token
pTokRet = Parser ∘ gpTokRet

pTokShaped ∷ (Token → 𝑂 a) → Parser a
pTokShaped = Parser ∘ gpTokShaped

pTokSat ∷ (Token → 𝔹) → Parser Token
pTokSat = Parser ∘ gpTokSat

pAny ∷ Parser Token
pAny = Parser gpAny

pDie ∷ Parser a
pDie = Parser gpDie

pGuard ∷ 𝔹 → Parser ()
pGuard = Parser ∘ gpGuard

pFailEff ∷ 𝑂 a → Parser a
pFailEff = Parser ∘ gpFailEff

pNewExpressionContext ∷ Parser a → Parser a
pNewExpressionContext = onParser gpNewExpressionContext

pNewErrContext ∷ 𝕊 → Parser a → Parser a
pNewErrContext = onParser ∘ gpNewErrContext

pNewContext ∷ 𝕊 → Parser a → Parser a
pNewContext = onParser ∘ gpNewContext

pWithContextRendered ∷ Parser a → Parser (𝐴 SrcCxt a)
pWithContextRendered = onParser gpWithContextRendered

pNewWithContextRendered ∷ 𝕊 → Parser a → Parser (𝐴 SrcCxt a)
pNewWithContextRendered = onParser ∘ gpNewWithContextRendered

pGetContextRendered ∷ Parser SrcCxt
pGetContextRendered = Parser gpGetContextRendered

pNewGetContextRendered ∷ Parser SrcCxt
pNewGetContextRendered = Parser gpNewGetContextRendered

pTokName ∷ Parser 𝕊
pTokName = Parser $ gpTokShaped $ view name_TL

pTokSyntax ∷ 𝕊 → Parser ()
pTokSyntax = Parser ∘ gpTok ∘ Syntax_T

pTokSyntaxAny ∷ (ToIter 𝕊 t) ⇒ t → Parser ()
pTokSyntaxAny = Parser ∘ gpTokAny ∘ map Syntax_T ∘ iter

pTokNatN ∷ Parser ℕ
pTokNatN = Parser $ gpTokShaped $ view nat_TL

pTokNatN64 ∷ Parser ℕ64
pTokNatN64 = Parser ∘ failEff ∘ natO64 *$ pTokNatN

pTokInt ∷ Parser ℤ
pTokInt = Parser $ gpTokShaped $ view int_TL

pTokInt64 ∷ Parser ℤ64
pTokInt64 = failEff ∘ intO64 *$ pTokInt

pTokNat ∷ Parser ℕ
pTokNat = failEff ∘ natO *$ pTokInt

pTokNat64 ∷ Parser ℕ64
pTokNat64 = failEff ∘ natO64 *$ pTokInt

pTokDouble ∷ Parser 𝔻
pTokDouble = Parser $ gpTokShaped $ view dbl_TL

pTokString ∷ Parser 𝕊
pTokString = Parser $ gpTokShaped $ view string_TL

pTokChar ∷ Parser ℂ
pTokChar = Parser $ gpTokShaped $ view char_TL

pTokBlock ∷ 𝕊 → Parser ()
pTokBlock = Parser ∘ gpTok ∘ Block_T

pTokOpen ∷ Parser ()
pTokOpen = Parser $ gpTok BlockOpen_T

pTokClose ∷ Parser ()
pTokClose = Parser $ gpTok BlockClose_T

pTokSep ∷ Parser ()
pTokSep = Parser $ gpTok BlockSep_T

parse ∷ (ToIter (ParserToken Token) ts) ⇒ Parser a → 𝕊 → ts → Doc ∨ a
parse p so ts = gparse (unParser p) so ts

parseIO ∷ (ToIter (ParserToken Token) ts) ⇒ Parser a → 𝕊 → ts → IO a
parseIO p so ts = gparseIO (unParser p) so ts

parseIOMain ∷ (Pretty a,ToIter (ParserToken Token) ts) ⇒ Parser a → 𝕊 → ts → IO ()
parseIOMain p so ts = gparseIOMain (unParser p) so ts

lexParse ∷ Lexer → Parser a → 𝕊 → 𝕊 → Doc ∨ Doc ∨ a
lexParse l p so = (mapInl Inr ∘ parse p so) *∘ (mapInl Inl ∘ lex l so)

lexParseIO ∷ Lexer → Parser a → 𝕊 → 𝕊 → IO a
lexParseIO l p so s = parseIO p so *$ lexIO l so s

lexParseIOMain ∷ (Pretty a) ⇒ Lexer → Parser a → 𝕊 → 𝕊 → IO ()
lexParseIOMain l p so s = parseIOMain p so *$ lexIO l so s

------------
-- MIXFIX --
------------

data Mixes c a = Mixes
  { mixesPrefix  ∷ Parser (𝐴 c a → a)
  , mixesPostfix ∷ Parser (𝐴 c a → a)
  , mixesInfix  ∷ Parser (𝐴 c a → 𝐴 c a → a)
  , mixesInfixL ∷ Parser (𝐴 c a → 𝐴 c a → a)
  , mixesInfixR ∷ Parser (𝐴 c a → 𝐴 c a → a)
  }

instance Null (Mixes c a) where null = Mixes null null null null null
instance Append (Mixes c a) where
  Mixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ Mixes pre₂ post₂ inf₂ infl₂ infr₂ =
    Mixes (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) $ infr₁ ⧺ infr₂
instance Monoid (Mixes c a)

data Mixfix c a = Mixfix
  { mixfixTerminals ∷ Parser a
  , mixfixLevels ∷ ℕ64 ⇰ Mixes c a
  }
instance Null (Mixfix c a) where null = Mixfix null null
instance Append (Mixfix c a) where Mixfix ts₁ ls₁ ⧺ Mixfix ts₂ ls₂ = Mixfix (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance Monoid (Mixfix c a)

mixOnlyTerms ∷ Mixfix c a → Mixfix c a
mixOnlyTerms m = Mixfix (mixfixTerminals m) null

mixPrefix ∷ ℕ64 → Parser (𝐴 c a → a) → Mixfix c a
mixPrefix l p = null { mixfixLevels = dict [ l ↦ null { mixesPrefix = p } ] }

mixPostfix ∷ ℕ64 → Parser (𝐴 c a → a) → Mixfix c a
mixPostfix l p = null { mixfixLevels = dict [ l ↦ null { mixesPostfix = p } ] }

mixInfix ∷ ℕ64 → Parser (𝐴 c a → 𝐴 c a → a) → Mixfix c a
mixInfix l p = null { mixfixLevels = dict [ l ↦ null { mixesInfix = p } ] }

mixInfixL ∷ ℕ64 → Parser (𝐴 c a → 𝐴 c a → a) → Mixfix c a
mixInfixL l p = null { mixfixLevels = dict [ l ↦ null { mixesInfixL = p } ] }

mixInfixR ∷ ℕ64 → Parser (𝐴 c a → 𝐴 c a → a) → Mixfix c a
mixInfixR l p = null { mixfixLevels = dict [ l ↦ null { mixesInfixR = p } ] }

mixTerminal ∷ Parser a → Mixfix c a
mixTerminal p = null { mixfixTerminals = p }

mixfix ∷ (SrcCxt → c) → 𝕊 → Mixfix c a → Parser (𝐴 c a)
mixfix f s m = 
  let m' = GenMixfixF
        { genMixfixFTerminals = unParser $ mixfixTerminals m
        , genMixfixFLevels = mapOn (mixfixLevels m) $ \ ms → GenMixesF
            { genMixesFPrefix = unParser $ mixesPrefix ms
            , genMixesFPostfix = unParser $ mixesPostfix ms
            , genMixesFInfix = unParser $ mixesInfix ms
            , genMixesFInfixL = unParser $ mixesInfixL ms
            , genMixesFInfixR = unParser $ mixesInfixR ms
            }
        }
  in
  Parser $ gfmixfix (gpNewContext s) gpNewExpressionContext (map (mapATag f) ∘ gpWithContextRendered) m'
