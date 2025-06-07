module UVMHS.Lib.Parser.Parser where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.RawParser
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex

data Parser t a = Parser
  { parserNext ∷ t ⇰ Parser t a
  , parserFallback ∷ RawParser t a
  }

onParserRaw ∷ (RawParser t a → RawParser t a) → Parser t a → Parser t a
onParserRaw f (Parser n b) = Parser (map (onParserRaw f) n) $ f b

rawToParser ∷ RawParser t a → Parser t a
rawToParser p = Parser dø𝐷 p

rawFrParser ∷ (Ord t) ⇒ Parser t a → RawParser t a
rawFrParser (Parser n b)
  | isEmpty n = b
  | otherwise = tries
      [ do t ← rpPluck
           case n ⋕? parserTokenValue t of
             Some cp → do
               rpRecord t
               rawFrParser cp
             None → rpFail (parserTokenContext t) (parserTokenSuffix t)
      , b
      ]

instance Return (Parser t) where
  return ∷ ∀ a. a → Parser t a
  return x = rawToParser $ return x
instance (Ord t) ⇒ Bind (Parser t) where
  (≫=) ∷ ∀ a b. Parser t a → (a → Parser t b) → Parser t b
  Parser n b ≫= k = Parser (map (extend k) n) $ b ≫= rawFrParser ∘ k
instance (Ord t) ⇒ Functor (Parser t) where map = mmap
instance (Ord t) ⇒ Monad (Parser t)

instance (Ord t) ⇒ MonadFail (Parser t) where
  abort ∷ ∀ a. Parser t a
  abort = rawToParser abort
  (⎅) ∷ ∀ a. Parser t a → Parser t a → Parser t a
  cp₁ ⎅ cp₂ = rawToParser $ rawFrParser cp₁ ⎅ rawFrParser cp₂

instance Null (Parser t a) where
  null ∷ Parser t a
  null = rawToParser abort
instance (Ord t) ⇒ Append (Parser t a) where
  (⧺) ∷ Parser t a → Parser t a → Parser t a
  Parser n₁ b₁ ⧺ Parser n₂ b₂ = Parser (dunionBy (⧺) n₁ n₂) $ b₁ ⎅ b₂
instance (Ord t) ⇒ Monoid (Parser t a)

instance Eps (Parser t ()) where
  eps ∷ Parser t ()
  eps = rawToParser $ return ()
instance (Ord t) ⇒ Seq (Parser t ()) where
  (▷) ∷ Parser t () → Parser t () → Parser t ()
  cp₁ ▷ cp₂ = onParserRaw (\ p → p ≫ rawFrParser cp₂) cp₁
instance (Ord t) ⇒ Seqoid (Parser t ())

pRender ∷ (Ord t) ⇒ Formats → Parser t a → Parser t a
pRender fm = rawToParser ∘ rpRender fm ∘ rawFrParser

pErr ∷ (Ord t) ⇒ 𝕊 → Parser t a → Parser t a
pErr s = rawToParser ∘ rpErr s ∘ rawFrParser

pFinal ∷ (Ord t) ⇒ Parser t a → Parser t a
pFinal = rawToParser ∘ rpFinal ∘ rawFrParser

pTok ∷ (Ord t) ⇒ t → Parser t ()
pTok t = Parser (t ↦ return ()) abort

pTokAny ∷ (Ord t,ToIter t ts) ⇒ ts → Parser t ()
pTokAny ts = concat $ mapOn (iter ts) pTok

pTokRet ∷ (Ord t) ⇒ t → Parser t t
pTokRet t = Parser (t ↦ return t) abort

pTokShaped ∷ (t → 𝑂 a) → Parser t a
pTokShaped = rawToParser ∘ rpShaped

pTokSat ∷ (t → 𝔹) → Parser t t
pTokSat = rawToParser ∘ rpSatisfies

pAny ∷ Parser t t
pAny = rawToParser rpAny

pWord ∷ ∀ s t. (Ord t,s ⇄ 𝐼 t) ⇒ s → Parser t ()
pWord s = do
  void $ rawToParser $ rpWord s

pWordRet ∷ ∀ s t. (Ord t,s ⇄ 𝐼 t) ⇒ s → Parser t s
pWordRet = rawToParser ∘ rpWord

pOptional ∷ (Ord t) ⇒ Parser t a → Parser t (𝑂 a)
pOptional = rawToParser ∘ rpOptional ∘ rawFrParser

pMany ∷ (Ord t) ⇒ Parser t a → Parser t (𝐿 a)
pMany xM = tries
  [ pOneOrMore xM
  , return Nil
  ]

pOneOrMore ∷ (Ord t) ⇒ Parser t a → Parser t (𝐿 a)
pOneOrMore xM = do
  x ← xM
  xs ← pMany xM
  return $ x :& xs

pManySepBy ∷ (Ord t) ⇒ Parser t () → Parser t a → Parser t (𝐿 a)
pManySepBy sepM xM = tries
  [ pOneOrMoreSepBy sepM xM
  , return Nil
  ]

pOneOrMoreSepBy ∷ (Ord t) ⇒ Parser t () → Parser t a → Parser t (𝐿 a)
pOneOrMoreSepBy sepM xM = do
  x ← xM
  xs ← pMany $ do
    sepM 
    xM
  return $ x :& xs

pDie ∷ Parser t a
pDie = rawToParser rpDie

pGuard ∷ 𝔹 → Parser t ()
pGuard = rawToParser ∘ rpGuard

pFailEff ∷ 𝑂 a → Parser t a
pFailEff = rawToParser ∘ rpFailEff

----------------------------
-- Basic Language Parsing --
----------------------------

pTokName_DEP ∷ Parser TokenBasic 𝕊
pTokName_DEP = pTokShaped $ view nameTBasicL

pTokName ∷ Parser TokenWSBasic 𝕊
pTokName = pTokShaped $ view nameTWSBasicL

pTokSyntax_DEP ∷ 𝕊 → Parser TokenBasic ()
pTokSyntax_DEP = pTok ∘ SyntaxTBasic

pTokSyntax ∷ 𝕊 → Parser TokenWSBasic ()
pTokSyntax = pTok ∘ SyntaxTWSBasic

pTokSyntaxAny ∷ (ToIter 𝕊 t) ⇒ t → Parser TokenWSBasic ()
pTokSyntaxAny = pTokAny ∘ map SyntaxTWSBasic ∘ iter

pTokNatN_DEP ∷ Parser TokenBasic ℕ
pTokNatN_DEP = pTokShaped $ view naturalTBasicL

pTokNatN ∷ Parser TokenWSBasic ℕ
pTokNatN = pTokShaped $ view naturalTWSBasicL

pTokNatN64_DEP ∷ Parser TokenBasic ℕ64
pTokNatN64_DEP = failEff ∘ natO64 *$ pTokNatN_DEP

pTokNatN64 ∷ Parser TokenWSBasic ℕ64
pTokNatN64 = failEff ∘ natO64 *$ pTokNatN

pTokInt_DEP ∷ Parser TokenBasic ℤ
pTokInt_DEP = pTokShaped $ view integerTBasicL

pTokInt ∷ Parser TokenWSBasic ℤ
pTokInt = pTokShaped $ view integerTWSBasicL

pTokInt64_DEP ∷ Parser TokenBasic ℤ64
pTokInt64_DEP = failEff ∘ intO64 *$ pTokInt_DEP

pTokInt64 ∷ Parser TokenWSBasic ℤ64
pTokInt64 = failEff ∘ intO64 *$ pTokInt

pTokNat_DEP ∷ Parser TokenBasic ℕ
pTokNat_DEP = failEff ∘ natO *$ pTokInt_DEP

pTokNat ∷ Parser TokenWSBasic ℕ
pTokNat = failEff ∘ natO *$ pTokInt

pTokNat64_DEP ∷ Parser TokenBasic ℕ64
pTokNat64_DEP = failEff ∘ natO64 *$ pTokInt_DEP

pTokNat64 ∷ Parser TokenWSBasic ℕ64
pTokNat64 = failEff ∘ natO64 *$ pTokInt

pTokDouble_DEP ∷ Parser TokenBasic 𝔻
pTokDouble_DEP = pTokShaped $ view doubleTBasicL

pTokDouble ∷ Parser TokenWSBasic 𝔻
pTokDouble = pTokShaped $ view doubleTWSBasicL

pTokString_DEP ∷ Parser TokenBasic 𝕊
pTokString_DEP = pTokShaped $ view stringTBasicL

pTokString ∷ Parser TokenWSBasic 𝕊
pTokString = pTokShaped $ view stringTWSBasicL

pTokChar_DEP ∷ Parser TokenBasic ℂ
pTokChar_DEP = pTokShaped $ view charTBasicL

pTokChar ∷ Parser TokenWSBasic ℂ
pTokChar = pTokShaped $ view charTWSBasicL

pTokBlock ∷ 𝕊 → Parser TokenWSBasic ()
pTokBlock = pTok ∘ BlockTWSBasic

pTokOpen ∷ Parser TokenWSBasic ()
pTokOpen = pTok OpenTWSBasic

pTokClose ∷ Parser TokenWSBasic ()
pTokClose = pTok CloseTWSBasic

pTokDelim ∷ Parser TokenWSBasic ()
pTokDelim = pTok DelimiterTWSBasic

pNewExpressionContext ∷ (Ord t) ⇒ Parser t a → Parser t a
pNewExpressionContext = rawToParser ∘ rpNewExpressionContext ∘ rawFrParser

pNewErrContext ∷ (Ord t) ⇒ 𝕊 → Parser t a → Parser t a
pNewErrContext msg = rawToParser ∘ rpNewErrContext msg ∘ rawFrParser

pNewContext ∷ (Ord t) ⇒ 𝕊 → Parser t a → Parser t a
pNewContext s = rawToParser ∘ rpNewContext s ∘ rawFrParser

pWithContextRendered ∷ (Ord t) ⇒ Parser t a → Parser t (𝐴 SrcCxt a)
pWithContextRendered = rawToParser ∘ rpWithContextRendered ∘ rawFrParser

pNewWithContextRendered ∷ (Ord t) ⇒ 𝕊 → Parser t a → Parser t (𝐴 SrcCxt a)
pNewWithContextRendered s = pNewContext s ∘ pWithContextRendered

pGetContextRendered ∷ Parser t SrcCxt
pGetContextRendered = rawToParser rpGetContextRendered

pNewGetContextRendered ∷ (Ord t) ⇒ Parser t SrcCxt
pNewGetContextRendered = pNewExpressionContext pGetContextRendered

pManyContext ∷ (Ord t,Comonad f) ⇒ (∀ b. Parser t b → Parser t (f b)) → Parser t a → Parser t (𝐿 (f a))
pManyContext f xM = tries
  [ pOneOrMoreContext f xM
  , return Nil
  ]

pOneOrMoreContext ∷ (Ord t,Comonad f) ⇒ (∀ b. Parser t b → Parser t (f b)) → Parser t a → Parser t (𝐿 (f a))
pOneOrMoreContext f xM = do
  xxs ← f $ do
    x ← xM
    xs ← pManyContext f xM
    return $ x :* xs
  let x :* xs = extract xxs
  return $ siphon xxs x :& xs

pManySepByContext ∷ (Ord t,Comonad f) ⇒ (∀ b. Parser t b → Parser t (f b)) → Parser t () → Parser t a → Parser t (𝐿 (f a))
pManySepByContext f sepM xM = tries
  [ pOneOrMoreSepByContext f sepM xM
  , return Nil
  ]

pOneOrMoreSepByContext ∷ (Ord t,Comonad f) ⇒ (∀ b. Parser t b → Parser t (f b)) → Parser t () → Parser t a → Parser t (𝐿 (f a))
pOneOrMoreSepByContext f sepM xM = do
  xxs ← f $ do
    x ← xM
    xs ← pManyContext f $ map snd $ sepM ⧆ xM
    return $ x :* xs
  let x :* xs = extract xxs
  return $ siphon xxs x :& xs

---------------------
-- Running Parsers --
---------------------

runParser₀ ∷ (ToIter (ParserToken t) ts,Ord t) ⇒ 𝕊 → ts → Parser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runParser₀ so = (∘ rawFrParser) ∘ runRawParser (parserEnv₀ so) ∘ parserState₀ ∘ stream

parse ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ Parser t a → 𝕊 → ts → Doc ∨ a
parse p so ts = case runParser₀ so ts $ pFinal p of
  (pe :* None) → Inl $ displaySourceError so pe
  (_ :* Some (_ :* x)) → Inr x

parseIO ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ Parser t a → 𝕊 → ts → IO a
parseIO p s ts = case parse p s ts of
  Inl d → do pprint d ; abortIO
  Inr a → return a

parseIOMain ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ Parser t a → 𝕊 → ts → IO ()
parseIOMain p s ts = do
  x ← parseIO p s ts
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]

tokenizeAndParse ∷ (Eq u,Show u,Plus u,Eq o,Ord w,Pretty a) ⇒ 𝕊 → Lexer CharClass ℂ o u w → Parser w a → 𝕊 → (Doc ∨ Doc) ∨ a
tokenizeAndParse so lex xM s = do
  case tokenize lex so $ tokens s of
    Inl d → Inl $ Inl d
    Inr ts → case parse xM so $ finalizeTokens ts of
      Inl d → Inl $ Inr d
      Inr x → Inr x

tokenizeAndParseIO ∷ (Eq u,Show u,Plus u,Eq o,Ord w,Pretty a) ⇒ 𝕊 → Lexer CharClass ℂ o u w → Parser w a → 𝕊 → IO a
tokenizeAndParseIO so lex xM s = do
  case tokenizeAndParse so lex xM s of
    Inl (Inl d) → do pprint $ ppErr "LEXING ERROR" ; pprint d ; abortIO
    Inl (Inr d) → do pprint $ ppErr "PARSING ERROR" ; pprint d ; abortIO
    Inr x → return x

tokenizeAndParseIOMain ∷ (Eq u,Show u,Plus u,Eq o,Ord w,Pretty a) ⇒ 𝕊 → Lexer CharClass ℂ o u w → Parser w a → 𝕊 → IO ()
tokenizeAndParseIOMain so lex xM s = do
  x ← tokenizeAndParseIO so lex xM s
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]
