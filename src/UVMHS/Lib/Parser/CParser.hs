module UVMHS.Lib.Parser.CParser where

import UVMHS.Core

import UVMHS.Lib.Variables
import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex

data CParser t a = CParser
  { cParserNext ∷ t ⇰ CParser t a
  , cParserFallback ∷ Parser t a
  }

onCParser ∷ (Parser t a → Parser t a) → CParser t a → CParser t a
onCParser f (CParser n b) = CParser (map (onCParser f) n) $ f b

toCParser ∷ Parser t a → CParser t a
toCParser p = CParser dø p

frCParser ∷ (Ord t) ⇒ CParser t a → Parser t a
frCParser (CParser n b) 
  | isEmpty n = b
  | otherwise = tries
      [ do t ← pPluck
           case n ⋕? parserTokenValue t of
             Some cp → do
               pRecord t
               frCParser cp
             None → pFail (parserTokenContext t) (parserTokenSuffix t)
      , b
      ]

instance Return (CParser t) where 
  return ∷ ∀ a. a → CParser t a
  return x = toCParser $ return x
instance (Ord t) ⇒ Bind (CParser t) where
  (≫=) ∷ ∀ a b. CParser t a → (a → CParser t b) → CParser t b
  CParser n b ≫= k = 
    CParser (map (extend k) n) 
            (b ≫= frCParser ∘ k) 
instance (Ord t) ⇒ Functor (CParser t) where map = mmap
instance (Ord t) ⇒ Monad (CParser t)

instance (Ord t) ⇒ MonadFail (CParser t) where
  abort ∷ ∀ a. CParser t a
  abort = toCParser abort
  (⎅) ∷ ∀ a. CParser t a → CParser t a → CParser t a
  cp₁ ⎅ cp₂ = toCParser $ frCParser cp₁ ⎅ frCParser cp₂

instance Null (CParser t a) where
  null ∷ CParser t a
  null = toCParser abort
instance (Ord t) ⇒ Append (CParser t a) where
  (⧺) ∷ CParser t a → CParser t a → CParser t a
  CParser n₁ b₁ ⧺ CParser n₂ b₂ = CParser (unionWith (⧺) n₁ n₂) (b₁ ⎅ b₂)
instance (Ord t) ⇒ Monoid (CParser t a)

instance Eps (CParser t ()) where
  eps ∷ CParser t ()
  eps = toCParser $ return ()
instance (Ord t) ⇒ Seq (CParser t ()) where
  (▷) ∷ CParser t () → CParser t () → CParser t ()
  cp₁ ▷ cp₂ = onCParser (\ p → p ≫ frCParser cp₂) cp₁
instance (Ord t) ⇒ Seqoid (CParser t ())

cpRender ∷ (Ord t) ⇒ Formats → CParser t a → CParser t a
cpRender fm = toCParser ∘ pRender fm ∘ frCParser

cpErr ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpErr s = toCParser ∘ pErr s ∘ frCParser

cpToken ∷ (Ord t) ⇒ t → CParser t t
cpToken t = CParser (t ↦ return t) abort

cpFinal ∷ (Ord t) ⇒ CParser t a → CParser t a
cpFinal = toCParser ∘ pFinal ∘ frCParser

cpShaped ∷ (t → 𝑂 a) → CParser t a
cpShaped = toCParser ∘ pShaped

cpSatisfies ∷ (t → 𝔹) → CParser t t
cpSatisfies = toCParser ∘ pSatisfies 

cpAny ∷ CParser t t
cpAny = toCParser pAny

cpWord ∷ ∀ s t. (Eq t,s ⇄ 𝐼 t) ⇒ s → CParser t s
cpWord = toCParser ∘ pWord

cpOptional ∷ (Ord t) ⇒ CParser t a → CParser t (𝑂 a)
cpOptional = toCParser ∘ pOptional ∘ frCParser

cpMany ∷ (Ord t) ⇒ CParser t a → CParser t (𝐿 a)
cpMany xM = tries
  [ cpOneOrMore xM
  , return Nil
  ]

cpOneOrMore ∷ (Ord t) ⇒ CParser t a → CParser t (𝐿 a)
cpOneOrMore xM = do
  x ← xM
  xs ← cpMany xM
  return $ x:&xs

cpManySepBy ∷ (Ord t) ⇒ CParser t () → CParser t a → CParser t (𝐿 a)
cpManySepBy sepM xM = tries
  [ cpOneOrMoreSepBy sepM xM
  , return Nil
  ]

cpOneOrMoreSepBy ∷ (Ord t) ⇒ CParser t () → CParser t a → CParser t (𝐿 a)
cpOneOrMoreSepBy sepM xM = do
  x ← xM
  xs ← map snd ^$ cpMany $ sepM ⧆ xM
  return $ x :& xs

----------------------------
-- Basic Language Parsing --
----------------------------

cpSyntax ∷ 𝕊 → CParser TokenBasic TokenBasic
cpSyntax = cpToken ∘ SyntaxTBasic

cpName ∷ CParser TokenBasic 𝕏
cpName = var ^$ cpShaped $ view nameTBasicL

cpNatural ∷ CParser TokenBasic ℕ
cpNatural = cpShaped $ view naturalTBasicL

cpInteger ∷ CParser TokenBasic ℤ
cpInteger = cpShaped $ view integerTBasicL

cpDouble ∷ CParser TokenBasic 𝔻
cpDouble = cpShaped $ view doubleTBasicL

cpString ∷ CParser TokenBasic 𝕊
cpString = cpShaped $ view stringTBasicL

cpSyntaxWS ∷ 𝕊 → CParser TokenWSBasic TokenWSBasic
cpSyntaxWS = cpToken ∘ SyntaxTWSBasic

cpNameWS ∷ CParser TokenWSBasic 𝕏
cpNameWS = var ^$ cpShaped $ view nameTWSBasicL

cpNaturalWS ∷ CParser TokenWSBasic ℕ
cpNaturalWS = cpShaped $ view naturalTWSBasicL

cpIntegerWS ∷ CParser TokenWSBasic ℤ
cpIntegerWS = cpShaped $ view integerTWSBasicL

cpDoubleWS ∷ CParser TokenWSBasic 𝔻
cpDoubleWS = cpShaped $ view doubleTWSBasicL

cpStringWS ∷ CParser TokenWSBasic 𝕊
cpStringWS = cpShaped $ view stringTWSBasicL

cpBlockWS ∷ 𝕊 → CParser TokenWSBasic TokenWSBasic
cpBlockWS = cpToken ∘ BlockTWSBasic

cpOpenWS ∷ CParser TokenWSBasic ()
cpOpenWS = void $ cpToken OpenTWSBasic

cpCloseWS ∷ CParser TokenWSBasic ()
cpCloseWS = void $ cpToken CloseTWSBasic

cpDelimWS ∷ CParser TokenWSBasic ()
cpDelimWS = void $ cpToken DelimiterTWSBasic

cpNewExpressionContext ∷ (Ord t) ⇒ CParser t a → CParser t a
cpNewExpressionContext = toCParser ∘ pNewExpressionContext ∘ frCParser

cpNewErrContext ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpNewErrContext msg = toCParser ∘ pNewErrContext msg ∘ frCParser

cpNewContext ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpNewContext s = toCParser ∘ pNewContext s ∘ frCParser

cpWithContextRendered ∷ (Ord t) ⇒ CParser t a → CParser t (Annotated FullContext a)
cpWithContextRendered = toCParser ∘ pWithContextRendered ∘ frCParser

cpNewWithContextRendered ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t (Annotated FullContext a)
cpNewWithContextRendered s = cpNewContext s ∘ cpWithContextRendered

cpGetContextRendered ∷ CParser t FullContext
cpGetContextRendered = toCParser pGetContextRendered

cpNewGetContextRendered ∷ (Ord t) ⇒ CParser t FullContext
cpNewGetContextRendered = cpNewExpressionContext cpGetContextRendered

cpManyContext ∷ (Ord t,Comonad f) ⇒ (∀ b. CParser t b → CParser t (f b)) → CParser t a → CParser t (𝐿 (f a))
cpManyContext f xM = tries
  [ cpOneOrMoreContext f xM
  , return Nil
  ]

cpOneOrMoreContext ∷ (Ord t,Comonad f) ⇒ (∀ b. CParser t b → CParser t (f b)) → CParser t a → CParser t (𝐿 (f a))
cpOneOrMoreContext f xM = do
  xxs ← f $ do
    x ← xM
    xs ← cpManyContext f xM
    return $ x :* xs
  let x :* xs = extract xxs
  return $ siphon xxs x :& xs

cpManySepByContext ∷ (Ord t,Comonad f) ⇒ (∀ b. CParser t b → CParser t (f b)) → CParser t () → CParser t a → CParser t (𝐿 (f a))
cpManySepByContext f sepM xM = tries
  [ cpOneOrMoreSepByContext f sepM xM
  , return Nil
  ]

cpOneOrMoreSepByContext ∷ (Ord t,Comonad f) ⇒ (∀ b. CParser t b → CParser t (f b)) → CParser t () → CParser t a → CParser t (𝐿 (f a))
cpOneOrMoreSepByContext f sepM xM = do
  xxs ← f $ do
    x ← xM
    xs ← cpManyContext f $ map snd $ sepM ⧆ xM
    return $ x :* xs
  let x :* xs = extract xxs
  return $ siphon xxs x :& xs

---------------------
-- Running Parsers --
---------------------
             
runParser₀ ∷ (ToStream (ParserToken t) ts,Ord t) ⇒ ts → CParser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runParser₀ = (∘ frCParser) ∘ runParser parserEnv₀ ∘ parserState₀ ∘ stream

parse ∷ (Pretty a,ToStream (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → Doc ∨ a
parse p so ts = case runParser₀ ts $ cpFinal p of
  (pe :* None) → Inl $ displaySourceError so pe
  (_ :* Some (_ :* x)) → Inr x

parseIO ∷ (Pretty a,ToStream (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → IO a
parseIO p s ts = case parse p s ts of
  Inl d → pprint d ≫ abortIO
  Inr a → return a

parseIOMain ∷ (Pretty a,ToStream (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → IO ()
parseIOMain p s ts = do
  x ← parseIO p s ts
  pprint $ ppVertical 
    [ ppHeader "Success"
    , pretty x
    ]
