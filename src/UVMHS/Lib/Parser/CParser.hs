module UVMHS.Lib.Parser.CParser where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex

data CParser t a = CParser
  { cParserNext ∷ t ⇰ CParser t a
  , cParserFallback ∷ RawParser t a
  }

onCParser ∷ (RawParser t a → RawParser t a) → CParser t a → CParser t a
onCParser f (CParser n b) = CParser (map (onCParser f) n) $ f b

toCParser ∷ RawParser t a → CParser t a
toCParser p = CParser dø𝐷 p

frCParser ∷ (Ord t) ⇒ CParser t a → RawParser t a
frCParser (CParser n b)
  | isEmpty n = b
  | otherwise = tries
      [ do t ← rpPluck
           case n ⋕? parserTokenValue t of
             Some cp → do
               rpRecord t
               frCParser cp
             None → rpFail (parserTokenContext t) (parserTokenSuffix t)
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
  CParser n₁ b₁ ⧺ CParser n₂ b₂ = CParser (dunionBy (⧺) n₁ n₂) (b₁ ⎅ b₂)
instance (Ord t) ⇒ Monoid (CParser t a)

instance Eps (CParser t ()) where
  eps ∷ CParser t ()
  eps = toCParser $ return ()
instance (Ord t) ⇒ Seq (CParser t ()) where
  (▷) ∷ CParser t () → CParser t () → CParser t ()
  cp₁ ▷ cp₂ = onCParser (\ p → p ≫ frCParser cp₂) cp₁
instance (Ord t) ⇒ Seqoid (CParser t ())

cpRender ∷ (Ord t) ⇒ Formats → CParser t a → CParser t a
cpRender fm = toCParser ∘ rpRender fm ∘ frCParser

cpErr ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpErr s = toCParser ∘ rpErr s ∘ frCParser

cpToken ∷ (Ord t) ⇒ t → CParser t t
cpToken t = CParser (t ↦ return t) abort

cpFinal ∷ (Ord t) ⇒ CParser t a → CParser t a
cpFinal = toCParser ∘ rpFinal ∘ frCParser

cpShaped ∷ (t → 𝑂 a) → CParser t a
cpShaped = toCParser ∘ rpShaped

cpSatisfies ∷ (t → 𝔹) → CParser t t
cpSatisfies = toCParser ∘ rpSatisfies

cpAny ∷ CParser t t
cpAny = toCParser rpAny

cpWord ∷ ∀ s t. (Eq t,s ⇄ 𝐼 t) ⇒ s → CParser t s
cpWord = toCParser ∘ rpWord

cpOptional ∷ (Ord t) ⇒ CParser t a → CParser t (𝑂 a)
cpOptional = toCParser ∘ rpOptional ∘ frCParser

cpMany ∷ (Ord t) ⇒ CParser t a → CParser t (𝐿 a)
cpMany xM = tries
  [ cpOneOrMore xM
  , return Nil
  ]

cpOneOrMore ∷ (Ord t) ⇒ CParser t a → CParser t (𝐿 a)
cpOneOrMore xM = do
  x ← xM
  xs ← cpMany xM
  return $ x :& xs

cpManySepBy ∷ (Ord t) ⇒ CParser t () → CParser t a → CParser t (𝐿 a)
cpManySepBy sepM xM = tries
  [ cpOneOrMoreSepBy sepM xM
  , return Nil
  ]

cpOneOrMoreSepBy ∷ (Ord t) ⇒ CParser t () → CParser t a → CParser t (𝐿 a)
cpOneOrMoreSepBy sepM xM = do
  x ← xM
  xs ← cpMany $ do
    void sepM 
    xM
  return $ x :& xs

cpDie ∷ CParser t a
cpDie = toCParser rpDie

cpGuard ∷ 𝔹 → CParser t ()
cpGuard = toCParser ∘ rpGuard

cpFailEff ∷ 𝑂 a → CParser t a
cpFailEff = toCParser ∘ rpFailEff

----------------------------
-- Basic Language Parsing --
----------------------------

cpName ∷ CParser TokenBasic 𝕊
cpName = cpShaped $ view nameTBasicL

cpNameWS ∷ CParser TokenWSBasic 𝕊
cpNameWS = cpShaped $ view nameTWSBasicL

cpSyntax ∷ 𝕊 → CParser TokenBasic TokenBasic
cpSyntax = cpToken ∘ SyntaxTBasic

cpSyntaxWS ∷ 𝕊 → CParser TokenWSBasic TokenWSBasic
cpSyntaxWS = cpToken ∘ SyntaxTWSBasic

cpNatN ∷ CParser TokenBasic ℕ
cpNatN = cpShaped $ view naturalTBasicL

cpNatNWS ∷ CParser TokenWSBasic ℕ
cpNatNWS = cpShaped $ view naturalTWSBasicL

cpNatN64 ∷ CParser TokenBasic ℕ64
cpNatN64 = failEff ∘ natO64 *$ cpNatN

cpNat64NWS ∷ CParser TokenWSBasic ℕ64
cpNat64NWS = failEff ∘ natO64 *$ cpNatNWS

cpInt ∷ CParser TokenBasic ℤ
cpInt = cpShaped $ view integerTBasicL

cpIntWS ∷ CParser TokenWSBasic ℤ
cpIntWS = cpShaped $ view integerTWSBasicL

cpInt64 ∷ CParser TokenBasic ℤ64
cpInt64 = failEff ∘ intO64 *$ cpInt

cpInt64WS ∷ CParser TokenWSBasic ℤ64
cpInt64WS = failEff ∘ intO64 *$ cpIntWS

cpNat ∷ CParser TokenBasic ℕ
cpNat = failEff ∘ natO *$ cpInt

cpNatWS ∷ CParser TokenWSBasic ℕ
cpNatWS = failEff ∘ natO *$ cpIntWS

cpNat64 ∷ CParser TokenBasic ℕ64
cpNat64 = failEff ∘ natO64 *$ cpInt

cpNat64WS ∷ CParser TokenWSBasic ℕ64
cpNat64WS = failEff ∘ natO64 *$ cpIntWS

cpDouble ∷ CParser TokenBasic 𝔻
cpDouble = cpShaped $ view doubleTBasicL

cpDoubleWS ∷ CParser TokenWSBasic 𝔻
cpDoubleWS = cpShaped $ view doubleTWSBasicL

cpString ∷ CParser TokenBasic 𝕊
cpString = cpShaped $ view stringTBasicL

cpStringWS ∷ CParser TokenWSBasic 𝕊
cpStringWS = cpShaped $ view stringTWSBasicL

cpChar ∷ CParser TokenBasic ℂ
cpChar = cpShaped $ view charTBasicL

cpCharWS ∷ CParser TokenWSBasic ℂ
cpCharWS = cpShaped $ view charTWSBasicL

cpBlockWS ∷ 𝕊 → CParser TokenWSBasic TokenWSBasic
cpBlockWS = cpToken ∘ BlockTWSBasic

cpOpenWS ∷ CParser TokenWSBasic ()
cpOpenWS = void $ cpToken OpenTWSBasic

cpCloseWS ∷ CParser TokenWSBasic ()
cpCloseWS = void $ cpToken CloseTWSBasic

cpDelimWS ∷ CParser TokenWSBasic ()
cpDelimWS = void $ cpToken DelimiterTWSBasic

cpNewExpressionContext ∷ (Ord t) ⇒ CParser t a → CParser t a
cpNewExpressionContext = toCParser ∘ rpNewExpressionContext ∘ frCParser

cpNewErrContext ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpNewErrContext msg = toCParser ∘ rpNewErrContext msg ∘ frCParser

cpNewContext ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t a
cpNewContext s = toCParser ∘ rpNewContext s ∘ frCParser

cpWithContextRendered ∷ (Ord t) ⇒ CParser t a → CParser t (𝐴 SrcCxt a)
cpWithContextRendered = toCParser ∘ rpWithContextRendered ∘ frCParser

cpNewWithContextRendered ∷ (Ord t) ⇒ 𝕊 → CParser t a → CParser t (𝐴 SrcCxt a)
cpNewWithContextRendered s = cpNewContext s ∘ cpWithContextRendered

cpGetContextRendered ∷ CParser t SrcCxt
cpGetContextRendered = toCParser rpGetContextRendered

cpNewGetContextRendered ∷ (Ord t) ⇒ CParser t SrcCxt
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

runParser₀ ∷ (ToIter (ParserToken t) ts,Ord t) ⇒ 𝕊 → ts → CParser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runParser₀ so = (∘ frCParser) ∘ runRawParser (parserEnv₀ so) ∘ parserState₀ ∘ stream

parse ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → Doc ∨ a
parse p so ts = case runParser₀ so ts $ cpFinal p of
  (pe :* None) → Inl $ displaySourceError so pe
  (_ :* Some (_ :* x)) → Inr x

parseIO ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → IO a
parseIO p s ts = case parse p s ts of
  Inl d → do pprint d ; abortIO
  Inr a → return a

parseIOMain ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ CParser t a → 𝕊 → ts → IO ()
parseIOMain p s ts = do
  x ← parseIO p s ts
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]
