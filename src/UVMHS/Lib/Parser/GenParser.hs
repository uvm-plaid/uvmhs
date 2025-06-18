module UVMHS.Lib.Parser.GenParser where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.RawParser
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput

data GenParser t a = GenParser
  { genParserNext ∷ t ⇰ GenParser t a
  , genParserFallback ∷ RawParser t a
  }

gpMapRawParser ∷ (RawParser t a → RawParser t a) → GenParser t a → GenParser t a
gpMapRawParser f (GenParser n b) = GenParser (map (gpMapRawParser f) n) $ f b

gpRawParser ∷ RawParser t a → GenParser t a
gpRawParser p = GenParser dø𝐷 p

gpToRawParser ∷ (Ord t) ⇒ GenParser t a → RawParser t a
gpToRawParser (GenParser n b)
  | isEmpty n = b
  | otherwise = tries
      [ do t ← rpPluck
           case n ⋕? parserTokenValue t of
             Some cp → do
               rpRecord t
               gpToRawParser cp
             None → rpFail (parserTokenContext t) (parserTokenSuffix t)
      , b
      ]

instance Return (GenParser t) where
  return ∷ ∀ a. a → GenParser t a
  return x = gpRawParser $ return x
instance (Ord t) ⇒ Bind (GenParser t) where
  (≫=) ∷ ∀ a b. GenParser t a → (a → GenParser t b) → GenParser t b
  GenParser n b ≫= k = GenParser (map (extend k) n) $ b ≫= gpToRawParser ∘ k
instance (Ord t) ⇒ Functor (GenParser t) where map = mmap
instance (Ord t) ⇒ Monad (GenParser t)

instance (Ord t) ⇒ MonadFail (GenParser t) where
  abort ∷ ∀ a. GenParser t a
  abort = gpRawParser abort
  (⎅) ∷ ∀ a. GenParser t a → GenParser t a → GenParser t a
  cp₁ ⎅ cp₂ = gpRawParser $ gpToRawParser cp₁ ⎅ gpToRawParser cp₂

instance Null (GenParser t a) where
  null ∷ GenParser t a
  null = gpRawParser abort
instance (Ord t) ⇒ Append (GenParser t a) where
  (⧺) ∷ GenParser t a → GenParser t a → GenParser t a
  GenParser n₁ b₁ ⧺ GenParser n₂ b₂ = GenParser (dunionBy (⧺) n₁ n₂) $ b₁ ⎅ b₂
instance (Ord t) ⇒ Monoid (GenParser t a)

instance Eps (GenParser t ()) where
  eps ∷ GenParser t ()
  eps = gpRawParser $ return ()
instance (Ord t) ⇒ Seq (GenParser t ()) where
  (▷) ∷ GenParser t () → GenParser t () → GenParser t ()
  cp₁ ▷ cp₂ = gpMapRawParser (\ p → p ≫ gpToRawParser cp₂) cp₁
instance (Ord t) ⇒ Seqoid (GenParser t ())

gpRender ∷ (Ord t) ⇒ Formats → GenParser t a → GenParser t a
gpRender fm = gpRawParser ∘ rpRender fm ∘ gpToRawParser

gpErr ∷ (Ord t) ⇒ 𝕊 → GenParser t a → GenParser t a
gpErr s = gpRawParser ∘ rpErr s ∘ gpToRawParser

gpFinal ∷ (Ord t) ⇒ GenParser t a → GenParser t a
gpFinal = gpRawParser ∘ rpFinal ∘ gpToRawParser

gpTok ∷ (Ord t) ⇒ t → GenParser t ()
gpTok t = GenParser (t ↦ return ()) abort

gpTokAny ∷ (Ord t,ToIter t ts) ⇒ ts → GenParser t ()
gpTokAny ts = concat $ mapOn (iter ts) gpTok

gpTokRet ∷ (Ord t) ⇒ t → GenParser t t
gpTokRet t = GenParser (t ↦ return t) abort

gpTokShaped ∷ (t → 𝑂 a) → GenParser t a
gpTokShaped = gpRawParser ∘ rpShaped

gpTokSat ∷ (t → 𝔹) → GenParser t t
gpTokSat = gpRawParser ∘ rpSatisfies

gpAny ∷ GenParser t t
gpAny = gpRawParser rpAny

gpWord ∷ ∀ s t. (Ord t,s ⇄ 𝐼 t) ⇒ s → GenParser t ()
gpWord s = do
  void $ gpRawParser $ rpWord s

gpWordRet ∷ ∀ s t. (Ord t,s ⇄ 𝐼 t) ⇒ s → GenParser t s
gpWordRet = gpRawParser ∘ rpWord

gpDie ∷ GenParser t a
gpDie = gpRawParser rpDie

gpGuard ∷ 𝔹 → GenParser t ()
gpGuard = gpRawParser ∘ rpGuard

gpFailEff ∷ 𝑂 a → GenParser t a
gpFailEff = gpRawParser ∘ rpFailEff

gpNewExpressionContext ∷ (Ord t) ⇒ GenParser t a → GenParser t a
gpNewExpressionContext = gpRawParser ∘ rpNewExpressionContext ∘ gpToRawParser

gpNewErrContext ∷ (Ord t) ⇒ 𝕊 → GenParser t a → GenParser t a
gpNewErrContext msg = gpRawParser ∘ rpNewErrContext msg ∘ gpToRawParser

gpNewContext ∷ (Ord t) ⇒ 𝕊 → GenParser t a → GenParser t a
gpNewContext s = gpRawParser ∘ rpNewContext s ∘ gpToRawParser

gpWithContextRendered ∷ (Ord t) ⇒ GenParser t a → GenParser t (𝐴 SrcCxt a)
gpWithContextRendered = gpRawParser ∘ rpWithContextRendered ∘ gpToRawParser

gpNewWithContextRendered ∷ (Ord t) ⇒ 𝕊 → GenParser t a → GenParser t (𝐴 SrcCxt a)
gpNewWithContextRendered s = gpNewContext s ∘ gpWithContextRendered

gpGetContextRendered ∷ GenParser t SrcCxt
gpGetContextRendered = gpRawParser rpGetContextRendered

gpNewGetContextRendered ∷ (Ord t) ⇒ GenParser t SrcCxt
gpNewGetContextRendered = gpNewExpressionContext gpGetContextRendered

---------------------
-- Running Parsers --
---------------------

runGenParser₀ ∷ (ToIter (ParserToken t) ts,Ord t) ⇒ 𝕊 → ts → GenParser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runGenParser₀ so = (∘ gpToRawParser) ∘ runRawParser (parserEnv₀ so) ∘ parserState₀ ∘ stream

gparse ∷ (ToIter (ParserToken t) ts,Ord t) ⇒ GenParser t a → 𝕊 → ts → Doc ∨ a
gparse p so ts = case runGenParser₀ so ts $ gpFinal p of
  (pe :* None) → Inl $ displaySourceError so pe
  (_ :* Some (_ :* x)) → Inr x

gparseIO ∷ (ToIter (ParserToken t) ts,Ord t) ⇒ GenParser t a → 𝕊 → ts → IO a
gparseIO p s ts = case gparse p s ts of
  Inl err → do 
    pprint $ ppVertical
      [ ppErr "[Parsing Failure]"
      , err
      ]
    abortIO
  Inr a → return a

gparseIOMain ∷ (Pretty a,ToIter (ParserToken t) ts,Ord t) ⇒ GenParser t a → 𝕊 → ts → IO ()
gparseIOMain p s ts = do
  x ← gparseIO p s ts
  pprint $ ppVertical
    [ ppHeader "[Parsing Success]"
    , pretty x
    ]
