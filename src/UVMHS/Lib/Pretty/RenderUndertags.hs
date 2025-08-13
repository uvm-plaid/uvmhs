module UVMHS.Lib.Pretty.RenderUndertags where

import UVMHS.Core

import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.Sep

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Common
import UVMHS.Lib.Pretty.Shape

data RenderUTEnv = RenderUTEnv
  { renderUTEnvUnderFormat ∷ 𝑂 (ℂ ∧ Formats) 
  , renderUTEnvNest ∷ ℕ64
  , renderUTEnvIndent ∷ ShapeA
  }
makeLenses ''RenderUTEnv

renderUTEnv₀ ∷ RenderUTEnv
renderUTEnv₀ = RenderUTEnv
  { renderUTEnvUnderFormat = None 
  , renderUTEnvNest = 0
  , renderUTEnvIndent = null
  }

data RenderUTState = RenderUTState
  { t2StateCol ∷ ℕ64
  --                   column
  --                   ⌄⌄⌄
  , t2StateUnders ∷ 𝐼 (ℕ64 ∧ ℕ64 ∧ ℂ ∧ Formats)
  --                         ^^^
  --                         length
  }
makeLenses ''RenderUTState

t2State₀ ∷ RenderUTState
t2State₀ = RenderUTState
  { t2StateCol = 𝕟64 0
  , t2StateUnders = null
  }

type RenderUTM = RWS RenderUTEnv SummaryO RenderUTState
newtype RenderUT = RenderUT { unRenderUT ∷ RenderUTM () }

onRenderUT ∷ (RenderUTM () → RenderUTM ()) → RenderUT → RenderUT
onRenderUT f (RenderUT xM) = RenderUT $ f xM

onRenderUT2 ∷ (RenderUTM () → RenderUTM () → RenderUTM ()) → RenderUT → RenderUT → RenderUT
onRenderUT2 f (RenderUT xM₁) (RenderUT xM₂) = RenderUT $ f xM₁ xM₂

instance Null   RenderUT where null = RenderUT skip
instance Append RenderUT where (⧺)  = onRenderUT2 (≫)
instance Monoid RenderUT

buildUndertags ∷ ℕ64 → RenderUTM ()
buildUndertags l = do
  uf ← askL renderUTEnvUnderFormatL
  case uf of
    None → skip
    Some (c :* fm) → do
      col ← getL t2StateColL
      modifyL t2StateUndersL $ pospend $ single (col :* l :* c :* fm)

renderNewline ∷ 𝔹 → ℕ64 → RenderUTM ()
renderNewline a n = do
  nst ← askL renderUTEnvNestL
  sh ← askL renderUTEnvIndentL
  let n' = n + nst + if a then shapeALastLength sh else 0
  tell $ summaryChunksO $ sepI () ⧺ SepE (single $ PaddingChunkO n')
  putL t2StateColL n

renderRaw ∷ ℕ64 → 𝕊 → RenderUTM ()
renderRaw l s = do
  tell $ summaryChunksO $ SepE $ single $ RawChunkO l s
  modifyL t2StateColL $ (+) l

renderPadding ∷ ℕ64 → RenderUTM ()
renderPadding n =
  case n ≡ zero of
    True → skip
    False → do
      tell $ summaryChunksO $ SepE $ single $ PaddingChunkO n
      modifyL t2StateColL $ (+) n

renderUndertags ∷ RenderUTM ()
renderUndertags = do
  us ← list ^$ getL t2StateUndersL
  putL t2StateUndersL null
  case us ≡ null of
    True → skip
    False → do
      renderNewline False zero
      eachOn us $ \ (colf :* l :* c :* fm) → do
        col ← getL t2StateColL
        renderPadding $ colf - col
        mapOut (annotateSummaryO fm) $ renderRaw l $ string $ replicate (nat l) c

renderChunkUndertags ∷ ChunkI → RenderUTM ()
renderChunkUndertags = \case
  RawChunkI l s → do buildUndertags l ; renderRaw l s
  NewlineChunkI a n → do renderUndertags ; renderNewline a n

annotateRenderUT ∷ Annotation → RenderUTM () → RenderUTM ()
annotateRenderUT (Annotation fm ut n i) = 
  mapOut (annotateSummaryO fm) 
  ∘ mapEnvL renderUTEnvUnderFormatL (first𝑂 ut)
  ∘ mapEnvL renderUTEnvIndentL (pospend i)
  ∘ mapEnvL renderUTEnvNestL ((+) n)

compileRenderUT ∷ TreeI → RenderUT
compileRenderUT rd = onRenderUT (\ xM → xM ≫ renderUndertags) $ un𝑇V rd fₑ fₐ
  where
    fₑ = RenderUT ∘ eachWith renderChunkUndertags
    fₐ = onRenderUT ∘ annotateRenderUT

execRenderUT ∷ TreeI → SummaryO
execRenderUT = evalRWS renderUTEnv₀ t2State₀ ∘ retOut ∘ unRenderUT ∘ compileRenderUT
