module UVMHS.Lib.Pretty.DocA where

import UVMHS.Core

import UVMHS.Lib.TreeAnnote

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Common
import UVMHS.Lib.Pretty.Shape

-- DocA renders group and algin commands

data DocAEnv = DocAEnv
  -- global env
  { docAEnvMaxLineWidth ∷ 𝑂 ℕ64
  , docAEnvMaxRibbonWidth ∷ 𝑂 ℕ64
  -- local env
  , docAEnvNest ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''DocAEnv

docAEnv₀ ∷ DocAEnv
docAEnv₀ = DocAEnv
  { docAEnvMaxLineWidth = Some $ 𝕟64 120
  , docAEnvMaxRibbonWidth = Some $ 𝕟64 100
  , docAEnvNest = 𝕟64 0
  }

data DocAState = DocAState
  { docAStateRib ∷ ℕ64
  , docAStateRow ∷ ℕ64
  , docAStateCol ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''DocAState

docAState₀ ∷ DocAState
docAState₀ = DocAState
  { docAStateRib = 𝕟64 0
  , docAStateRow = 𝕟64 0
  , docAStateCol = 𝕟64 0
  }

type DocAM = RWS DocAEnv TreeI DocAState
data DocA = 
    StaticDocA SummaryI
  | DynamicDocA SummaryI (DocAM ())
makePrisms ''DocA

staticDocA ∷ DocA → SummaryI
staticDocA = \case
  StaticDocA s → s
  DynamicDocA s _ → s

dynamicDocA ∷ DocA → DocAM ()
dynamicDocA = \case
  StaticDocA s → renderSummaryI s
  DynamicDocA _ d → d

instance Null DocA where null = StaticDocA null
instance Append DocA where 
  StaticDocA s₁ ⧺ StaticDocA s₂ = StaticDocA $ s₁ ⧺ s₂
  StaticDocA s₁ ⧺ DynamicDocA s₂ r₂ = DynamicDocA (s₁ ⧺ s₂) $ renderSummaryI s₁ ≫ r₂
  DynamicDocA s₁ r₁ ⧺ StaticDocA s₂ = DynamicDocA (s₁ ⧺ s₂) $ r₁ ≫ renderSummaryI s₂
  DynamicDocA s₁ r₁ ⧺ DynamicDocA s₂ r₂ = DynamicDocA (s₁ ⧺ s₂) $ r₁ ≫ r₂
instance Monoid DocA

renderSummaryI ∷ SummaryI → DocAM ()
renderSummaryI s = 
  let f =
        if shapeIAligned $ summaryIShape s
        then alignDocAM
        else id
  in f $ do
    nest ← askL docAEnvNestL
    let o = mapOn (summaryIContents s) $ map $ \case
          NewlineChunkI n → NewlineChunkI $ n + nest
          c → c
    tell o
    case shapeIShape $ summaryIShape s of
      SingleLine l → do
        modifyL docAStateRibL $ (+) l
        modifyL docAStateColL $ (+) l
      MultiLine (ShapeM _ _ ll lines) → do
        modifyL docAStateRowL $ (+) lines
        putL docAStateRibL ll
        putL docAStateColL ll

stringDocA ∷ 𝕊 → DocA
stringDocA = StaticDocA ∘ summaryChunksI ∘ splitChunksI

docAModal ∷ DocA → DocA → DocA
docAModal d₁ d₂ = DynamicDocA (staticDocA d₁) $ dynamicDocA d₂

annotateDocA ∷ Annotation → DocA → DocA
annotateDocA a = \case
  StaticDocA s → StaticDocA $ annotateSummaryI a s
  DynamicDocA s xM → DynamicDocA (annotateSummaryI a s) $ mapOut (annote a) xM

groupDocAM ∷ SummaryI → DocAM () → DocAM ()
groupDocAM s xM = do
  if summaryIForceBreak s 
  then xM
  else do
    lwO ← askL docAEnvMaxLineWidthL
    rwO ← askL docAEnvMaxRibbonWidthL
    nest ← askL docAEnvNestL
    rib ← getL docAStateRibL
    col ← getL docAStateColL
    let ml :* mr = case shapeIShape $ summaryIShape s of
          SingleLine l → (nest + col + l) :* (rib + l)
          MultiLine (ShapeM fl mml ll _) → 
            joins [ nest + col + fl , nest + mml , nest + ll ]
            :*
            joins [ rib + fl , mml , ll ]
        mlb = case lwO of
          None → True
          Some lw → ml ≤ lw
        mrb = case rwO of
          None → True
          Some rw → mr ≤ rw
    case mlb ⩓ mrb of 
      True → renderSummaryI s
      False → xM

groupDocA ∷ DocA → DocA
groupDocA = \case
  StaticDocA s → StaticDocA s
  DynamicDocA s xM → DynamicDocA s $ groupDocAM s xM

alignDocAM ∷ DocAM a → DocAM a
alignDocAM xM = do
  col ← getL docAStateColL
  nest ← askL docAEnvNestL
  putL docAStateColL $ 𝕟64 0
  x ← localL docAEnvNestL (nest + col) xM
  modifyL docAStateColL $ (+) col
  return x

alignDocA ∷ DocA → DocA
alignDocA = \case
  StaticDocA s → StaticDocA $ alignSummary s
  DynamicDocA s r → DynamicDocA (alignSummary s) $ alignDocAM r

execDocAWith ∷ (DocAM () → DocAM ()) → DocA → TreeI
execDocAWith f d = evalRWS docAEnv₀ docAState₀ $ retOut $ f $ case d of
  StaticDocA s → tell $ summaryIContents s
  DynamicDocA _ r → r

execDocA ∷ DocA → TreeI
execDocA = execDocAWith id
