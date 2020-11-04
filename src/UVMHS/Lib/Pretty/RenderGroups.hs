module UVMHS.Lib.Pretty.RenderGroups where

import UVMHS.Core

import UVMHS.Lib.ATree

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Common

data RenderGroupsEnv = RenderGroupsEnv
  -- global env
  { renderGroupsEnvMaxLineWidth ∷ 𝑂 ℕ64
  , renderGroupsEnvMaxRibbonWidth ∷ 𝑂 ℕ64
  -- local env
  , renderGroupsEnvNest ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''RenderGroupsEnv

renderGroupsEnv₀ ∷ RenderGroupsEnv
renderGroupsEnv₀ = RenderGroupsEnv
  { renderGroupsEnvMaxLineWidth = Some $ 𝕟64 120
  , renderGroupsEnvMaxRibbonWidth = Some $ 𝕟64 100
  , renderGroupsEnvNest = 𝕟64 0
  }

data RenderGroupsState = RenderGroupsState
  { renderGroupsStateRib ∷ ℕ64
  , renderGroupsStateRow ∷ ℕ64
  , renderGroupsStateCol ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''RenderGroupsState

renderGroupsState₀ ∷ RenderGroupsState
renderGroupsState₀ = RenderGroupsState
  { renderGroupsStateRib = 𝕟64 0
  , renderGroupsStateRow = 𝕟64 0
  , renderGroupsStateCol = 𝕟64 0
  }

type RenderGroupsM = RWS RenderGroupsEnv ITree RenderGroupsState
data RenderGroups = RenderGroups 
  { renderGroupsSummary ∷ Summary
  , renderGroupsRender ∷ RenderGroupsM ()
  }
makeLenses ''RenderGroups

instance Null RenderGroups where null = RenderGroups null skip
instance Append RenderGroups where RenderGroups s₁ r₁ ⧺ RenderGroups s₂ r₂ = RenderGroups (s₁ ⧺ s₂) (r₁ ≫ r₂)
instance Monoid RenderGroups

annotateRenderGroups ∷ Annotation → RenderGroups → RenderGroups
annotateRenderGroups a (RenderGroups (Summary sh rs) xM) = RenderGroups (Summary sh (annotate𝑉𝐴 a rs)) (mapOut (annotate𝑉𝐴 a) xM)

renderIChunks𝕊 ∷ Shape → 𝐼 IChunk → ITree
renderIChunks𝕊 sh chunks
  | sh ≡ null = null
  | otherwise = element𝑉𝐴 chunks

renderITree ∷ Shape → ITree → RenderGroupsM ()
renderITree sh rdis = do
  nest ← askL renderGroupsEnvNestL
  tell $ mapOn rdis $ map $ \case
    NewlineIChunk n → NewlineIChunk $ n + nest
    c → c
  case sh of
    SingleLineShape l → do
      modifyL renderGroupsStateRibL $ (+) l
      modifyL renderGroupsStateColL $ (+) l
    MultiLineShape (MShape _ _ _ ll lines) → do
      modifyL renderGroupsStateRowL $ (+) lines
      putL renderGroupsStateRibL ll
      putL renderGroupsStateColL ll

stringCChunk ∷ 𝕊 → RenderGroups
stringCChunk s =
  let chunks = splitIChunks𝕊 s
      sh = concat $ map shapeIChunk chunks
      rd = renderIChunks𝕊 sh chunks
   in RenderGroups (Summary sh rd) $ renderITree sh rd

stringCChunkModal ∷ 𝕊 → 𝕊 → RenderGroups
stringCChunkModal sf sb =
  let chunksf = splitIChunks𝕊 sf
      chunksb = splitIChunks𝕊 sb
      shf = concat $ map shapeIChunk chunksf
      shb = concat $ map shapeIChunk chunksb
      rdf = renderIChunks𝕊 shf chunksf
      rdb = renderIChunks𝕊 shb chunksb
  in RenderGroups (Summary shf rdf) $ renderITree shb rdb

alignRenderGroupsM ∷ RenderGroupsM a → RenderGroupsM a
alignRenderGroupsM xM = do
  col ← getL renderGroupsStateColL
  nest ← askL renderGroupsEnvNestL
  putL renderGroupsStateColL $ 𝕟64 0
  x ← localL renderGroupsEnvNestL (nest + col) xM
  modifyL renderGroupsStateColL $ (+) col
  return x

alignRenderGroups ∷ RenderGroups → RenderGroups
alignRenderGroups (RenderGroups s r) = RenderGroups (alignSummary s) $ alignRenderGroupsM r

nestRenderGroups ∷ ℕ64 → RenderGroups → RenderGroups
nestRenderGroups n (RenderGroups s r) = RenderGroups s $ mapEnvL renderGroupsEnvNestL ((+) n) r

groupRenderGroupsM ∷ Shape → ITree → RenderGroupsM () → RenderGroupsM ()
groupRenderGroupsM sh rdis xM 
  | shape multiLineShapeL sh = xM
  | otherwise = do
      lwO ← askL renderGroupsEnvMaxLineWidthL
      rwO ← askL renderGroupsEnvMaxRibbonWidthL
      nest ← askL renderGroupsEnvNestL
      rib ← getL renderGroupsStateRibL
      col ← getL renderGroupsStateColL
      let ml :* mr = case sh of
            SingleLineShape l → (nest + col + l) :* (rib + l)
            MultiLineShape (MShape _ fl mml ll _) → 
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
        True → renderITree sh rdis
        False → xM

groupRenderGroups ∷ RenderGroups → RenderGroups
groupRenderGroups (RenderGroups s@(Summary sh rdis) xM) = RenderGroups s $ groupRenderGroupsM sh rdis xM

-- modeRenderGroups ∷ Shape → 𝐼 (T2 IChunk) → PrettyMode → RenderGroupsM () → RenderGroupsM ()
-- modeRenderGroups sh rdis = \case
--   NullMode → id
--   AMode → alignRenderGroups
--   GMode → groupRenderGroups sh rdis
--   AGMode → alignRenderGroups ∘ groupRenderGroups sh rdis
-- 
-- compileRenderGroups ∷ RenderGroups → RenderGroupsM ()
-- compileRenderGroups = \case
--   Leaf𝐴 (Summary shf rdisf) () m (shb :* rdisb) → modeRenderGroups shf rdisf m $ renderITree shb rdisb
--   Append𝐴 (Summary shf rdisf) () m ld₁ lds₂ ld₃ → modeRenderGroups shf rdisf m $ do
--     compileRenderGroups ld₁
--     eachWith compileRenderGroups lds₂
--     compileRenderGroups ld₃
-- compileRenderGroups ∷ RenderGroups → RenderGroupsM ()
-- compileRenderGroups (𝑉𝐴 g) = g fₑ fₐ
--   where
--     fₑ ∷ Summary → RenderGroupsM ()
--     fₑ (Summary sh rd) = renderITree sh rd
--     fₐ ∷ Summary → RenderGroupsM () → RenderGroupsM ()
--     fₐ _ xM = xM
-- 
--
execRenderGroupsWith ∷ (RenderGroupsM () → RenderGroupsM ()) → RenderGroups → ITree
execRenderGroupsWith f = evalRWS renderGroupsEnv₀ renderGroupsState₀ ∘ retOut ∘ f ∘ renderGroupsRender

execRenderGroups ∷ RenderGroups → ITree
execRenderGroups = execRenderGroupsWith id


