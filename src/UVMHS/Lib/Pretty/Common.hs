module UVMHS.Lib.Pretty.Common where

import UVMHS.Core

import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.Sep

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Shape

-----------------
-- Input Chunk --
-----------------

data ChunkI =
  --          length
  --          ⌄⌄⌄
    RawChunkI ℕ64 𝕊
  --              ^
  --              string with no newlines
  | NewlineChunkI 𝔹 ℕ64
  --              ^ ^^^
  --        aligned indent after newline
  deriving (Eq,Ord,Show)

rawChunksI ∷ 𝕊 → ChunkI
rawChunksI s = RawChunkI (𝕟64 $ length𝕊 s) s

splitChunksI ∷ 𝕊 → 𝐼 ChunkI
splitChunksI s =
  materialize
  $ filter (\ c → c ≢ RawChunkI (𝕟64 0) "")
  $ inbetween (NewlineChunkI False zero)
  $ map rawChunksI $ splitOn𝕊 "\n" s

shapeIChunk ∷ ChunkI → Shape
shapeIChunk = \case
  RawChunkI l _ → SingleLine l
  NewlineChunkI _ n → newlineShape ⧺ SingleLine n

extendAlignedNewlinesIChunk ∷ ℕ64 → ChunkI → ChunkI
extendAlignedNewlinesIChunk n = \case
  RawChunkI l s → RawChunkI l s
  NewlineChunkI b l → NewlineChunkI b $ if b then l + n else l

------------------
-- Output Chunk --
------------------

data ChunkO =
  --          length
  --          ⌄⌄⌄
    RawChunkO ℕ64 𝕊
  --              ^
  --              string with no newlines
  | PaddingChunkO ℕ64
  --              ^^^
  --              padding length
  deriving (Eq,Ord,Show)

instance ASized ChunkO where
  asize (RawChunkO n _) = n
  asize (PaddingChunkO n) = n

shapeOChunk ∷ ChunkO → Shape
shapeOChunk = \case
  RawChunkO l _ → SingleLine l
  PaddingChunkO n → SingleLine n

--------------------
-- Document Trees --
--------------------

type TreeI = 𝑇V Annotation (𝐼 ChunkI)

--                              stuff
--                              between
--                              newlines
--                              ⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄
type TreeO = 𝑇V Formats (Sep () (𝐼A ChunkO))
--                           ^^
--                           newline indicator

chunkIO ∷ ChunkO → ChunkI
chunkIO = \case
  RawChunkO n s → RawChunkI n s
  PaddingChunkO n → RawChunkI n $ string $ replicate n ' '

treeIO ∷ TreeO → TreeI
treeIO = map𝑇V formatAnnotation $ concat ∘ iter ∘ mapSep (const $ single @_ @(𝐼 _) $ NewlineChunkI False zero) (map chunkIO ∘ iter)

--------------
-- SummaryI --
--------------

data SummaryI = SummaryI
  { summaryIForceBreak ∷ 𝔹
  , summaryIShape ∷ ShapeA
  , summaryIContents ∷ TreeI
  }
makeLenses ''SummaryI

alignChunks ∷ TreeI → TreeI
alignChunks = mapp $ \case
  RawChunkI l s → RawChunkI l s
  NewlineChunkI _ l → NewlineChunkI True l


alignSummary ∷ SummaryI → SummaryI
alignSummary (SummaryI b sh c) = SummaryI b (alignShapeA sh) $ alignChunks c

instance Null SummaryI where null = SummaryI False null null
instance Append SummaryI where
  SummaryI b₁ sh₁ cs₁ ⧺ SummaryI b₂ sh₂ cs₂ =
    let cs₂' =
          if not $ shapeIAligned sh₂
          then cs₂
          else mappOn cs₂ $ extendAlignedNewlinesIChunk $ shapeLastLength $ shapeIShape sh₁
    in SummaryI (b₁ ⩔ b₂) (sh₁ ⧺ sh₂) $ cs₁ ⧺ cs₂'
instance Monoid SummaryI

summaryChunksI ∷ 𝐼 ChunkI → SummaryI
summaryChunksI chunks =
  let sh = concat $ map shapeIChunk $ iter chunks
  in SummaryI False (ShapeA False sh) $ single chunks

annotateSummaryI ∷ Annotation → SummaryI → SummaryI
annotateSummaryI a (SummaryI b sh cs) = SummaryI b sh $ annote a cs

--------------
-- SummaryO --
--------------

data SummaryO = SummaryO
  { summaryOShape ∷ Shape
  , summaryOContents ∷ TreeO
  }
makeLenses ''SummaryO

instance Null SummaryO where null = SummaryO null null
instance Append SummaryO where SummaryO sh₁ cs₁ ⧺ SummaryO sh₂ cs₂ = SummaryO (sh₁ ⧺ sh₂) $ cs₁ ⧺ cs₂
instance Monoid SummaryO

summaryChunksO ∷ Sep () (𝐼A ChunkO) → SummaryO
summaryChunksO chunks =
  let sh = concat $ mapSep (const newlineShape) (concat ∘ map shapeOChunk ∘ iter) chunks
  in SummaryO sh $ single chunks

annotateSummaryO ∷ Formats → SummaryO → SummaryO
annotateSummaryO fm (SummaryO sh cs) = SummaryO sh $ annote fm cs

---------------
-- Alignment --
---------------

data HAlign = LH | CH | RH
data VAlign = TV | CV | BV

hvalign ∷ HAlign → VAlign → ℕ64 → ℕ64 → SummaryO → SummaryO
hvalign ha va m n (SummaryO sh cs) =
  let w   = shapeWidth sh
      wd  = (w ⊔ m) - w
      wdm = wd ⌿ 𝕟64 2
      h   = shapeNewlines sh
      hd  = (h ⊔ n) - h
      hdm = hd ⌿ 𝕟64 2
        -- mmmmmmmm
        -- wwwwwddd
        --        m
        --
        -- nnnnnnnn
        -- hhhhhddd
        --        m
      f ∷ 𝐼A ChunkO → 𝐼A ChunkO
      f = case ha of
        -- mmmmmmmmm
        -- XX
        -- →
        -- XX␣␣␣␣␣␣␣
        LH → hwrap (const zero) $ \ s → m - s
        -- mmmmmmmmm
        -- XX
        -- →
        -- ␣XX␣␣␣␣␣␣
        CH → hwrap (const wdm) $ \ s → m - s - wdm
        -- mmmmmmmmm
        -- XX
        -- →
        -- ␣␣␣␣␣␣␣XX
        RH → hwrap (\ s → m - s) $ const zero
      g ∷ Sep () (𝐼A ChunkO) → Sep () (𝐼A ChunkO)
      g = case va of
        TV → vwrap (zero @ℕ64) $ n - h
        CV → vwrap hdm $ n - h - hdm
        BV → vwrap (n - h) $ zero @ℕ64
  in SummaryO (boxShape m n) $ map (map f ∘ g) cs
  where
    hwrap fi fj xs =
      let s = asize xs
          i = fi s
          j = fj s
      in concat
        [ if i ≡ zero then null else single $ PaddingChunkO i
        , xs
        , if j ≡ zero then null else single $ PaddingChunkO j
        ]
    vwrap i j xs =
      concat
      [ concat $ replicate i $ sepI ()
      , xs
      , concat $ replicate j $ sepI ()
      ]
