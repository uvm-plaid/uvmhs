module UVMHS.Lib.Pretty.Common where

import UVMHS.Core

import UVMHS.Lib.ATree

import UVMHS.Lib.Pretty.Annotation

-----------------
-- Input Chunk --
-----------------
  
data IChunk =
  --          length
  --          ⌄⌄⌄
    RawIChunk ℕ64 𝕊
  --              ^
  --              string with no newlines
  | NewlineIChunk ℕ64
  --              ^^^
  --              indent after newline
  deriving (Eq,Ord,Show)

rawIChunk𝕊 ∷ 𝕊 → IChunk
rawIChunk𝕊 s = RawIChunk (𝕟64 $ length𝕊 s) s
 
splitIChunks𝕊 ∷ 𝕊 → 𝐼 IChunk
splitIChunks𝕊 s = 
  materialize $ filter (\ c → c ≢ RawIChunk (𝕟64 0) "") $ inbetween (NewlineIChunk zero) $ map rawIChunk𝕊 $ splitOn𝕊 "\n" s

extendNewlinesIChunk ∷ ℕ64 → IChunk → IChunk
extendNewlinesIChunk n = \case
  RawIChunk l s → RawIChunk l s
  NewlineIChunk l → NewlineIChunk $ n + l

------------------
-- Output Chunk --
------------------

data OChunk =
  --          length
  --          ⌄⌄⌄
    RawOChunk ℕ64 𝕊
  --              ^
  --              string with no newlines
  | NewlineOChunk
  | PaddingOChunk ℕ64
  --              ^^^
  --              padding length
  deriving (Eq,Ord,Show)

--------------------
-- Document Trees --
--------------------

type ITree = 𝑉𝐴 Annotation (𝐼 IChunk)
type OTree = 𝑉𝐴 Formats (𝐼 OChunk)

-----------
-- Shape --
-----------

-- An aligned shape looks like:
--
--     □□□□XXX
--         XXXXX
--         XXXX
--
-- and a non-aligned shape looks like:
--
--     □□□□XXX
--     XXXXX
--     XXXX
--
-- shapes are abstracted as:
--
-- XXX
-- YY
-- YY
-- YY
-- ZZZZ
-- 
-- where:
-- + XXX:  represented by the length of the first line
-- + YY:   represented by the maximum length of any line that isn't
--   the first or last
-- + ZZZZ: represented by the maximum length of the last line
-- + also: the total number of lines (i.e., how many lines of YY)
--
-- A special case is a single-line shape, which is represented as
-- just the length of the line.
--
-- shapes can be combined:
--
-- aligned + aligned = aligned
-- aligned + non-aligned = non-aligned
-- non-aligned + aligned = 

data MShape = MShape
  { multiShapeAligned ∷ 𝔹
  , multiShapeFirstLength ∷ ℕ64
  , multiShapeMidMaxLength ∷ ℕ64
  , multiShapeLastLength ∷ ℕ64
  , multiShapeLines ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''MShape

data Shape = SingleLineShape ℕ64 | MultiLineShape MShape
  deriving (Eq,Ord,Show)
makePrisms ''Shape

instance Null Shape where 
  null = SingleLineShape $ 𝕟64 0
instance Append Shape where
  SingleLineShape l₁ ⧺ SingleLineShape l₂ = 
    -- AAA ⧺ XXX = AAAXXX
    SingleLineShape $ l₁ ⧺ l₂
  SingleLineShape l₁ ⧺ MultiLineShape (MShape a₂ fl₂ mml₂ ll₂ lines₂)
    -- AAA  ⧺  □□□□XXX  =  □□□□AAAXXX
    --         YY          YY
    --         ZZZZ        ZZZZ
    | not a₂ = MultiLineShape $ MShape False (l₁ + fl₂) mml₂ ll₂ lines₂
    -- AAA  ⧺  □□□□XXX  =  □□□□AAAXXX
    --             YY          ␣␣␣YY
    --             ZZZZ        ␣␣␣ZZZZ
    | otherwise = MultiLineShape $ MShape True (l₁ + fl₂) (l₁ + mml₂) (l₁ + ll₂) lines₂
  MultiLineShape (MShape a₁ fl₁ mml₁ ll₁ lines₁) ⧺ SingleLineShape l₂ = 
    -- □□□□XXX  ⧺  AAA  =  □□□□XXX
    -- YY                  YY
    -- ZZZZ                ZZZZAAA
    -- □□□□XXX  ⧺  AAA  =  □□□□XXX
    --     YY                  YY
    --     ZZZZ                ZZZZAAA
    MultiLineShape $ MShape a₁ fl₁ mml₁ (ll₁ + l₂) lines₁
  MultiLineShape (MShape a₁ fl₁ mml₁ ll₁ lines₁) ⧺ MultiLineShape (MShape a₂ fl₂ mml₂ ll₂ lines₂)
    -- □□□□XXX  ⧺  □□□□AAA  =  □□□□XXX
    -- YY          BB          YY
    -- ZZZZ        CCCC        ZZZZAAA
    --                         BB
    --                         CCCC
    -- □□□□XXX   ⧺  □□□□AAA  =  □□□□XXX
    --     YY       BB              YY
    --     ZZZZ     CCCC            ZZZZAAA
    --                              BB
    --                              CCCC
    | not a₂ = MultiLineShape $ MShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ mml₂) ll₂ (lines₁ + lines₂)
    -- □□□□XXX  ⧺  □□□□AAA   =  □□□□XXX
    -- YY              BB       YY
    -- ZZZZ            CCCC     ZZZZAAA
    --                          ␣␣␣␣BB
    --                          ␣␣␣␣CCCC
    -- □□□□XXX   ⧺  □□□□AAA   =  □□□□XXX
    --     YY           BB           YY
    --     ZZZZ         CCCC         ZZZZAAA
    --                               ␣␣␣␣BB
    --                               ␣␣␣␣CCCC
    | otherwise = MultiLineShape $ MShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ (ll₁ + mml₂)) (ll₁ + ll₂) (lines₁ + lines₂)
instance Monoid Shape

alignShape ∷ Shape → Shape
alignShape (SingleLineShape l) = SingleLineShape l
alignShape (MultiLineShape ms) = MultiLineShape ms { multiShapeAligned = True }

-- getShapeAligned ∷ Shape → 𝔹
-- getShapeAligned (SingleLineShape _) = False
-- getShapeAligned (MultiLineShape ms) = multiShapeAligned ms

shapeIChunk ∷ IChunk → Shape
shapeIChunk (RawIChunk l _) = SingleLineShape l
shapeIChunk (NewlineIChunk n) = MultiLineShape $ MShape False (𝕟64 0) (𝕟64 0) n (𝕟64 1)
 
-------------
-- Summary --
-------------

data Summary = Summary
  { summaryShape ∷ Shape
  , summaryContents ∷ ITree
  }
makeLenses ''Summary

instance Null Summary where null = Summary null null
instance Append Summary where
  Summary sh₁ cs₁ ⧺ Summary sh₂ cs₂ = 
    let sh = sh₁ ⧺ sh₂
    in case (sh₁,sh₂) of
    (SingleLineShape _,SingleLineShape _) → Summary sh $ cs₁ ⧺ cs₂
    (SingleLineShape l₁,MultiLineShape ms₂)
      | not $ multiShapeAligned ms₂ → Summary sh $ cs₁ ⧺ cs₂
      | otherwise →
          let cs₂' = mappOn cs₂ $ extendNewlinesIChunk l₁
          in Summary sh $ cs₁ ⧺ cs₂'
    (MultiLineShape _,SingleLineShape _) → Summary sh $ cs₁ ⧺ cs₂
    (MultiLineShape ms₁,MultiLineShape ms₂)
      | not $ multiShapeAligned ms₂ → Summary sh $ cs₁ ⧺ cs₂
      | otherwise → 
          let cs₂' = mappOn cs₂ $ extendNewlinesIChunk $ multiShapeLastLength ms₁
          in Summary sh $ cs₁ ⧺ cs₂'
instance Monoid Summary

alignSummary ∷ Summary → Summary
alignSummary (Summary sh cs) = Summary (alignShape sh) cs


