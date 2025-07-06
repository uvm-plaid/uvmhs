module UVMHS.Lib.Pretty2.Shape2 where

import UVMHS.Core

import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy

-- =========== --
-- ABSTRACTION --
-- =========== --

class Abstraction a c | c → a where
  η ∷ c → a
class Concretization a c | c → a where
  μ ∷ a → FuzzyM c

-- ==== --
-- Line --
-- ==== --

-- A line is a chunk of text, not to include newline characters, preceeded by
-- some number of whitespace (' ') characters.
--
-- E.g., this text
--
--     ␣␣abc
--
-- is the chunk of text "abc" preceeded by two whitespace characters, written
-- notationally as "␣␣", but displayed literally as "  ".
--
-- A line is represented as a pair of the indent width (a natural number) and
-- the content (a string), so the representation of the above example is
-- ⟨2,"abc"⟩

-- NOTATION:
-- ‣ l ~ ⟨iw,c⟩ ∈ Line
data Line = Line 
  { lineIndentWidth ∷ {-# UNPACK #-} LineIndentWidth
  , lineContent     ∷ {-# UNPACK #-} 𝕊
  } deriving (Eq,Ord,Show)

-- NOTATION:
-- ‣ iw ∈ LineIndentWidth
newtype LineIndentWidth = LineIndentWidth { unLineIndentWidth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Zero,Plus,Additive,Bot,Join,JoinLattice)

makeLenses ''Line
makeLenses ''LineIndentWidth

totalWidthLine ∷ Line → ℕ64
totalWidthLine (Line liw lc) = unLineIndentWidth liw + length64𝕊 lc

appendContentLine ∷ 𝕊 → Line → Line
appendContentLine s (Line iw c) = Line iw $ c ⧺ s

addIndentWidthLine ∷ LineIndentWidth → Line → Line
addIndentWidthLine iw' (Line iw c) = Line (iw' + iw) c

-- ========== --
-- LINE SHAPE --
-- ========== --

-- Shapes are abstractions of lines. A single shape abstracts a single line.
-- For text that looks like this:
--
--     ␣␣abc
--
-- the shape then looks like this:
--
--     ␣␣•••
--
--
-- A shape is then respresnted as the indent width (a natural number) and the
-- content width (a natural number), so the representation of the above example
-- is just ⟨2,3⟩

-- NOTATION:
-- ‣ ls ~ ⟨liw,lcw⟩ ∈ LineShape
data LineShape = LineShape
  { lineShapeIndentWidth  ∷ {-# UNPACK #-} LineIndentWidth
  , lineShapeContentWidth ∷ {-# UNPACK #-} LineContentWidth
  } deriving (Eq,Ord,Show)

-- NOTATION: 
-- ‣ lcw ∈ LineContentWidth
newtype LineContentWidth = LineContentWidth { unLineContentWidth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Zero,Plus,Additive,Bot,Join,JoinLattice)

makeLenses ''LineShape
makeLenses ''LineContentWidth

instance Abstraction LineShape Line where
  η (Line liw lc) = LineShape liw $ LineContentWidth $ length64𝕊 lc

totalWidthLineShape ∷ LineShape → ℕ64
totalWidthLineShape (LineShape liw lcw) = unLineIndentWidth liw + unLineContentWidth lcw

addContentWidthLineShape ∷ LineContentWidth → LineShape → LineShape
addContentWidthLineShape = alter lineShapeContentWidthL ∘ (+)

addIndentWidthLineShape ∷ LineIndentWidth → LineShape → LineShape
addIndentWidthLineShape = alter lineShapeIndentWidthL ∘ (+)

-- =============== --
-- LINE SHAPES ABS --
-- =============== --

-- An abstract line shape represents a set of concrete line shapes, and by
-- proxy a set of concrete lines. To represent a set of concrete line shapes,
-- the abstract representation consists of a map from indentation widths to the
-- maximum content length associated with that indentation width in the set.
--
-- E.g., these two lines:
-- 
--     ␣␣•••
--     ␣␣••••
--     ␣␣␣••
--
-- get abstracted as:
--
--     {2↦4,3↦2}
--
-- It would be strictly less useful to represent this as the inverse mapping,
-- i.e., map from content lengths to maximum indend widths. The reason for this
-- is that the precise value of indent widths are used for other abstract
-- operations, whereas only the upper bound value of content widths is ever
-- needed to perform precise abstract operations.

-- NOTATION:
-- ‣ icws ~ {liw↦lcw} ∈ LineShapesAbs
newtype LineShapesAbs = LineShapesAbs { unLineShapesAbs ∷ 𝑉 LineContentWidth }
  deriving (Eq,Ord,Show,Bot,Join,JoinLattice)
makeLenses ''LineShapesAbs

instance Abstraction LineShapesAbs LineShape where
  η (LineShape liw lc) = LineShapesAbs $ intΩ64 (unLineIndentWidth liw) ↦♮ lc

onLineShapesAbs ∷ (𝑉 LineContentWidth → 𝑉 LineContentWidth) → LineShapesAbs → LineShapesAbs
onLineShapesAbs f = LineShapesAbs ∘ f ∘ unLineShapesAbs

addIndentLineShapesAbs ∷ LineIndentWidth → LineShapesAbs → LineShapesAbs
addIndentLineShapesAbs liw = 
  let liw' = intΩ64 $ unLineIndentWidth liw
  in onLineShapesAbs $ assoc𝑉 ∘ map (mapFst (+ liw')) ∘ iter

-- =============================================== --
-- =============================================== --
-- =============================================== --

data MultilineShapeAbs = MultilineShapeAbs
  { multilineShapeAbsNewlines       ∷ {-# UNPACK #-} ℕ64
  , multilineShapeAbsFirstLine      ∷ {-# UNPACK #-} ℕ64
  , multilineShapeAbsMaxMiddleLines ∷ {-# UNPACK #-} LineShapesAbs
  , multilineShapeAbsLastLine       ∷ {-# UNPACK #-} LineShape
  } deriving (Eq,Ord,Show) 

data ShapeAbs =
    SingleLine_SA {-# UNPACK #-} ℕ64
  | Multiline_SA  {-# UNPACK #-} MultilineShapeAbs
  deriving (Eq,Ord,Show)

--   AA    ⧺    XX    =    AAXX     =   □AAXX
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--              YYY        YYY      =   YYY
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--             +YYY       +␣␣YYY    =   →␣␣YYY
--
--   AA    ⧺    XX    =    AA       =   □AA
--   BBB                   BBBXX    =   BBBXX
--
--   AA    ⧺    XX    =    AA       =   □AA
--  +BBB                  +BBBXX    =   →BBXX
--
--   AA    ⧺    XX    =    AA       =   □AA
--   BBB        YYY        BBBXX    =   BBBXX
--                         YYY      =   YYY
--
--   AA    ⧺    XX    =    AA       =   □AA
--   BBB       +YYY        BBBXX    =   BBBXX
--                         ␣␣␣YYY   =   ␣␣␣YYY
--
--   AA    ⧺    XX    =    AA       =    □AA
--  +BBB        YYY       +BBBXX    =    →BBBXX
--                         YYY      =    YYY
--
--   AA    ⧺    XX    =    AA       =    □AA
--  +BBB       +YYY       +BBBXX    =    →BBBXX
--                        +␣␣␣YYY   =    →␣␣␣YYY
--
-- ***********************
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--            4-YYY      4-YYY      =   ▷▷▷▷YYY
--
---------------------------------------------
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--            1+YYY      0+␣␣YYY    =   →␣␣YYY
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--            3+YYY      1+␣␣YYY    =   →␣␣YYY
--
--   AA    ⧺    XX    =    AAXX     =   □AAXX
--            4+YYY      2+␣␣YYY    =   →▷␣␣YYY
--
---------------------------------------------
--
--   AA    ⧺    XX    =    AA       =   □AA
-- 4-BBB                 4-BBBXX    =   ▷▷▷▷BBBXX
--
---------------------------------------------
--
--   AA    ⧺    XX    =    AA       =   □AA
-- 1+BBB                 1+BBBXX    =   →BBXX
--
--   AA    ⧺    XX    =    AA       =   □AA
-- 3+BBB                 3+BBBXX    =   →▷▷BBXX
--
--   AA    ⧺    XX    =    AA       =   □□□□□AA
--   BBB        YYY        BBBXX    =   BBBXX
--                         YYY      =   YYY
--
--   AA    ⧺    XX    =    AA       =   □□□□□AA
--   BBB       +YYY        BBBXX    =   BBBXX
--                         ␣␣␣YYY   =      YYY
--
--   AA    ⧺    XX    =    AA       =    □□□□□AA
--  +BBB        YYY       +BBBXX    =         BBBXX
--                         YYY      =    YYY
--
--   AA    ⧺    XX    =    AA       =    □□□□□AA
--  +BBB       +YYY       +BBBXX    =         BBBXX
--                        +␣␣␣YYY   =            YYY
