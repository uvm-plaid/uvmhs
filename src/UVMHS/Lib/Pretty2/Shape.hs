module UVMHS.Lib.Pretty2.Shape where

import UVMHS.Core

import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy

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
-- ‣ li ∈ LineIndentWidth
newtype LineIndentWidth = LineIndentWidth { unLineIndentWidth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Zero,Plus,Additive,Bot,Join,JoinLattice)
makeLenses ''LineIndentWidth

-- NOTATION:
-- ‣ l ~ ⟨liw,lc⟩ ∈ Line
data Line = Line 
  { lineIndentWidth ∷ {-# UNPACK #-} LineIndentWidth
  , lineContent     ∷ {-# UNPACK #-} 𝕊
  } deriving (Eq,Ord,Show)

totalWidthLine ∷ Line → ℕ64
totalWidthLine (Line liw lc) = unLineIndentWidth liw + count lc

appendContentLine ∷ 𝕊 → Line → Line
appendContentLine s (Line liw lc) = Line liw $ lc ⧺ s

addIndentWidthLine ∷ LineIndentWidth → Line → Line
addIndentWidthLine liw' (Line liw lc) = Line (liw' + liw) lc

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
-- ‣ lcw ∈ LineContentWidth
newtype LineContentWidth = LineContentWidth { unLineContentWidth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Zero,Plus,Additive,Bot,Join,JoinLattice)
makeLenses ''LineContentWidth

-- NOTATION:
-- ‣ ls ~ ⟨liw,lcw⟩ ∈ LineShape
data LineShape = LineShape
  { lineShapeIndentWidth  ∷ {-# UNPACK #-} LineIndentWidth
  , lineShapeContentWidth ∷ {-# UNPACK #-} LineContentWidth
  } deriving (Eq,Ord,Show)
makeLenses ''LineShape

totalWidthLineShape ∷ LineShape → ℕ64
totalWidthLineShape (LineShape liw lcw) = unLineIndentWidth liw + unLineContentWidth lcw

addContentWidthLineShape ∷ LineContentWidth → LineShape → LineShape
addContentWidthLineShape = alter lineShapeContentWidthL ∘ (+)

addIndentWidthLineShape ∷ LineIndentWidth → LineShape → LineShape
addIndentWidthLineShape = alter lineShapeIndentWidthL ∘ (+)

instance Abstraction LineShape Line where
  η (Line liw lc) = LineShape liw $ LineContentWidth $ count lc

-- instance AbstractionFuzzy LineShape Line where
--   μ (LineShape liw lcw) = do
--     lc ← fuzzyStringWithLength lcw
--     return $ Line liw lc

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

onLineShapesAbs ∷ (𝑉 LineContentWidth → 𝑉 LineContentWidth) → LineShapesAbs → LineShapesAbs
onLineShapesAbs f = LineShapesAbs ∘ f ∘ unLineShapesAbs

instance Abstraction LineShapesAbs LineShape where
  η (LineShape liw lc) = LineShapesAbs $ intΩ64 (unLineIndentWidth liw) ↦♮ lc

addIndentLineShapesAbs ∷ LineIndentWidth → LineShapesAbs → LineShapesAbs
addIndentLineShapesAbs liw = 
  let liw' = intΩ64 $ unLineIndentWidth liw
  in onLineShapesAbs $ assoc𝑉 ∘ map (mapFst (+ liw')) ∘ iter


-- -- ========= --
-- -- LineAlign --
-- -- ========= --
-- 
-- -- When laying out pretty printed text, some lines can be "aligned".
-- -- "alignment" means layout out a sequence of lines left-justified as
-- -- determined by the starting column position. E.g., laying out this text:
-- --
-- --     XX
-- --     YYYY
-- --     ZZZ
-- --
-- -- as "non-aligned" after this text:
-- --
-- --     AAAA
-- --     BBB
-- --
-- -- looks like this:
-- --
-- --     AAAA
-- --     BBBXX
-- --     YYYY
-- --     ZZZ
-- --
-- -- whereas laying it out as "aligned" looks like this:
-- --
-- --     AAA
-- --     BBBXX
-- --        YYYY
-- --        ZZZ
-- --
-- -- Alignment can be dependent on the surrounding pretty printing context, or it
-- -- can be independent of the context.
-- --
-- -- It is not as straightforward to just record whether or not a block of text
-- -- is "aligned", "not aligned", or "alignment dependent on context". E.g., in
-- -- the above example that resulted in the following layout:
-- --
-- --     AAA
-- --     BBBXX
-- --        YYYY
-- --        ZZZ
-- --
-- -- consider that the "AAA…" text was "not aligned", and the "XX…" text was
-- -- "aligned". Ther resulting text is "not aligned", however the "YYYY" and
-- -- "ZZZ" lines must now always be shifted a fixed amount equal to the length of
-- -- "BBB". This is all easy enough to represent... we can just flip the
-- -- alignment of "YYYY" and "ZZZ" lines to be "not aligned", and increase the
-- -- indent of those lines. Now consider the case where the alignment of "XX…" is
-- -- dependent on the context, i.e., when context says "align", it should align,
-- -- and when the context says "no align", it should not align. The result for
-- -- lines "YYYY" and "ZZZ" is not still "no align" in the result, but whether or
-- -- not to add the additional offset (the length of "BBB") is now dependent on
-- -- whether or not the context said to align or not. So, we add an additional
-- -- bit of information to the alignment info for each line: "additional offset
-- -- that is dependent on whether or not the alignment context indicated to align
-- -- or not"
-- 
-- newtype LineAlignDepIndent = LineAlignDepIndent { unLineAlignDepIndent ∷ ℕ64 }
--   deriving (Eq,Ord,Show)
-- 
-- -- data DepAlign = 
-- --     -- if context says "align": 
-- --     -- - align to column and indent an additional amount
-- --     -- - this line is still considered "aligned"
-- --     Additional_DA {-# UNPACK #-} ℕ64
-- --     -- if context says "align": 
-- --     -- - do not align to column and indent a fixed amount
-- --     -- - this line is not considered "aligned"
-- --   | Fixed_DA      {-# UNPACK #-} ℕ64
-- --   deriving (Eq,Ord,Show)
-- -- 
-- -- makePrisms ''DepAlign
-- 
-- data LineAlign =
--     -- never align, no matter what the align context is
--     No_LA ℕ64
--     -- always align, no matter what the align context is
--   | Yes_LA
--     -- alignment is dependent on the align context
--   | Dep_LA ℕ64
--   deriving (Eq,Ord,Show)
-- 
-- makePrisms ''LineAlign
-- 
-- 
-- -- align: no
-- -- context: *
-- -- □□□□XXX
-- -- AA
-- --
-- -- align: yes
-- -- context: *
-- -- □□□□XXX
-- -- →→→→AA
-- --
-- -- align: *
-- -- context: no
-- -- □□□□XXX
-- -- AA
-- --
-- -- align: *+2
-- -- context: yes
-- -- □□□□XXX
-- -- →→→→⇉⇉AA
-- --
-- -- align: *@2
-- -- context: yes
-- -- □□□□XXX
-- -- ⇨⇨AA
-- 
-- -- =================== --
-- -- CONCRETE STRUCTURES --
-- -- =================== --
-- 
-- -------------------
-- -- LineShape --
-- -------------------
-- 
-- --------------
-- -- DepAlign --
-- --------------
-- 
-- ---------------
-- -- LineAlign --
-- ---------------
-- 
-- ------------------------
-- -- LineAlignAlignment --
-- ------------------------
-- 
-- data LineAlignAlignment =
--     No_LAA
--   | Yes_LAA
--   | Dep_LAA
--   deriving (Eq,Ord,Show)
-- 
-- alignmentLineAlign ∷ LineAlign → LineAlignAlignment
-- alignmentLineAlign = \case
--   No_LA → No_LAA
--   Yes_LA → Yes_LAA
--   Dep_LA da → case da of
--     Additional_DA _ → Dep_LAA
--     Fixed_DA _ → No_LAA
-- 
-- ---------------
-- -- LineShape --
-- ---------------
-- 
-- data LineShape = LineShape
--   { lineShapeBase  ∷ {-# UNPACK #-} LineShape
--   , lineShapeAlign ∷ {-# UNPACK #-} LineAlign
--   } deriving (Eq,Ord,Show)
-- 
-- makeLenses ''LineShape
-- 
-- appendContentLineShape ∷ IndentedLineContentWidth → LineShape → LineShape
-- appendContentLineShape = alter lineShapeBaseL ∘ addContentIndentedLine
-- 
-- whenAlignAdditionalOffsetLineShape ∷ IndentedLineIndentWidth → LineShape → LineShape
-- whenAlignAdditionalOffsetLineShape off (LineShape lsb lsa) = case lsa of
--   No_LA → LineShape lsb lsa
--   Yes_LA → LineShape (addIndentIndentedLine off lsb) Yes_LA
--   Dep_LA da → case da of
--     Additional_DA add → LineShape lsb $ Dep_LA $ Additional_DA $ unIndentedLineIndentWidth off + add
--     Fixed_DA _ → LineShape lsb lsa
-- 
-- whenAlignInstantiateColumnLineShape ∷ IndentedLineIndentWidth → LineShape → LineShape
-- whenAlignInstantiateColumnLineShape col (LineShape lsb lsa) = case lsa of
--   No_LA → LineShape lsb lsa
--   Yes_LA → LineShape (addIndentIndentedLine col lsb) No_LA
--   Dep_LA da → case da of
--     Additional_DA add → LineShape lsb $ Dep_LA $ Fixed_DA $ unIndentedLineIndentWidth col + add
--     Fixed_DA _ → LineShape lsb lsa
-- 
-- -- =================== --
-- -- ABSTRACT STRUCTURES -- 
-- -- =================== --
-- 
-- ----------------------
-- -- LineShapesAbs --
-- ----------------------
-- 
-- --------------------------
-- -- LineShapeDepAlignAbs --
-- --------------------------
-- 
-- data LineShapeDepAlignAbs = LineShapeDepAlignAbs
--   -- map from additional indent to regular indent to content width
--   { lineShapeDepAlignAbsAdditional ∷ {-# UNPACK #-} (𝑉 LineShapesAbs)
--   -- map from fixed indent to regular indent to content width
--   , lineShapeDepAlignAbsFixed      ∷ {-# UNPACK #-} (𝑉 LineShapesAbs)
--   } deriving (Eq,Ord,Show)
-- 
-- makeLenses ''LineShapeDepAlignAbs
-- 
-- instance Bot LineShapeDepAlignAbs where
--   bot = LineShapeDepAlignAbs bot bot
-- instance Join LineShapeDepAlignAbs where
--   LineShapeDepAlignAbs 𝑏₁ 𝑜₁ ⊔ LineShapeDepAlignAbs 𝑏₂ 𝑜₂ = LineShapeDepAlignAbs (𝑏₁ ⊔ 𝑏₂) $ 𝑜₁ ⊔ 𝑜₂
-- instance JoinLattice LineShapeDepAlignAbs
-- 
-- whenAlignAdditionalOffsetLineShapeDepAlignAbs ∷ IndentedLineIndentWidth → LineShapeDepAlignAbs → LineShapeDepAlignAbs
-- whenAlignAdditionalOffsetLineShapeDepAlignAbs off (LineShapeDepAlignAbs adds fxds) =
--   let adds' = assoc𝑉 $ mapOn (iter adds) $ \ (add :* indcnts) → (add + intΩ64 (unIndentedLineIndentWidth off)) :* indcnts
--   in
--   LineShapeDepAlignAbs adds' fxds
-- 
-- whenAlignInstantiateColumnLineShapeDepAlignAbs ∷ IndentedLineIndentWidth → LineShapeDepAlignAbs → LineShapeDepAlignAbs
-- whenAlignInstantiateColumnLineShapeDepAlignAbs col (LineShapeDepAlignAbs adds fxds) =
--   let fxds' = joins
--         [ fxds
--         , assoc𝑉 $ mapOn (iter adds) $ \ (add :* lsba) → (intΩ64 (unIndentedLineIndentWidth col) + add) :* lsba
--         ]
--   in LineShapeDepAlignAbs bot fxds'
-- 
-- ------------------
-- -- LineShapesAbs --
-- ------------------
-- 
-- data LineShapesAbs = LineShapesAbs
--   { lineShapeAbsNoAlign   ∷ {-# UNPACK #-} LineShapesAbs
--   , lineShapeAbsYesAlign  ∷ {-# UNPACK #-} LineShapesAbs
--   , lineShapeAbsDepAlign  ∷ {-# UNPACK #-} LineShapeDepAlignAbs
--   } deriving (Eq,Ord,Show)
-- 
-- makeLenses ''LineShapeAbs
-- 
-- instance Bot LineShapesAbs where
--   bot = LineShapesAbs bot bot bot
-- instance Join LineShapesAbs where
--   LineShapesAbs sNA₁ sYA₁ sDA₁ ⊔ LineShapesAbs sNA₂ sYA₂ sDA₂ = 
--     LineShapesAbs (sNA₁ ⊔ sNA₂) (sYA₁ ⊔ sYA₂) $ sDA₁ ⊔ sDA₂
-- instance JoinLattice LineShapesAbs
-- 
-- instance Abstraction LineShapesAbs LineShape where
--   η (LineShape lsb lsa) =
--     let lsb' = η lsb
--     in case lsa of
--       No_LA → LineShapesAbs lsb' bot bot 
--       Yes_LA → LineShapesAbs bot lsb' bot 
--       Dep_LA 𝑜 → LineShapesAbs bot bot $ case 𝑜 of
--         Additional_DA add → LineShapeDepAlignAbs (intΩ64 add ↦♮ lsb') bot
--         Fixed_DA fxd → LineShapeDepAlignAbs bot $ intΩ64 fxd ↦♮ lsb'
-- 
-- whenAlignAdditionalOffsetLineShapeAbs ∷ IndentedLineIndentWidth → LineShapesAbs → LineShapesAbs
-- whenAlignAdditionalOffsetLineShapeAbs off (LineShapesAbs sNA sYA sDA) =
--   let sYA' = addIndentLineShapesAbs off sYA
--       sDA' = whenAlignAdditionalOffsetLineShapeDepAlignAbs off sDA
--   in
--   LineShapesAbs sNA sYA' sDA'
-- 
-- whenAlignInstantiateColumnLineShapeAbs ∷ IndentedLineIndentWidth → LineShapesAbs → LineShapesAbs
-- whenAlignInstantiateColumnLineShapeAbs col (LineShapesAbs sNA sYA sDA) = 
--   let sNA' = joins
--         [ sNA
--         , addIndentLineShapesAbs col sYA
--         ]
--       sYA' = bot
--       sDA' = whenAlignInstantiateColumnLineShapeDepAlignAbs col sDA
--   in
--   LineShapesAbs sNA' sYA' sDA'
-- 
-- -----------------------
-- -- MultilineShapeAbs --
-- -----------------------
-- 
-- data MultilineShapeAbs = MultilineShapeAbs
--   { multilineShapeAbsNewlines       ∷ {-# UNPACK #-} ℕ64
--   , multilineShapeAbsFirstLine      ∷ {-# UNPACK #-} ℕ64
--   , multilineShapeAbsMaxMiddleLines ∷ {-# UNPACK #-} LineShapesAbs
--   , multilineShapeAbsLastLine       ∷ {-# UNPACK #-} LineShape
--   } deriving (Eq,Ord,Show) 
-- 
-- makeLenses ''MultilineShapeAbs
-- 
-- -- instantiateAtColumnMultilineShapeAbs ∷ ℕ64 → MultilineShapeAbs → MultilineShapeAbs
-- -- instantiateAtColumnMultilineShapeAbs col (MultilineShapeAbs nls fl mml ll) =
-- --   let mml' = whenAlignInstantiateColumnLineShapeAbs col mml
-- --       ll'  = instantiateAtColumnLineShape    col ll
-- --   in MultilineShapeAbs nls fl mml' ll'
-- 
-- --------------
-- -- ShapeAbs --
-- --------------
-- 
-- data ShapeAbs =
--     SingleLine_SA {-# UNPACK #-} ℕ64
--   | Multiline_SA  {-# UNPACK #-} MultilineShapeAbs
--   deriving (Eq,Ord,Show)
-- 
-- --   AA  ⧺   XX  =   AA
-- -- +:BBB   +:YYY   +:BBBXX
-- --                 +:␣␣␣YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- -:BBB   +:YYY   -:BBBXX
-- --                 -:␣␣␣YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- +:BBB   -:YYY   +:BBBXX
-- --                 +:YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- -:BBB   -:YYY   -:BBBXX
-- --                 -:YYY
--
-- --    AA   ⧺    XXX   =    AA
-- -- +1:⇉BB    +2:⇉⇉YYY   +1:⇉BBXXX
-- --                      +0:␣␣␣YYY
--
-- --    AA   ⧺    XX    =    AA
-- -- +1:⇉BBB   +2:⇉⇉YYY   +1:⇉BBBXX
-- --                      +0:␣␣␣␣YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- -:BBB   +:YYY   -:BBBXX
-- --                 -:␣␣␣YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- +:BBB   -:YYY   +:BBBXX
-- --                 +:YYY
-- --
-- --   AA  ⧺   XX  =   AA
-- -- -:BBB   -:YYY   -:BBBXX
-- --                 -:YYY
-- --
-- 
-- instance Null ShapeAbs where null = SingleLine_SA zero
-- instance Append ShapeAbs where
--   𝑠₁ ⧺ 𝑠₂ = case (𝑠₁,𝑠₂) of
--     (SingleLine_SA ℓ₁,SingleLine_SA ℓ₂) → 
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAAXXX
--       SingleLine_SA $ ℓ₁ + ℓ₂
--     (SingleLine_SA ℓ₁,Multiline_SA (MultilineShapeAbs n₂ ℓF₂ ℓMMs₂ ℓL₂)) → 
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAAXXX
--       --           YY        YY
--       --           ZZZZ      ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAAXXX
--       --           →→→→⇉YY   →→→→␣␣␣⇉YY
--       --           →→→→⇉ZZZZ →→→→␣␣␣⇉ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAAXXX
--       --           ⇨⇨YY      ⇨⇨YY
--       --           ⇨⇨ZZZZ    ⇨⇨ZZZZ
--       let n    = n₂
--           ℓF   = ℓ₁ + ℓF₂
--           ℓMMs = whenAlignAdditionalOffsetLineShapeAbs (IndentedLineIndentWidth ℓ₁) ℓMMs₂
--           ℓL   = ℓL₂
--       in
--       Multiline_SA $ MultilineShapeAbs n ℓF ℓMMs ℓL
--     (Multiline_SA (MultilineShapeAbs n₁ ℓF₁ ℓMMs₁ ℓL₁),SingleLine_SA ℓ₂) → 
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- BB                  BB
--       -- CCCC                CCCCXXX
--       let n    = n₁
--           ℓF   = ℓF₁
--           ℓMMs = ℓMMs₁
--           ℓL   = appendContentLineShape (IndentedLineContentWidth ℓ₂) ℓL₁
--       in
--       Multiline_SA $ MultilineShapeAbs n ℓF ℓMMs ℓL
--     (Multiline_SA (MultilineShapeAbs n₁ ℓF₁ ℓMMs₁ ℓL₁),Multiline_SA (MultilineShapeAbs n₂ ℓF₂ ℓMMs₂ ℓL₂)) →
--       -- rule (1): 
--       --   if LHS is no align and RHS is yes align, then
--       --   unalign and shift RHS right by the length of LHS
--       -- rule (2): 
--       --   if LHS is yes align and RHS is yes align, then
--       --   shift RHS right by the length of LHS
--       -- rule (3): if rule (1) and (2) don't apply, leave alone
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- BB        YY        BB
--       -- CCCC      ZZZZ      CCCCXXX
--       --                     YY
--       --                     ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- BB        ⇨⇨YY      BB
--       -- CCCC      ⇨⇨ZZZZ    CCCCXXX
--       --                     ⇨⇨YY
--       --                     ⇨⇨ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- BB        →→→→⇉YY   BB
--       -- CCCC      →→→→⇉ZZZZ CCCCXXX
--       --                     ␣␣␣␣⇉YY
--       --                     ␣␣␣␣⇉ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- ⇨⇨BB      YY        ⇨⇨BB
--       -- ⇨⇨CCCC    ZZZZ      ⇨⇨CCCCXXX
--       --                     YY
--       --                     ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- ⇨⇨BB      ⇨⇨YY      ⇨⇨⇨⇨BB
--       -- ⇨⇨CCCC    ⇨⇨ZZZZ    ⇨⇨⇨⇨CCCCXXX
--       --                     YY
--       --                     ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- ⇨⇨BB      →→→→⇉YY   ⇨⇨BB
--       -- ⇨⇨CCCC    →→→→⇉ZZZZ ⇨⇨CCCCXXX
--       --                     ␣␣␣␣␣␣⇉YY
--       --                     ␣␣␣␣␣␣⇉ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- →→→→⇉BB   YY        →→→→⇉BB
--       -- →→→→⇉CCCC ZZZZ      →→→→⇉CCCCXXX
--       --                     YY
--       --                     ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- →→→→⇉BB   ⇨⇨YY      →→→→⇉BB
--       -- →→→→⇉CCCC ⇨⇨ZZZZ    →→→→⇉CCCCXXX
--       --                     ⇨⇨YY
--       --                     ⇨⇨ZZZZ
--       -- □□□□AAA ⧺ □□□□XXX = □□□□AAA
--       -- →→→→⇉BB   →→→→⇉YY   →→→→⇉BB
--       -- →→→→⇉CCCC →→→→⇉ZZZZ →→→→⇉CCCCXXX
--       --                     →→→→␣␣␣␣␣⇉YY
--       --                     →→→→␣␣␣␣␣⇉ZZZZ
--       let LineShape ℓL₁_lsb ℓL₁_lsa = ℓL₁
--           n      = n₁ + n₂
--           ℓF     = ℓF₁
--           ℓMM'   = η $ appendContentLineShape (IndentedLineContentWidth ℓF₂) ℓL₁
--           ℓMMs₂' = appto ℓMMs₂ $ case ℓL₁_lsa of
--             No_LA → whenAlignInstantiateColumnLineShapeAbs $ IndentedLineIndentWidth $ totalWidthIndentedLine ℓL₁_lsb
--             Yes_LA → whenAlignAdditionalOffsetLineShapeAbs $ IndentedLineIndentWidth $ totalWidthIndentedLine ℓL₁_lsb
--             Dep_LA da → case da of
--               Additional_DA add → whenAlignAdditionalOffsetLineShapeAbs $ IndentedLineIndentWidth $ add + totalWidthIndentedLine ℓL₁_lsb
--               Fixed_DA fxd → whenAlignInstantiateColumnLineShapeAbs $ IndentedLineIndentWidth $ fxd + totalWidthIndentedLine ℓL₁_lsb
--           ℓMMs   = ℓMMs₁ ⊔ ℓMM' ⊔ ℓMMs₂'
--           ℓL     = appto ℓL₂ $ case ℓL₁_lsa of
--             No_LA → whenAlignInstantiateColumnLineShape $ IndentedLineIndentWidth $ totalWidthIndentedLine ℓL₁_lsb
--             Yes_LA → whenAlignAdditionalOffsetLineShape $ IndentedLineIndentWidth $ totalWidthIndentedLine ℓL₁_lsb
--             Dep_LA da → case da of
--               Additional_DA add → whenAlignAdditionalOffsetLineShape $ IndentedLineIndentWidth $ add + totalWidthIndentedLine ℓL₁_lsb
--               Fixed_DA fxd → whenAlignInstantiateColumnLineShape $ IndentedLineIndentWidth $ fxd + totalWidthIndentedLine ℓL₁_lsb
--       in
--       Multiline_SA $ MultilineShapeAbs n ℓF ℓMMs ℓL
-- instance Monoid ShapeAbs
-- 
-- alignShape ∷ ShapeAbs → ShapeAbs
-- alignShape = \case
--   SingleLine_SA len → SingleLine_SA len
--   Multiline_SA (MultilineShapeAbs n ℓF ℓMMs ℓL) →
--     let LineShapesAbs sNA sYA (LineShapeDepAlignAbs adds fxds) = ℓMMs
--         LineShape ℓ α = ℓL
--         sNA' = joins
--           [ sNA
--           , joins $ mapOn (iter fxds) $ \ (fxd :* lsba) → addIndentLineShapesAbs (IndentedLineIndentWidth $ natΩ64 fxd) lsba
--           ]
--         sYA' = joins
--           [ sYA
--           , joins $ mapOn (iter adds) $ \ (add :* lsba) → addIndentLineShapesAbs (IndentedLineIndentWidth $ natΩ64 add) lsba
--           ]
--         ℓMMs' = LineShapesAbs sNA' sYA' bot
--         (ℓ',α') = case α of
--           No_LA → (ℓ,No_LA)
--           Yes_LA → (ℓ,Yes_LA)
--           Dep_LA da → case da of
--             Additional_DA add → (addIndentIndentedLine (IndentedLineIndentWidth add) ℓ,Yes_LA)
--             Fixed_DA fxd → (addIndentIndentedLine (IndentedLineIndentWidth fxd) ℓ,No_LA)
--         ℓL' = LineShape ℓ' α'
--     in Multiline_SA $ MultilineShapeAbs n ℓF ℓMMs' ℓL'
-- 
-- indentShape ∷ ℕ64 → ShapeAbs → ShapeAbs
-- indentShape ι = \case
--   SingleLine_SA ℓ → SingleLine_SA ℓ
--   Multiline_SA (MultilineShapeAbs n ℓF ℓMMs ℓL) →
--     let LineShapesAbs sNA sYA (LineShapeDepAlignAbs adds fxds) = ℓMMs
--         LineShape ℓ α = ℓL
--         sNA' = addIndentLineShapesAbs (IndentedLineIndentWidth ι) sNA
--         sYA' = addIndentLineShapesAbs (IndentedLineIndentWidth ι) sYA
--         adds' = assoc𝑉 $ mapOn (iter adds) $ \ (add :* lsba) → (add + intΩ64 ι) :* lsba
--         fxds' = assoc𝑉 $ mapOn (iter fxds) $ \ (fxd :* lsba) → (fxd + intΩ64 ι) :* lsba
--         ℓMMs' = LineShapesAbs sNA' sYA' $ LineShapeDepAlignAbs adds' fxds'
--         α' = case α of
--           No_LA → No_LA
--           Yes_LA → Yes_LA
--           Dep_LA da → Dep_LA $ case da of
--             Additional_DA add → Additional_DA $ add + ι
--             Fixed_DA fxd → Fixed_DA $ fxd + ι
--         ℓL' = LineShape ℓ α'
--     in Multiline_SA $ MultilineShapeAbs n ℓF ℓMMs' ℓL'
