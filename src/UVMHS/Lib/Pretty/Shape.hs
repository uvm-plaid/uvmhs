module UVMHS.Lib.Pretty.Shape where

import UVMHS.Core

-----------
-- Shape --
-----------

-- A shape looks like:
--
--     □□□□XXX
--     XX
--     XXX
--     XXXX
--     XX
--     XXXXXX
--     XXXX
--
-- this example shape is abstracted as:
--
-- □□□□XXX
-- YYYYYY
-- YYYYYY
-- YYYYYY
-- YYYYYY
-- YYYYYY
-- ZZZZ
--
-- where:
-- + XXX:    represented by the length of the first line
-- + YYYYYY: represented by the maximum length of any line that isn't
--           the first or last
-- + ZZZZ:   represented by the maximum length of the last line
-- + also:   the total number of newlines in the shape
--           (i.e., one more than number of lines in YYYYYYY)
--
-- A special case is a single-line shape, which is represented as
-- just the length of the line.
--
-- shapes can be combined and form a monoid

data ShapeM = ShapeM
  { shapeMFirstLength  ∷ {-# UNPACK #-} ℕ64
  , shapeMMidMaxLength ∷ {-# UNPACK #-} ℕ64
  , shapeMLastLength   ∷ {-# UNPACK #-} ℕ64
  , shapeMNewlines     ∷ {-# UNPACK #-} ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''ShapeM

data Shape =
    SingleLine {-# UNPACK #-} ℕ64
  | MultiLine {-# UNPACK #-} ShapeM
  deriving (Eq,Ord,Show)
makePrisms ''Shape

shapeFirstLength ∷ Shape → ℕ64
shapeFirstLength = \case
  SingleLine n → n
  MultiLine sh → shapeMFirstLength sh

shapeLastLength ∷ Shape → ℕ64
shapeLastLength = \case
  SingleLine n → n
  MultiLine sh → shapeMLastLength sh

newlineShapeM ∷ ShapeM
newlineShapeM = ShapeM zero zero zero one

newlineShape ∷ Shape
newlineShape = MultiLine newlineShapeM

boxShape ∷ ℕ64 → ℕ64 → Shape
boxShape n nls
  | nls ≡ zero = SingleLine n
  | otherwise  = MultiLine $ ShapeM n n n nls

shapeWidth ∷ Shape → ℕ64
shapeWidth = \case
  SingleLine n → n
  MultiLine (ShapeM fl mml ll _) → fl ⊔ mml ⊔ ll

shapeNewlines ∷ Shape → ℕ64
shapeNewlines = \case
  SingleLine _ → zero
  MultiLine sh → shapeMNewlines sh

instance Null Shape where null = SingleLine zero
instance Append Shape where
  SingleLine l₁ ⧺ SingleLine l₂ =
    -- AAA ⧺ XXX = AAAXXX
    SingleLine $ l₁ ⧺ l₂
  SingleLine l₁ ⧺ MultiLine (ShapeM fl₂ mml₂ ll₂ nls₂) =
    -- AAA  ⧺  □□□□XXX  =  □□□□AAAXXX
    --         YY          YY
    --         ZZZZ        ZZZZ
    MultiLine $ ShapeM (l₁ + fl₂) mml₂ ll₂ nls₂
  MultiLine (ShapeM fl₁ mml₁ ll₁ nls₁) ⧺ SingleLine l₂ =
    -- □□□□XXX  ⧺  AAA  =  □□□□XXX
    -- YY                  YY
    -- ZZZZ                ZZZZAAA
    MultiLine $ ShapeM fl₁ mml₁ (ll₁ + l₂) nls₁
  MultiLine (ShapeM fl₁ mml₁ ll₁ nls₁) ⧺ MultiLine (ShapeM fl₂ mml₂ ll₂ nls₂) =
    -- □□□□XXX  ⧺  □□□□AAA  =  □□□□XXX
    -- YY          BB          YY
    -- ZZZZ        CCCC        ZZZZAAA
    --                         BB
    --                         CCCC
    MultiLine $ ShapeM fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ mml₂) ll₂ $ nls₁ + nls₂
instance Monoid Shape

instance Bot Shape where bot = SingleLine zero
instance Join Shape where
  SingleLine l₁ ⊔ SingleLine l₂ = SingleLine $ l₁ ⊔ l₂
  SingleLine l₁ ⊔ MultiLine (ShapeM fl₂ mml₂ ll₂ nls₂) =
    MultiLine $ ShapeM (l₁ ⊔ fl₂) mml₂ ll₂ nls₂
  MultiLine (ShapeM fl₁ mml₁ ll₁ nls₁) ⊔ SingleLine l₂ =
    MultiLine $ ShapeM (l₂ ⊔ fl₁) mml₁ ll₁ nls₁
  MultiLine (ShapeM fl₁ mml₁ ll₁ nls₁) ⊔ MultiLine (ShapeM fl₂ mml₂ ll₂ nls₂)
    | nls₁ > nls₂ = MultiLine $ ShapeM (fl₁ ⊔ fl₂) (mml₁ ⊔ mml₂ ⊔ ll₂) ll₁ nls₁
    | nls₁ < nls₂ = MultiLine $ ShapeM (fl₁ ⊔ fl₂) (mml₁ ⊔ mml₂ ⊔ ll₁) ll₂ nls₂
    | otherwise   = MultiLine $ ShapeM (fl₁ ⊔ fl₂) (mml₁ ⊔ mml₂) (ll₁ ⊔ ll₂) $ nls₁ ⊔ nls₂

------------
-- ShapeA --
------------

-- A non-aligned shape looks like:
--
--     XXX
--     YY
--     ZZZZ
--
--     □XXX
--     YY
--     ZZZZ
--
--     □□XXX
--     YY
--     ZZZZ
--
--     □□□XXX
--     YY
--     ZZZZ
--
-- An aligned shape looks like:
--
--     XXX
--     YY
--     ZZZZ
--
--     □XXX
--     ⋅YY
--     ⋅ZZZZ
--
--     □□XXX
--     ⋅⋅YY
--     ⋅⋅ZZZZ
--
--     □□□XXX
--     ⋅⋅⋅YY
--     ⋅⋅⋅ZZZZ
--
--  Combinig shapes:
--
--      □□□XXX ⧺ □□□AAA ≡ □□□XXX
--      YY       BB       YY
--      ZZZZ     CCCC     ZZZZAAA
--                        BB
--                        CCC
--
--      □□□XXX ⧺ □□□AAA ≡ □□□XXX
--      YY       ⋅⋅⋅BB    YY
--      ZZZZ     ⋅⋅⋅CCCC  ZZZZAAA
--                        ⋅⋅⋅⋅BB
--                        ⋅⋅⋅⋅CCC
--
--      □□□XXX ⧺ □□□AAA ≡ □□□XXX
--      ⋅⋅⋅YY    BB       ⋅⋅⋅YY
--      ⋅⋅⋅ZZZZ  CCCC     ⋅⋅⋅ZZZZAAA
--                        BB
--                        CCC
--
--      □□□XXX ⧺ □□□AAA ≡ □□□XXX
--      ⋅⋅⋅YY    ⋅⋅⋅BB    ⋅⋅⋅YY
--      ⋅⋅⋅ZZZZ  ⋅⋅⋅CCCC  ⋅⋅⋅ZZZZAAA
--                        ⋅⋅⋅␣␣␣␣BB
--                        ⋅⋅⋅␣␣␣␣CCC
--
-- Notice that for each shape:
-- - some lines can be aligned, while others are not
-- - when combining two aligned shapes, you need to indent the RHS in addition
--   to marking lines as aligned
-- - the very first line doesn't need to track if it's aligned or not

data ShapeMA = ShapeMA
  { shapeMAFirstLength   ∷ {-# UNPACK #-} ℕ64
  , shapeMAMidMaxLengthU ∷ {-# UNPACK #-} ℕ64
  , shapeMAMidMaxLengthA ∷ {-# UNPACK #-} ℕ64
  , shapeMALastAlign     ∷ {-# UNPACK #-} 𝔹
  , shapeMALastLength    ∷ {-# UNPACK #-} ℕ64
  , shapeMANewlines      ∷ {-# UNPACK #-} ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''ShapeMA

data ShapeA =
    SingleLineA {-# UNPACK #-} ℕ64
  | MultiLineA {-# UNPACK #-} ShapeMA
  deriving (Eq,Ord,Show)
makePrisms ''ShapeA

instance Null ShapeA where null = SingleLineA zero
instance Append ShapeA where
  s₁ ⧺ s₂ = case (s₁,s₂) of
    (SingleLineA l₁,SingleLineA l₂) → SingleLineA $ l₁ + l₂
    (MultiLineA (ShapeMA fl mmlu mmla la ll nls),SingleLineA l) → MultiLineA $ ShapeMA fl mmlu mmla la (ll + l) nls
    (SingleLineA l,MultiLineA (ShapeMA fl mmlu mmla la ll nls)) → MultiLineA $ ShapeMA (fl + l) mmlu mmla la ll nls
    (MultiLineA (ShapeMA fl₁ mmlu₁ mmla₁ la₁ ll₁ nls₁),MultiLineA (ShapeMA fl₂ mmlu₂ mmla₂ la₂ ll₂ nls₂)) →
      let fl' = fl₁
          mmlu' = joins [mmlu₁,mmlu₂,if not la₁ then joins [mmla₂,ll₁ + fl₂] else 0]
          mmla' = joins [mmla₁,if la₁ then joins [mmla₂,ll₁ + fl₂] else 0]
          la' = la₂
          ll' = ll₂
          nls' = nls₁ + nls₂
      in
      MultiLineA $ ShapeMA fl' mmlu' mmla' la' ll' nls'
instance Monoid ShapeA

alignShapeA ∷ ShapeA → ShapeA
alignShapeA = \case
  SingleLineA l → SingleLineA l
  MultiLineA (ShapeMA fl mmlu mmla _la ll nls) →
    let mmlu' = 0
        mmla' = mmlu ⊔ mmla
        la' = True
    in
    MultiLineA $ ShapeMA fl mmlu' mmla' la' ll nls


shapeMToShapeMA ∷ ShapeM → ShapeMA
shapeMToShapeMA (ShapeM fl mml ll nls) = ShapeMA fl mml 0 False ll nls

shapeToShapeA ∷ Shape → ShapeA
shapeToShapeA = \case
  SingleLine l → SingleLineA l
  MultiLine s → MultiLineA $ shapeMToShapeMA s

shapeALastLength ∷ ShapeA → ℕ64
shapeALastLength = \case
  SingleLineA n → n
  MultiLineA sh → shapeMALastLength sh

shapeALastAlign ∷ ShapeA → 𝑂 𝔹
shapeALastAlign = \case
  SingleLineA _ → None
  MultiLineA sh → Some $ shapeMALastAlign sh

shapeAWidth ∷ ShapeA → ℕ64
shapeAWidth = \case
  SingleLineA n → n
  MultiLineA (ShapeMA fl mmlu mmla _ ll _) → fl ⊔ mmlu ⊔ mmla ⊔ ll
