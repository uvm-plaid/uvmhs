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
  MultiLine (ShapeM fl _ _ _) → fl

shapeLastLength ∷ Shape → ℕ64
shapeLastLength = \case
  SingleLine n → n
  MultiLine (ShapeM _ _ _ ll) → ll

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
  MultiLine (ShapeM _ _ _ nls) → nls 

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
    | otherwise       = MultiLine $ ShapeM (fl₁ ⊔ fl₂) (mml₁ ⊔ mml₂) (ll₁ ⊔ ll₂) $ nls₁ ⊔ nls₂

------------
-- ShapeA --
------------

-- A non-aligned shape looks like:
--
--     □□□□XXX
--     YY
--     ZZZZ
--
-- An aligned shape looks like:
--
--     □□□□XXX
--     ⋅⋅⋅⋅YY
--     ⋅⋅⋅⋅ZZZZ
--
-- Shapes can be combined, which may affect alignment:
--
--     non-aligned + non-aligned = non-aligned
--     non-aligned + aligned     = non-aligned
--     aligned     + non-aligned = aligned
--     aligned     + aligned     = aligned

data ShapeA = ShapeA
  { shapeIAligned ∷ {-# UNPACK #-} 𝔹
  , shapeIShape   ∷ {-# UNPACK #-} Shape
  }
  deriving (Eq,Ord,Show)
makeLenses ''ShapeA

instance Null ShapeA where null = ShapeA False null
instance Append ShapeA where
  ShapeA a₁ sh₁ ⧺ ShapeA a₂ sh₂ =
    let sh₂' =
          if shape singleLineL sh₂ ⩔ not a₂
          -- ‣ sh₁ is single-line
          -- ‣ sh₂ is single-line 
          --
          --     □□□□XXX  ⧺  □□□□AAA  =  □□□□XXXAAA
          --
          -- ‣ sh₁ is single-line 
          -- ‣ sh₂ is multiline non-aligned
          --
          --     □□□□XXX  ⧺  □□□□AAA  =  □□□□XXXAAA
          --                 BB          BB
          --                 CCCC        CCCC
          --
          -- ‣ sh₁ is multiline non-aligned
          -- ‣ sh₂ is single-line
          -- 
          --     □□□□XXX  ⧺ □□□□AAA  =  □□□□XXX
          --     YY                     YY     
          --     ZZZZ                   ZZZZAAA
          -- 
          -- ‣ sh₁ is multiline non-aligned 
          -- ‣ sh₂ is multiline non-aligned
          --     □□□□XXX  ⧺  □□□□AAA  =  □□□□XXX
          --     YY          BB          YY
          --     ZZZZ        CCCC        ZZZZAAA
          --                             BB
          --                             CCCC
          --
          -- ‣ sh₁ is multiline aligned
          -- ‣ sh₂ is single-line
          --
          --     □□□□XXX   ⧺  □□□□AAA  =  □□□□XXX 
          --     ⋅⋅⋅⋅YY                   ⋅⋅⋅⋅YY  
          --     ⋅⋅⋅⋅ZZZZ                 ⋅⋅⋅⋅ZZZZAAA
          --
          -- ‣ sh₁ is multiline aligned
          -- ‣ sh₂ is multiline non-aligned 
          --
          --     □□□□XXX   ⧺  □□□□AAA  =  □□□□XXX
          --     ⋅⋅⋅⋅YY       BB          ⋅⋅⋅⋅YY
          --     ⋅⋅⋅⋅ZZZZ     CCCC        ⋅⋅⋅⋅ZZZZAAA
          --                              ⋅⋅⋅⋅BB
          --                              ⋅⋅⋅⋅CCCC
          --
          then sh₂
          -- ‣ sh₁ is single-lined
          -- ‣ sh₂ is multiline aligned
          --
          --     □□□□XXX  ⧺   □□□□AAA   =  □□□□XXXAAA 
          --                  ⋅⋅⋅⋅BB       ⋅⋅⋅⋅␣␣␣BB  
          --                  ⋅⋅⋅⋅CCCC     ⋅⋅⋅⋅␣␣␣CCCC
          --
          -- ‣ sh₁ is multiline non-aligned
          -- ‣ sh₂ is multiline aligned
          --
          --     □□□□XXX  ⧺  □□□□AAA   =  □□□□XXX
          --     YY          ⋅⋅⋅⋅BB       YY
          --     ZZZZ        ⋅⋅⋅⋅CCCC     ZZZZAAA
          --                              ␣␣␣␣BB
          --                              ␣␣␣␣CCCC
          --
          -- ‣ sh₁ is multiline aligned
          -- ‣ sh₂ is multiline aligned 
          --
          --     □□□□XXX   ⧺  □□□□AAA   =  □□□□XXX
          --     ⋅⋅⋅⋅YY       ⋅⋅⋅⋅BB       ⋅⋅⋅⋅YY
          --     ⋅⋅⋅⋅ZZZZ     ⋅⋅⋅⋅CCCC     ⋅⋅⋅⋅ZZZZAAA
          --                               ⋅⋅⋅⋅␣␣␣␣BB
          --                               ⋅⋅⋅⋅␣␣␣␣CCCC
          --
          else 
            let MultiLine (ShapeM fl mml ll nls) = sh₂
            in MultiLine $ ShapeM fl (mml + shapeLastLength sh₁) (ll + shapeLastLength sh₁) nls
    in ShapeA (a₁ ⩔ shape singleLineL sh₁ ⩓ a₂) $ sh₁ ⧺ sh₂'
instance Monoid ShapeA
