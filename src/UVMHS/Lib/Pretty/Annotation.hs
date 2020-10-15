module UVMHS.Lib.Pretty.Annotation where

import UVMHS.Core

-----------
-- Color --
-----------

data Color3Bit =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq,Ord,Show)
data Color = 
    Color Color3Bit
  | Color8 ℕ8
  | Color24 ℕ8 ℕ8 ℕ8
  deriving (Eq,Ord,Show)

black,darkRed,darkGreen,darkYellow,darkBlue,darkPink,darkTeal,gray ∷ Color
darkGray,red,green,yellow,blue,pink,teal,lightGray ∷ Color
white,highlight ∷ Color

black = Color Black
red = Color Red
green = Color Green
yellow = Color Yellow
blue = Color Blue
pink = Color Magenta
teal = Color Cyan
white = Color24 (𝕟8 0) (𝕟8 0) (𝕟8 0)

darkRed = Color8 $ 𝕟8 1
darkGreen = Color8 $ 𝕟8 2
darkYellow = Color8 $ 𝕟8 3
darkBlue = Color8 $ 𝕟8 4
darkPink = Color8 $ 𝕟8 5
darkTeal = Color8 $ 𝕟8 6
gray = Color8 $ 𝕟8 7

darkGray = Color8 $ 𝕟8 8
lightGray = Color8 $ 𝕟8 15

highlight = Color8 $ 𝕟8 229

-------------
-- Formats --
-------------

data Format = 
    FG Color
  | NOFG
  | BG Color
  | NOBG
  | UL
  | NOUL
  | BD
  | NOBD
  | IT
  | NOIT
  deriving (Eq, Ord,Show)

data Formats = Formats
  { fgFormats ∷ 𝑂 (𝑂 Color)
  , bgFormats ∷ 𝑂 (𝑂 Color)
  , ulFormats ∷ 𝑂 𝔹
  , bdFormats ∷ 𝑂 𝔹
  , itFormats ∷ 𝑂 𝔹
  } deriving (Eq,Ord,Show)
instance Null Formats where null = Formats None None None None None
instance Append Formats where
  Formats fg₁ bg₁ ul₁ bd₁ it₁ ⧺ Formats fg₂ bg₂ ul₂ bd₂ it₂ = 
    Formats (first fg₁ fg₂) (first bg₁ bg₂) (first ul₁ ul₂) (first bd₁ bd₂) (first it₁ it₂)
instance Monoid Formats

format ∷ Format → Formats
format (FG c) = Formats (Some (Some c)) None None None None
format NOFG = Formats (Some None) None None None None
format (BG c) = Formats None (Some (Some c)) None None None
format NOBG = Formats None (Some None) None None None
format UL   = Formats None None (Some True) None None
format NOUL = Formats None None (Some False) None None
format BD   = Formats None None None (Some True) None
format NOBD = Formats None None None (Some False) None
format IT = Formats None None None None (Some True)
format NOIT = Formats None None None None (Some False)

formats ∷ (ToIter Format t) ⇒ t → Formats
formats = concat ∘ map format ∘ iter

override ∷ 𝐿 Format
override = list [NOFG,NOBG,NOUL,NOBD,NOIT]

----------------
-- Annotation --
----------------

data Annotation = Annotation
  { annotationFormats ∷ Formats
  , annotationUndertag ∷ 𝑂 (ℂ ∧ Formats)
  } deriving (Eq,Ord,Show)

instance Null Annotation where
  null = Annotation null None
instance Append Annotation where
  Annotation a₁ u₁ ⧺ Annotation a₂ u₂ = Annotation (a₁ ⧺ a₂) (last u₁ u₂)
instance Monoid Annotation

formatAnnotation ∷ Formats → Annotation
formatAnnotation fm = Annotation fm None

undertagAnnotation ∷ ℂ → Formats → Annotation
undertagAnnotation c fm = Annotation null $ Some (c :* fm)
