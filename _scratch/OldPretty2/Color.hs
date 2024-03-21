module UVMHS.Lib.Pretty.Color where

import UVMHS.Core

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

formats ∷ Format → Formats
formats (FG c) = Formats (Some (Some c)) None None None None
formats NOFG = Formats (Some None) None None None None
formats (BG c) = Formats None (Some (Some c)) None None None
formats NOBG = Formats None (Some None) None None None
formats UL   = Formats None None (Some True) None None
formats NOUL = Formats None None (Some False) None None
formats BD   = Formats None None None (Some True) None
formats NOBD = Formats None None None (Some False) None
formats IT = Formats None None None None (Some True)
formats NOIT = Formats None None None None (Some False)

override ∷ 𝐿 Format
override = list [NOFG,NOBG,NOUL,NOBD,NOIT]
