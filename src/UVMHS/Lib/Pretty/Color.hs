module UVMHS.Lib.Pretty.Color where

import UVMHS.Core

newtype Color = Color { colorCode ∷ ℕ } deriving (Eq,Ord)

instance Show Color where show = show ∘ colorCode

black,darkRed,darkGreen,darkYellow,darkBlue,darkPink,darkTeal,gray ∷ Color
darkGray,red,green,yellow,blue,pink,teal,lightGray ∷ Color
white,highlight ∷ Color

black = Color 0
darkRed = Color 1
darkGreen = Color 2
darkYellow = Color 3
darkBlue = Color 4
darkPink = Color 5
darkTeal = Color 6
gray = Color 7

darkGray = Color 8
red = Color 9
green = Color 10
yellow = Color 11
blue = Color 12
pink = Color 13
teal = Color 14
lightGray = Color 15

white = Color 255
highlight = Color 229

data Format = 
    FG Color
  | BG Color
  | UL
  | BD
  | IT
  deriving (Eq, Ord,Show)

data Formats = Formats
  { fgFormats ∷ 𝑂 Color
  , bgFormats ∷ 𝑂 Color
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
formats (FG c) = Formats (Some c) None None None None
formats (BG c) = Formats None (Some c) None None None
formats UL = Formats None None (Some True) None None
formats BD = Formats None None None (Some True) None
formats IT = Formats None None None None (Some True)

type FormatsIso = 𝑂 ℕ64 ∧ 𝑂 ℕ64 ∧ 𝑂 𝔹 ∧ 𝑂 𝔹 ∧ 𝑂 𝔹
instance Formats ⇄ FormatsIso where
  isoto (Formats fg bg ul bd it) = map (natΩ64 ∘ colorCode) fg :* map (natΩ64 ∘ colorCode) bg :* ul :* bd :* it
  isofr (fg :* bg :* ul :* bd :* it) = Formats (map (Color ∘ nat) fg) (map (Color ∘ nat) bg) ul bd it
