module UVMHS.Lib.Pretty.Annotation where

import UVMHS.Core
import UVMHS.Lib.Pretty.Color

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
  { fgFormats ∷ 𝑂 Color
  , bgFormats ∷ 𝑂 Color
  , ulFormats ∷ 𝑂 𝔹
  , bdFormats ∷ 𝑂 𝔹
  , itFormats ∷ 𝑂 𝔹
  } deriving (Eq,Ord,Show)
instance Null Formats where null = Formats None None None None None
instance Append Formats where
  Formats fg₁ bg₁ ul₁ bd₁ it₁ ⧺ Formats fg₂ bg₂ ul₂ bd₂ it₂ =
    Formats (first𝑂 fg₁ fg₂) (first𝑂 bg₁ bg₂) (first𝑂 ul₁ ul₂) (first𝑂 bd₁ bd₂) (first𝑂 it₁ it₂)
instance Monoid Formats

format ∷ Format → Formats
format (FG c) = Formats (Some c) None None None None
format NOFG = Formats (Some (Color DefaultColor)) None None None None
format (BG c) = Formats None (Some c) None None None
format NOBG = Formats None (Some (Color DefaultColor)) None None None
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
  Annotation a₁ u₁ ⧺ Annotation a₂ u₂ = Annotation (a₁ ⧺ a₂) (last𝑂 u₁ u₂)
instance Monoid Annotation

formatAnnotation ∷ Formats → Annotation
formatAnnotation fm = Annotation fm None

undertagAnnotation ∷ ℂ → Formats → Annotation
undertagAnnotation c fm = Annotation null $ Some (c :* fm)
