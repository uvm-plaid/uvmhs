module UVMHS.Lib.Pretty.Color where

import UVMHS.Core

data Color3Bit =
    DefaultColor
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | LightGray
  | DarkGray
  | LightRed
  | LightGreen
  | LightYellow
  | LightBlue
  | LightMagenta
  | LightCyan
  | White
  deriving (Eq,Ord,Show)
data Color = 
    Color Color3Bit
  | Color8 ℕ8
  | Color24 ℕ8 ℕ8 ℕ8
  deriving (Eq,Ord,Show)

defaultColor ∷ Color

black,red,green,yellow,blue,pink,teal,lightGray ∷ Color
darkGray,lightRed,lightGreen,lightYellow,lightBlue,lightPink,lightTeal,white ∷ Color

highlight ∷ Color

defaultColor = Color DefaultColor

black     = Color Black
red       = Color Red
green     = Color Green
yellow    = Color Yellow
blue      = Color Blue
pink      = Color Magenta
teal      = Color Cyan
lightGray = Color LightGray

darkGray     = Color DarkGray
lightRed     = Color LightRed
lightGreen   = Color LightGreen
lightYellow  = Color LightYellow
lightBlue    = Color LightBlue
lightPink    = Color LightMagenta
lightTeal    = Color LightCyan
white        = Color White

highlight = Color8 $ 𝕟8 229

allColors ∷ 𝐿 (𝕊 ∧ Color)
allColors = frhs
  [ ("defaultColor"      ,defaultColor)
  , ("black"             ,black       )
  , ("darkGray"          ,darkGray    )
  , ("lightGray"         ,lightGray   )
  , ("white"             ,white       )
  , ("red"               ,red         )
  , ("lightRed"          ,lightRed    )
  , ("green"             ,green       )
  , ("lightGreen"        ,lightGreen  )
  , ("yellow"            ,yellow      )
  , ("lightYellow"       ,lightYellow )
  , ("blue"              ,blue        )
  , ("lightBlue"         ,lightBlue   )
  , ("pink"              ,pink        )
  , ("lightPink"         ,lightPink   )
  , ("teal"              ,teal        )
  , ("lightTeal"         ,lightTeal   )
  , ("highlight"         ,highlight   )
  ]


