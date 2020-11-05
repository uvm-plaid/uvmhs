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

altBlack,altWhite ∷ Color
altNight,altNightLight,altRed,altRedLight,altGreen,altGreenLight,altOrange,altOrangeLight ∷ Color
altBlue,altBlueLight,altPurple,altPurpleLight,altTeal,altTealLight,altGray,altGrayLight ∷ Color

-- colors borrowed from terminal.sexy
-- altBlack       = Color24 (𝕟8 0)   (𝕟8 0)   $ 𝕟8 0
-- altWhite       = Color24 (𝕟8 255) (𝕟8 255) $ 𝕟8 255
-- 
-- altNight       = Color24 (𝕟8 40)  (𝕟8 42)  $ 𝕟8 46
-- altNightLight  = Color24 (𝕟8 55)  (𝕟8 59)  $ 𝕟8 65
-- altRed         = Color24 (𝕟8 165) (𝕟8 66)  $ 𝕟8 66
-- altRedLight    = Color24 (𝕟8 204) (𝕟8 102) $ 𝕟8 102
-- altGreen       = Color24 (𝕟8 140) (𝕟8 148) $ 𝕟8 64
-- altGreenLight  = Color24 (𝕟8 181) (𝕟8 189) $ 𝕟8 104
-- altOrange      = Color24 (𝕟8 222) (𝕟8 147) $ 𝕟8 95
-- altOrangeLight = Color24 (𝕟8 240) (𝕟8 198) $ 𝕟8 116
-- 
-- altBlue        = Color24 (𝕟8 95)  (𝕟8 129) $ 𝕟8 157
-- altBlueLight   = Color24 (𝕟8 129) (𝕟8 162) $ 𝕟8 190
-- altPurple      = Color24 (𝕟8 133) (𝕟8 103) $ 𝕟8 143
-- altPurpleLight = Color24 (𝕟8 178) (𝕟8 148) $ 𝕟8 187
-- altTeal        = Color24 (𝕟8 94)  (𝕟8 141) $ 𝕟8 135
-- altTealLight   = Color24 (𝕟8 138) (𝕟8 190) $ 𝕟8 183
-- altGray        = Color24 (𝕟8 112) (𝕟8 120) $ 𝕟8 128
-- altGrayLight   = Color24 (𝕟8 197) (𝕟8 200) $ 𝕟8 198

altBlack       = Color8 $ 𝕟8 16
altWhite       = Color8 $ 𝕟8 231

altNight       = Color8 $ 𝕟8 234
altNightLight  = Color8 $ 𝕟8 241
altRed         = Color8 $ 𝕟8 124
altRedLight    = Color8 $ 𝕟8 202
altGreen       = Color8 $ 𝕟8 22
altGreenLight  = Color8 $ 𝕟8 40
altOrange      = Color8 $ 𝕟8 172
altOrangeLight = Color8 $ 𝕟8 220

altBlue        = Color8 $ 𝕟8 33
altBlueLight   = Color8 $ 𝕟8 81
altPurple      = Color8 $ 𝕟8 97
altPurpleLight = Color8 $ 𝕟8 145
altTeal        = Color8 $ 𝕟8 30
altTealLight   = Color8 $ 𝕟8 108
altGray        = Color8 $ 𝕟8 246
altGrayLight   = Color8 $ 𝕟8 253

allColors ∷ 𝐿 (𝕊 ∧ Color)
allColors = frhs
  [ ("defaultColor"      ,defaultColor  )
  , ("black"             ,black         )
  , ("darkGray"          ,darkGray      )
  , ("lightGray"         ,lightGray     )
  , ("white"             ,white         )
  , ("red"               ,red           )
  , ("lightRed"          ,lightRed      )
  , ("green"             ,green         )
  , ("lightGreen"        ,lightGreen    )
  , ("yellow"            ,yellow        )
  , ("lightYellow"       ,lightYellow   )
  , ("blue"              ,blue          )
  , ("lightBlue"         ,lightBlue     )
  , ("pink"              ,pink          )
  , ("lightPink"         ,lightPink     )
  , ("teal"              ,teal          )
  , ("lightTeal"         ,lightTeal     )
  , ("highlight"         ,highlight     )
  , ("altBlack"          ,altBlack      )
  , ("altWhite"          ,altWhite      )
  , ("altNight"          ,altNight      )
  , ("altRed"            ,altRed        )
  , ("altGreen"          ,altGreen      )
  , ("altOrange"         ,altOrange     )
  , ("altBlue"           ,altBlue       )
  , ("altPurple"         ,altPurple     )
  , ("altTeal"           ,altTeal       )
  , ("altGray"           ,altGray       )
  , ("altNightLight"     ,altNightLight )
  , ("altRedLight"       ,altRedLight   )
  , ("altGreenLight"     ,altGreenLight )
  , ("altOrangeLight"    ,altOrangeLight)
  , ("altBlueLight"      ,altBlueLight  )
  , ("altPurpleLight"    ,altPurpleLight)
  , ("altTealLight"      ,altTealLight  )
  , ("altGrayLight"      ,altGrayLight  )
  ]


