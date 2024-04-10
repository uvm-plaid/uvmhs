module UVMHS.Lib.Pretty.ConsoleHTML where

import UVMHS.Core

import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Console
import UVMHS.Lib.Pretty.Class
import UVMHS.Lib.Pretty.Core

htmlColor ∷ 𝑂 Color → 𝕊
htmlColor None = "initial"
htmlColor (Some (Color c)) = case c of
  Black → "black"
  Red → "red"
  Green → "lime"
  Yellow → "yellow"
  Blue → "blue"
  Magenta → "fuchsia"
  Cyan → "aqua"
  White → "white"
htmlColor (Some (Color8 c)) = htmlColorFrom256 ⋕! c
htmlColor (Some (Color24 r g b)) = "rgb(" ⧺ show𝕊 r ⧺ "," ⧺ show𝕊 g ⧺ "," ⧺ show𝕊 b ⧺ ")"

htmlFGCode ∷ 𝑂 Color → 𝑄 𝕊 → 𝑄 𝕊
htmlFGCode c s = concat
  [ single "<span style='color:"
  , single $ htmlColor c
  , single "'>"
  , s
  , single "</span>"
  ]

htmlBGCode ∷ 𝑂 Color → 𝑄 𝕊 → 𝑄 𝕊
htmlBGCode c s = concat
  [ single $ concat
      [ "<span style='background-color:"
      , htmlColor c
      , "'>"
      ]
  , s
  , single "</span>"
  ]

htmlULCode ∷ 𝔹 → 𝑄 𝕊 → 𝑄 𝕊
htmlULCode True s = concat [single "<u>",s,single "</u>"]
htmlULCode False s = concat [single "<span style='text-decoration:none'>",s,single "</span>"]

htmlBDCode ∷ 𝔹 → 𝑄 𝕊 → 𝑄 𝕊
htmlBDCode True s = concat [single "<b>",s,single"</b>"]
htmlBDCode False s = concat[single "<span style='font-weight:normal'>",s,single "</span>"]

htmlITCode ∷ 𝔹 → 𝑄 𝕊 → 𝑄 𝕊
htmlITCode True s = concat [single "<em>",s,single"</em>"]
htmlITCode False s = concat [single "<span style='font-style:normal'>",s,single"</span>"]

consoleFormatHTML ∷ Formats → 𝑄 𝕊 → 𝑄 𝕊
consoleFormatHTML (Formats fgM bgM ulM bdM itM) = compose $ concat $ map (mzero𝑂 @ 𝑄)
  [ htmlFGCode ^$ fgM
  , htmlBGCode ^$ bgM
  , htmlULCode ^$ ulM
  , htmlBDCode ^$ bdM
  , htmlITCode ^$ itM
  ]

htmlEscapeChar ∷ ℂ → 𝕊
htmlEscapeChar c
  | c == '&' = "&amp;"
  | c == '<' = "&lt;"
  | c == '>' = "&gt;"
  | otherwise = single𝕊 c

htmlEscape ∷ 𝕊 → 𝑄 𝕊
htmlEscape = seq ∘ map htmlEscapeChar ∘ iter

renderConsoleOutHTML ∷ ConsoleOut → 𝑄 𝕊 ∧ ()
renderConsoleOutHTML NullCO = skip
renderConsoleOutHTML (ChunkCO s) = tell $ htmlEscape s
renderConsoleOutHTML (AppendCO o₁ o₂) = exec [renderConsoleOutHTML o₁,renderConsoleOutHTML o₂]
renderConsoleOutHTML (FormatCO f o) = mapOut (consoleFormatHTML f) $ renderConsoleOutHTML o

execConsoleOutHTML ∷ ConsoleOut → 𝑄 𝕊
execConsoleOutHTML = snd ∘ retOut ∘ renderConsoleOutHTML

prenderHTMLWith ∷ (Pretty a) ⇒ (Doc → Doc) → a → 𝑄 𝕊
prenderHTMLWith f = execConsoleOutHTML ∘ execPrettyOut ∘ execDoc ∘ f ∘ pretty

prenderHTMLWidth ∷ (Pretty a) ⇒ ℕ → a → 𝑄 𝕊
prenderHTMLWidth = prenderHTMLWith ∘ onDoc ∘ mapEnv ∘ update maxColumnWidthL

prenderHTML ∷ (Pretty a) ⇒ a → 𝑄 𝕊
prenderHTML = prenderHTMLWith id

pprintHTML ∷ (Pretty a) ⇒ a → IO ()
pprintHTML = out ∘ concat ∘ prenderHTML

prenderHTMLStandalone ∷ (Pretty a) ⇒ a → 𝑄 𝕊
prenderHTMLStandalone x = concat
  [ single $ concat
      [ "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
      , "<pre>"
      ]
  , prenderHTML x
  , single "</pre>"
  ]

pprintHTMLStandalone ∷ (Pretty a) ⇒ a → IO ()
pprintHTMLStandalone = out ∘ concat ∘ prenderHTMLStandalone

htmlColorFrom256 ∷ ℕ8 ⇰ 𝕊
htmlColorFrom256 = dict
  [ 𝕟8 000 ↦ "#000000"
  , 𝕟8 001 ↦ "#800000"
  , 𝕟8 002 ↦ "#008000"
  , 𝕟8 003 ↦ "#808000"
  , 𝕟8 004 ↦ "#000080"
  , 𝕟8 005 ↦ "#800080"
  , 𝕟8 006 ↦ "#008080"
  , 𝕟8 007 ↦ "#c0c0c0"
  , 𝕟8 008 ↦ "#808080"
  , 𝕟8 009 ↦ "#ff0000"
  , 𝕟8 010 ↦ "#00ff00"
  , 𝕟8 011 ↦ "#ffff00"
  , 𝕟8 012 ↦ "#0000ff"
  , 𝕟8 013 ↦ "#ff00ff"
  , 𝕟8 014 ↦ "#00ffff"
  , 𝕟8 015 ↦ "#ffffff"
  , 𝕟8 016 ↦ "#000000"
  , 𝕟8 017 ↦ "#00005f"
  , 𝕟8 018 ↦ "#000087"
  , 𝕟8 019 ↦ "#0000af"
  , 𝕟8 020 ↦ "#0000d7"
  , 𝕟8 021 ↦ "#0000ff"
  , 𝕟8 022 ↦ "#005f00"
  , 𝕟8 023 ↦ "#005f5f"
  , 𝕟8 024 ↦ "#005f87"
  , 𝕟8 025 ↦ "#005faf"
  , 𝕟8 026 ↦ "#005fd7"
  , 𝕟8 027 ↦ "#005fff"
  , 𝕟8 028 ↦ "#008700"
  , 𝕟8 029 ↦ "#00875f"
  , 𝕟8 030 ↦ "#008787"
  , 𝕟8 031 ↦ "#0087af"
  , 𝕟8 032 ↦ "#0087d7"
  , 𝕟8 033 ↦ "#0087ff"
  , 𝕟8 034 ↦ "#00af00"
  , 𝕟8 035 ↦ "#00af5f"
  , 𝕟8 036 ↦ "#00af87"
  , 𝕟8 037 ↦ "#00afaf"
  , 𝕟8 038 ↦ "#00afd7"
  , 𝕟8 039 ↦ "#00afff"
  , 𝕟8 040 ↦ "#00d700"
  , 𝕟8 041 ↦ "#00d75f"
  , 𝕟8 042 ↦ "#00d787"
  , 𝕟8 043 ↦ "#00d7af"
  , 𝕟8 044 ↦ "#00d7d7"
  , 𝕟8 045 ↦ "#00d7ff"
  , 𝕟8 046 ↦ "#00ff00"
  , 𝕟8 047 ↦ "#00ff5f"
  , 𝕟8 048 ↦ "#00ff87"
  , 𝕟8 049 ↦ "#00ffaf"
  , 𝕟8 050 ↦ "#00ffd7"
  , 𝕟8 051 ↦ "#00ffff"
  , 𝕟8 052 ↦ "#5f0000"
  , 𝕟8 053 ↦ "#5f005f"
  , 𝕟8 054 ↦ "#5f0087"
  , 𝕟8 055 ↦ "#5f00af"
  , 𝕟8 056 ↦ "#5f00d7"
  , 𝕟8 057 ↦ "#5f00ff"
  , 𝕟8 058 ↦ "#5f5f00"
  , 𝕟8 059 ↦ "#5f5f5f"
  , 𝕟8 060 ↦ "#5f5f87"
  , 𝕟8 061 ↦ "#5f5faf"
  , 𝕟8 062 ↦ "#5f5fd7"
  , 𝕟8 063 ↦ "#5f5fff"
  , 𝕟8 064 ↦ "#5f8700"
  , 𝕟8 065 ↦ "#5f875f"
  , 𝕟8 066 ↦ "#5f8787"
  , 𝕟8 067 ↦ "#5f87af"
  , 𝕟8 068 ↦ "#5f87d7"
  , 𝕟8 069 ↦ "#5f87ff"
  , 𝕟8 070 ↦ "#5faf00"
  , 𝕟8 071 ↦ "#5faf5f"
  , 𝕟8 072 ↦ "#5faf87"
  , 𝕟8 073 ↦ "#5fafaf"
  , 𝕟8 074 ↦ "#5fafd7"
  , 𝕟8 075 ↦ "#5fafff"
  , 𝕟8 076 ↦ "#5fd700"
  , 𝕟8 077 ↦ "#5fd75f"
  , 𝕟8 078 ↦ "#5fd787"
  , 𝕟8 079 ↦ "#5fd7af"
  , 𝕟8 080 ↦ "#5fd7d7"
  , 𝕟8 081 ↦ "#5fd7ff"
  , 𝕟8 082 ↦ "#5fff00"
  , 𝕟8 083 ↦ "#5fff5f"
  , 𝕟8 084 ↦ "#5fff87"
  , 𝕟8 085 ↦ "#5fffaf"
  , 𝕟8 086 ↦ "#5fffd7"
  , 𝕟8 087 ↦ "#5fffff"
  , 𝕟8 088 ↦ "#870000"
  , 𝕟8 089 ↦ "#87005f"
  , 𝕟8 090 ↦ "#870087"
  , 𝕟8 091 ↦ "#8700af"
  , 𝕟8 092 ↦ "#8700d7"
  , 𝕟8 093 ↦ "#8700ff"
  , 𝕟8 094 ↦ "#875f00"
  , 𝕟8 095 ↦ "#875f5f"
  , 𝕟8 096 ↦ "#875f87"
  , 𝕟8 097 ↦ "#875faf"
  , 𝕟8 098 ↦ "#875fd7"
  , 𝕟8 099 ↦ "#875fff"
  , 𝕟8 100 ↦ "#878700"
  , 𝕟8 101 ↦ "#87875f"
  , 𝕟8 102 ↦ "#878787"
  , 𝕟8 103 ↦ "#8787af"
  , 𝕟8 104 ↦ "#8787d7"
  , 𝕟8 105 ↦ "#8787ff"
  , 𝕟8 106 ↦ "#87af00"
  , 𝕟8 107 ↦ "#87af5f"
  , 𝕟8 108 ↦ "#87af87"
  , 𝕟8 109 ↦ "#87afaf"
  , 𝕟8 110 ↦ "#87afd7"
  , 𝕟8 111 ↦ "#87afff"
  , 𝕟8 112 ↦ "#87d700"
  , 𝕟8 113 ↦ "#87d75f"
  , 𝕟8 114 ↦ "#87d787"
  , 𝕟8 115 ↦ "#87d7af"
  , 𝕟8 116 ↦ "#87d7d7"
  , 𝕟8 117 ↦ "#87d7ff"
  , 𝕟8 118 ↦ "#87ff00"
  , 𝕟8 119 ↦ "#87ff5f"
  , 𝕟8 120 ↦ "#87ff87"
  , 𝕟8 121 ↦ "#87ffaf"
  , 𝕟8 122 ↦ "#87ffd7"
  , 𝕟8 123 ↦ "#87ffff"
  , 𝕟8 124 ↦ "#af0000"
  , 𝕟8 125 ↦ "#af005f"
  , 𝕟8 126 ↦ "#af0087"
  , 𝕟8 127 ↦ "#af00af"
  , 𝕟8 128 ↦ "#af00d7"
  , 𝕟8 129 ↦ "#af00ff"
  , 𝕟8 130 ↦ "#af5f00"
  , 𝕟8 131 ↦ "#af5f5f"
  , 𝕟8 132 ↦ "#af5f87"
  , 𝕟8 133 ↦ "#af5faf"
  , 𝕟8 134 ↦ "#af5fd7"
  , 𝕟8 135 ↦ "#af5fff"
  , 𝕟8 136 ↦ "#af8700"
  , 𝕟8 137 ↦ "#af875f"
  , 𝕟8 138 ↦ "#af8787"
  , 𝕟8 139 ↦ "#af87af"
  , 𝕟8 140 ↦ "#af87d7"
  , 𝕟8 141 ↦ "#af87ff"
  , 𝕟8 142 ↦ "#afaf00"
  , 𝕟8 143 ↦ "#afaf5f"
  , 𝕟8 144 ↦ "#afaf87"
  , 𝕟8 145 ↦ "#afafaf"
  , 𝕟8 146 ↦ "#afafd7"
  , 𝕟8 147 ↦ "#afafff"
  , 𝕟8 148 ↦ "#afd700"
  , 𝕟8 149 ↦ "#afd75f"
  , 𝕟8 150 ↦ "#afd787"
  , 𝕟8 151 ↦ "#afd7af"
  , 𝕟8 152 ↦ "#afd7d7"
  , 𝕟8 153 ↦ "#afd7ff"
  , 𝕟8 154 ↦ "#afff00"
  , 𝕟8 155 ↦ "#afff5f"
  , 𝕟8 156 ↦ "#afff87"
  , 𝕟8 157 ↦ "#afffaf"
  , 𝕟8 158 ↦ "#afffd7"
  , 𝕟8 159 ↦ "#afffff"
  , 𝕟8 160 ↦ "#d70000"
  , 𝕟8 161 ↦ "#d7005f"
  , 𝕟8 162 ↦ "#d70087"
  , 𝕟8 163 ↦ "#d700af"
  , 𝕟8 164 ↦ "#d700d7"
  , 𝕟8 165 ↦ "#d700ff"
  , 𝕟8 166 ↦ "#d75f00"
  , 𝕟8 167 ↦ "#d75f5f"
  , 𝕟8 168 ↦ "#d75f87"
  , 𝕟8 169 ↦ "#d75faf"
  , 𝕟8 170 ↦ "#d75fd7"
  , 𝕟8 171 ↦ "#d75fff"
  , 𝕟8 172 ↦ "#d78700"
  , 𝕟8 173 ↦ "#d7875f"
  , 𝕟8 174 ↦ "#d78787"
  , 𝕟8 175 ↦ "#d787af"
  , 𝕟8 176 ↦ "#d787d7"
  , 𝕟8 177 ↦ "#d787ff"
  , 𝕟8 178 ↦ "#d7af00"
  , 𝕟8 179 ↦ "#d7af5f"
  , 𝕟8 180 ↦ "#d7af87"
  , 𝕟8 181 ↦ "#d7afaf"
  , 𝕟8 182 ↦ "#d7afd7"
  , 𝕟8 183 ↦ "#d7afff"
  , 𝕟8 184 ↦ "#d7d700"
  , 𝕟8 185 ↦ "#d7d75f"
  , 𝕟8 186 ↦ "#d7d787"
  , 𝕟8 187 ↦ "#d7d7af"
  , 𝕟8 188 ↦ "#d7d7d7"
  , 𝕟8 189 ↦ "#d7d7ff"
  , 𝕟8 190 ↦ "#d7ff00"
  , 𝕟8 191 ↦ "#d7ff5f"
  , 𝕟8 192 ↦ "#d7ff87"
  , 𝕟8 193 ↦ "#d7ffaf"
  , 𝕟8 194 ↦ "#d7ffd7"
  , 𝕟8 195 ↦ "#d7ffff"
  , 𝕟8 196 ↦ "#ff0000"
  , 𝕟8 197 ↦ "#ff005f"
  , 𝕟8 198 ↦ "#ff0087"
  , 𝕟8 199 ↦ "#ff00af"
  , 𝕟8 200 ↦ "#ff00d7"
  , 𝕟8 201 ↦ "#ff00ff"
  , 𝕟8 202 ↦ "#ff5f00"
  , 𝕟8 203 ↦ "#ff5f5f"
  , 𝕟8 204 ↦ "#ff5f87"
  , 𝕟8 205 ↦ "#ff5faf"
  , 𝕟8 206 ↦ "#ff5fd7"
  , 𝕟8 207 ↦ "#ff5fff"
  , 𝕟8 208 ↦ "#ff8700"
  , 𝕟8 209 ↦ "#ff875f"
  , 𝕟8 210 ↦ "#ff8787"
  , 𝕟8 211 ↦ "#ff87af"
  , 𝕟8 212 ↦ "#ff87d7"
  , 𝕟8 213 ↦ "#ff87ff"
  , 𝕟8 214 ↦ "#ffaf00"
  , 𝕟8 215 ↦ "#ffaf5f"
  , 𝕟8 216 ↦ "#ffaf87"
  , 𝕟8 217 ↦ "#ffafaf"
  , 𝕟8 218 ↦ "#ffafd7"
  , 𝕟8 219 ↦ "#ffafff"
  , 𝕟8 220 ↦ "#ffd700"
  , 𝕟8 221 ↦ "#ffd75f"
  , 𝕟8 222 ↦ "#ffd787"
  , 𝕟8 223 ↦ "#ffd7af"
  , 𝕟8 224 ↦ "#ffd7d7"
  , 𝕟8 225 ↦ "#ffd7ff"
  , 𝕟8 226 ↦ "#ffff00"
  , 𝕟8 227 ↦ "#ffff5f"
  , 𝕟8 228 ↦ "#ffff87"
  , 𝕟8 229 ↦ "#ffffaf"
  , 𝕟8 230 ↦ "#ffffd7"
  , 𝕟8 231 ↦ "#ffffff"
  , 𝕟8 232 ↦ "#080808"
  , 𝕟8 233 ↦ "#121212"
  , 𝕟8 234 ↦ "#1c1c1c"
  , 𝕟8 235 ↦ "#262626"
  , 𝕟8 236 ↦ "#303030"
  , 𝕟8 237 ↦ "#3a3a3a"
  , 𝕟8 238 ↦ "#444444"
  , 𝕟8 239 ↦ "#4e4e4e"
  , 𝕟8 240 ↦ "#585858"
  , 𝕟8 241 ↦ "#626262"
  , 𝕟8 242 ↦ "#6c6c6c"
  , 𝕟8 243 ↦ "#767676"
  , 𝕟8 244 ↦ "#808080"
  , 𝕟8 245 ↦ "#8a8a8a"
  , 𝕟8 246 ↦ "#949494"
  , 𝕟8 247 ↦ "#9e9e9e"
  , 𝕟8 248 ↦ "#a8a8a8"
  , 𝕟8 249 ↦ "#b2b2b2"
  , 𝕟8 250 ↦ "#bcbcbc"
  , 𝕟8 251 ↦ "#c6c6c6"
  , 𝕟8 252 ↦ "#d0d0d0"
  , 𝕟8 253 ↦ "#dadada"
  , 𝕟8 254 ↦ "#e4e4e4"
  , 𝕟8 255 ↦ "#eeeeee"
  ]
