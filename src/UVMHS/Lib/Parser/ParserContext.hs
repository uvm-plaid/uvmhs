module UVMHS.Lib.Parser.ParserContext where

import UVMHS.Core

import UVMHS.Lib.Window
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.Loc

data ParserContext = ParserContext
  { parserContextLocRange ∷ LocRange
  , parserContextDisplayL ∷ WindowL Doc Doc
  , parserContextDisplayR ∷ WindowR Doc Doc
  , parserContextError ∷ WindowR Doc Doc
  }
makeLenses ''ParserContext
makePrettySum ''ParserContext

instance Null ParserContext where 
  null = ParserContext bot null null null
instance Append ParserContext where 
  ParserContext l₁ dL₁ dR₁ e₁ ⧺ ParserContext l₂ dL₂ dR₂ e₂ = ParserContext (l₁ ⊔ l₂) (dL₁ ⧺ dL₂) (dR₁ ⧺ dR₂) (e₁ ⧺ e₂)
instance Monoid ParserContext

formatParserContext ∷ Formats → ParserContext → ParserContext
formatParserContext fmt (ParserContext lr dL dR e) =
  ParserContext lr (mapWindowL (ppFormat fmt) (ppFormat fmt) dL)
                   (mapWindowR (ppFormat fmt) (ppFormat fmt) dR)
                   (mapWindowR (ppFormat fmt) (ppFormat fmt) e)

data FullContext = FullContext
  { fullContextPrefix ∷ WindowR Doc Doc
  , fullContextContext ∷ WindowL Doc Doc
  , fullContextSuffix ∷ WindowL Doc Doc
  }

instance Pretty FullContext where
  pretty (FullContext pre d pi) = concat
    [ ppFormat (formats [BG lightGray]) $ ppString "«"
    , ppAlign $ concat
        [ renderWindowR pre 
        , ppUT '^' green $ renderWindowL d
        , renderWindowL pi
        ]
    , ppFormat (formats [BG lightGray]) $ ppString "»"
    ]

instance Show FullContext where show = chars ∘ ppshow
