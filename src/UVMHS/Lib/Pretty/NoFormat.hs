module UVMHS.Lib.Pretty.NoFormat where

import UVMHS.Core
import UVMHS.Lib.Pretty.Core

-- # NoFormat

noFormatChunk ∷ Chunk → 𝕊
noFormatChunk (LineNumber _) = ""
noFormatChunk (Text s) = s
noFormatChunk Newline = "\n"

noFormatOutputElem ∷ OutputElem → 𝕊
noFormatOutputElem (RawChunk s) = noFormatChunk s
noFormatOutputElem (AnnotatedOutput _ o) = noFormatOutput o

noFormatOutput ∷ Output → 𝕊
noFormatOutput = build𝕊 ∘ map noFormatOutputElem ∘ iter
