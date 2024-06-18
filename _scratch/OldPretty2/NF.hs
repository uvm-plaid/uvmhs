module UVMHS.Lib.Pretty.NF where

import UVMHS.Core

import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Core

data NFEnv = NFEnv
  { nfformats ∷ Formats
  , nfundertagFormats ∷ 𝑂 (ℂ ∧ Formats)
  }
makeLenses ''NFEnv

data OutputElemNF =
    LineNumberNF Formats ℕ
  | CharNF Formats (𝑂 (ℂ ∧ Formats)) ℂ
  | NewlineNF
  deriving (Eq,Ord,Show)

chunkNF ∷ Chunk → ReaderT NFEnv 𝑄 OutputElemNF
chunkNF = \case
  LineNumber n → do
    fmt ← askL nfformatsL
    return $ LineNumberNF fmt n
  Text t → do
    fmts ← askL nfformatsL
    ufmts ← askL nfundertagFormatsL
    c ← from t
    return $ CharNF fmts ufmts c
  Newline → return NewlineNF

annotatedOutputNF ∷ Annotation → Output → ReaderT NFEnv 𝑄 OutputElemNF
annotatedOutputNF a o = case a of
  FormatA fmts → do
    mapEnvL nfformatsL ((⧺) $ concat $ map formats $ iter fmts) $ outputNF o
  UndertagA fmtscO →
    let f = case fmtscO of
          None → None
          Some (fmts :* c) → Some (c :* (concat $ map formats $ iter fmts))
    in localL nfundertagFormatsL f $ outputNF o

outputNF ∷ Output → ReaderT NFEnv 𝑄 OutputElemNF
outputNF os = do
  o ← from os
  case o of
    RawChunk c → chunkNF c
    AnnotatedOutput a os' → annotatedOutputNF a os'

prettyNFOutput ∷ Output → 𝑄 OutputElemNF
prettyNFOutput o = runReaderT (NFEnv null None) $ outputNF o

prettyNFDoc ∷ Doc → 𝑄 OutputElemNF
prettyNFDoc = prettyNFOutput ∘ output ∘ execDoc
