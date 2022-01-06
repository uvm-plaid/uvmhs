module UVMHS.Core.Data.Char where

import UVMHS.Core.Init

import qualified Prelude as HS
import qualified Data.Char as HS

toLower ∷ ℂ → ℂ
toLower = HS.toLower

toUpper ∷ ℂ → ℂ
toUpper = HS.toUpper

isSpace ∷ ℂ → 𝔹
isSpace = HS.isSpace

isAlphaNum ∷ ℂ → 𝔹
isAlphaNum = HS.isAlphaNum

isLetter ∷ ℂ → 𝔹
isLetter = HS.isLetter

isNumber ∷ ℂ → 𝔹
isNumber = HS.isNumber

isDigit ∷ ℂ → 𝔹
isDigit = HS.isDigit

chrFrNat ∷ ℕ64 → ℂ
chrFrNat = HS.chr ∘ HS.fromIntegral

natFrChr ∷ ℂ → ℕ64
natFrChr = HS.fromIntegral ∘ HS.ord


