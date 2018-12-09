module UVMHS.Core.Data.Char where

import UVMHS.Init

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

isDigit ∷ ℂ → 𝔹
isDigit = HS.isDigit

