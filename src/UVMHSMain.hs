{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified UVMHSContrib.Lang.Arith as Arith
import qualified UVMHSContrib.Lang.SExp as SExp

import qualified Data.Time.Clock as T
import qualified Prelude as HS

import qualified UVMHS.Lib.Parser.Regex as R

data TokenClass = NameT | IntegerT | DoubleT | StringT | KeywordT | WhitespaceT | CommentT
  deriving (Eq,Ord,Show)
data Token = Token { tokenClass ∷ TokenClass , tokenValue ∷ 𝕊 }
  deriving (Eq,Ord,Show)

makePrettySum ''TokenClass
makePrettySum ''Token

lKeyword ∷ (Ord u,Additive u) ⇒ Regex CharClass ℂ TokenClass u
lKeyword = concat $ map lWord ["let","=","in","+","(",")"]

mainRegex ∷ Regex CharClass ℂ TokenClass ℕ64
mainRegex = concat
  [ lInt ▷ outRegex (𝕟64 2) (formats [FG red]) IntegerT
  , lDbl ▷ outRegex (𝕟64 1) (formats [FG green]) DoubleT
  , lString ▷ outRegex (𝕟64 1) (formats [FG teal]) StringT
  , lKeyword ▷ outRegex (𝕟64 2) (formats [FG yellow]) KeywordT
  , lName ▷ outRegex (𝕟64 1) (formats [FG blue]) NameT
  , lSpace ▷ outRegex (𝕟64 1) (formats []) WhitespaceT
  , lCommentMLOpen ▷ outRegex (𝕟64 1) (formats [FG gray]) CommentT
  ]

commentRegex ∷ Regex CharClass ℂ TokenClass ℕ64
commentRegex = lCommentMLBody ▷ outRegex (𝕟64 1) (formats [FG gray]) CommentT

buildToken ∷ 𝐼S ℂ → 𝑂 TokenClass → 𝔹 ∧ Token
buildToken _ None = error "untagged token result"
buildToken cs (Some c) = (c ∈ pow [WhitespaceT,CommentT]) :* Token c (stringS cs)

lNatural' ∷ (Ord u,Ord o,Additive u) ⇒ Regex CharClass ℂ o u
lNatural' = oomRegex $ concat $ map tokRegex ['0'..'9']

main ∷ IO ()
main = out "¯\\_﹙ツ﹚_/¯"
