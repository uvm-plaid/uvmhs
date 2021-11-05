module UVMHS.Core.Classes.DSL where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Collections

infixr 0 ⊙$
infixr 1 ⇨
infixl 7 ⊙
infixl 7 ⊙⋆


class Arrow a where (⇨) ∷ a → a → a
class Apply a where (⊙) ∷ a → a → a
class Tup a where tup ∷ (ToIter a t) ⇒ t → a

(⊙$) ∷ (Apply e) ⇒ e → e → e
(⊙$) = (⊙)

(⊙⋆) ∷ (Apply e,ToIter e t) ⇒ e → t → e
(⊙⋆) x = fold𝐼 x (flip (⊙)) ∘ iter
