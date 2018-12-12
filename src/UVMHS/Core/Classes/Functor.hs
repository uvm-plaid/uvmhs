module UVMHS.Core.Classes.Functor where

import UVMHS.Init
import UVMHS.Core.Classes.Morphism

infixr 0 ^$, ^^$
infixl 6 ^∘, ^^∘
infixl 9 ^⋅, ^^⋅

class Functor (t ∷ ★ → ★) where map ∷ (a → b) → (t a → t b)

mapOn ∷ (Functor t) ⇒ t a → (a → b) → t b 
mapOn = flip map

mapp ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
mapp = map ∘ map

(^⋅) ∷ (Functor t) ⇒ (a → b) → t a → t b 
(^⋅) = map

(^^⋅) ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
(^^⋅) = mapp

(^$) ∷ (Functor t) ⇒ (a → b) → t a → t b 
(^$) = map

(^^$) ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
(^^$) = mapp

(^∘) ∷ (Functor t) ⇒ (b → c) → (a → t b) → a → t c 
g ^∘ f = map g ∘ f

(^^∘) ∷ (Functor t,Functor u) ⇒ (b → c) → (a → t (u b)) → a → t (u c)
g ^^∘ f = mapp g ∘ f

class Functor2 (w ∷ (★ → ★) → (★ → ★)) where map2 ∷ (t →⁻ u) → w t →⁻ w u
class Functor2Iso (w ∷ (★ → ★) → (★ → ★)) where map2iso ∷ Iso2 t u → w t →⁻ w u
