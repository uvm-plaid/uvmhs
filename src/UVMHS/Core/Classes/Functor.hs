module UVMHS.Core.Classes.Functor where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Morphism

infixr 0 ^$, ^^$
infixl 7 ^∘, ^^∘

class Functor (t ∷ ★ → ★) where map ∷ (a → b) → (t a → t b)

mapOn ∷ (Functor t) ⇒ t a → (a → b) → t b 
mapOn = flip map

mapp ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
mapp = map ∘ map

mappOn ∷ (Functor t,Functor u) ⇒ t (u a) → (a → b) → t (u b)
mappOn = flip mapp

mappp ∷ (Functor t,Functor u,Functor v) ⇒ (a → b) → t (u (v a)) → t (u (v b))
mappp = mapp ∘ map

mapppOn ∷ (Functor t,Functor u,Functor v) ⇒ t (u (v a)) → (a → b) → t (u (v b))
mapppOn = flip mappp

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
