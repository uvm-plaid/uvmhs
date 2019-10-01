module UVMHS.Core.Classes.Comonad where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Functor

infixr 2 =≫ 

class Extract (w ∷ ★ → ★) where extract ∷ w a → a
class Cobind (w ∷ ★ → ★) where (=≫) ∷ w a → (w a → b) → w b
class (Functor w,Extract w,Cobind w) ⇒ Comonad w

wextend ∷ (Cobind w) ⇒ (w a → b) → w a → w b
wextend f xM = xM =≫ f

(%⋅) ∷ (Cobind w) ⇒ (w a → b) → w a → w b
(%⋅) = wextend

(%$) ∷ (Cobind w) ⇒ (w a → b) → w a → w b
(%$) = wextend

(%∘) ∷ (Cobind w) ⇒ (w b → c) → (w a → b) → (w a → c)
g %∘ f = g ∘ wextend f

kextract ∷ (Extract w) ⇒ (a → b) → w a → b
kextract f = f ∘ extract

kextract2 ∷ (Extract w) ⇒ (a → b → c) → w a → w b → c
kextract2 f xW yW = f (extract xW) (extract yW)

siphon ∷ (Cobind w) ⇒ w a → b → w b
siphon xW y = xW =≫ \ _ → y

submerge ∷ (Functor m,Comonad w) ⇒ w (m a) → m (w a)
submerge aMW = map (siphon aMW) (extract aMW)

wmap ∷ (Comonad w) ⇒ (a → b) → w a → w b
wmap = wextend ∘ kextract

