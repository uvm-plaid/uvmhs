module UVMHS.Core.Data.LazyList where

import UVMHS.Core.Init

import UVMHS.Core.Classes

instance Null [a] where null = emptyLL
instance Append [a] where (⧺) = appendLL
instance Monoid [a]
instance Functor [] where map = mapLL
instance Return [] where return = singleLL
instance Bind [] where (≫=) = bindLL
instance Monad []
instance FunctorM [] where mapM = mapMLL
instance Single a [a] where single = singleLL
instance ToIter a [a] where iter = iterLL

emptyLL ∷ [a]
emptyLL = []

singleLL ∷ a → [a]
singleLL x = x : []

consLL ∷ a → [a] → [a]
consLL = (:)

snocLL ∷ [a] → a → [a]
snocLL xs x = case xs of
  [] → x : []
  x' : xs' → x' : snocLL xs' x

appendLL ∷ [a] → [a] → [a]
appendLL xs ys = case xs of
  [] → ys
  x : xs' → x : appendLL xs' ys

mapLL ∷ (a → b) → [a] → [b]
mapLL f xs = case xs of
  [] → []
  x : xs' → f x : map f xs'

bindLL ∷ [a] → (a → [b]) → [b]
bindLL xs k = case xs of
  [] → []
  x : xs' → appendLL (k x) (bindLL xs' k)

mapMLL ∷ (Monad m) ⇒ (a → m b) → [a] → m [b]
mapMLL f xs = case xs of
  [] → return []
  x : xs' → do
    y ← f x
    ys ← mapMLL f xs'
    return $ y : ys
