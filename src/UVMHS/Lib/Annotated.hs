module UVMHS.Lib.Annotated where

import UVMHS.Core

import UVMHS.Lib.Pretty

data 𝐴 e a = 𝐴
  { atag ∷ e
  , aval ∷ a
  } deriving (Show)
makeLenses ''𝐴
makePrettySum ''𝐴

instance (Eq a) ⇒ Eq (𝐴 t a) where (==) = (≡) `on` aval
instance (Ord a) ⇒ Ord (𝐴 t a) where compare = compare `on` aval
instance Extract (𝐴 t) where extract = aval
instance Cobind (𝐴 t) where 𝐴 e x =≫ f = 𝐴 e $ f $ 𝐴 e x
instance Functor (𝐴 t) where map = wmap
instance FunctorM (𝐴 t) where mapM f (𝐴 e x) = 𝐴 e ^$ f x
instance Comonad (𝐴 t)

instance (Null e,Null a) ⇒ Null (𝐴 e a) where null = 𝐴 null null
instance (Append e,Append a) ⇒ Append (𝐴 e a) where 𝐴 e₁ x₁ ⧺ 𝐴 e₂ x₂ = 𝐴 (e₁ ⧺ e₂) $ x₁ ⧺ x₂

map𝐴 ∷ (e → e') → (a → b) → 𝐴 e a → 𝐴 e' b
map𝐴 f g (𝐴 e x) = 𝐴 (f e) $ g x

mapATag ∷ (e → e') → 𝐴 e a → 𝐴 e' a
mapATag f = map𝐴 f id

mapAVal ∷ (a → b) → 𝐴 e a → 𝐴 e b
mapAVal f = map𝐴 id f

mapM𝐴 ∷ (Monad m) ⇒ (e → m e') → (a → m b) → 𝐴 e a → m (𝐴 e' b)
mapM𝐴 f g (𝐴 e x) = do
  e' ← f e
  y ← g x
  return $ 𝐴 e' y

mapMATag ∷ (Monad m) ⇒ (e → m e') → 𝐴 e a → m (𝐴 e' a)
mapMATag f = mapM𝐴 f return

mapMAVal ∷ (Monad m) ⇒ (a → m b) → 𝐴 e a → m (𝐴 e b)
mapMAVal f = mapM𝐴 return f

untag ∷ ((() → e) → b → b) → 𝐴 e a → (a → b) → b
untag cxt (𝐴 𝒸 x) f = cxt (const 𝒸) $ f x

untagWith ∷ ((() → e) → b → b) → (a → b) → 𝐴 e a → b
untagWith = flip ∘ untag

retag ∷ (Monad m) ⇒ m e → a → m (𝐴 e a)
retag eM x = do
  e ← eM
  return $ 𝐴 e x
