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
