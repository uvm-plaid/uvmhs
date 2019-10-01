module UVMHS.Lib.Annotated where

import UVMHS.Core

import UVMHS.Lib.Pretty

data Annotated e a = Annotated
  { annotatedTag ∷ e
  , annotatedElem ∷ a
  } deriving (Show)
makeLenses ''Annotated
makePrettySum ''Annotated

instance (Eq a) ⇒ Eq (Annotated t a) where (==) = (≡) `on` annotatedElem
instance (Ord a) ⇒ Ord (Annotated t a) where compare = compare `on` annotatedElem
instance Extract (Annotated t) where extract = annotatedElem
instance Cobind (Annotated t) where Annotated e x =≫ f = Annotated e $ f $ Annotated e x
instance Functor (Annotated t) where map = wmap
instance Comonad (Annotated t)

