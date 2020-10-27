module UVMHS.Core.Classes.Morphism where

import UVMHS.Core.Init

infixr 1 →⁻,→⁼,⇄,⇄⁻,⇄⁼
infixl 7 ⊚

type (m ∷ ★ → ★) →⁻ (n ∷ ★ → ★) = ∀ a. m a → n a
type (t ∷ (★ → ★) → ★ → ★) →⁼ (u ∷ (★ → ★) → ★ → ★) = ∀ m. t m →⁻ u m

class a ⇄ b | a → b where
  isoto ∷ a → b
  isofr ∷ b → a
data Iso a b = Iso 
  { ito ∷ a → b
  , ifr ∷ b → a
  }

{-# INLINE toiso #-}
toiso ∷ (a ⇄ b) ⇒ Iso a b 
toiso = Iso isoto isofr

{-# INLINE friso #-}
friso ∷ (a ⇄ b) ⇒ Iso b a
friso = Iso isofr isoto

class t ⇄⁻ u | t → u where
  isoto2 ∷ t →⁻ u
  isofr2 ∷ u →⁻ t
data Iso2 t u = Iso2 
  { ito2 ∷ t →⁻ u
  , ifr2 ∷ u →⁻ t
  }

{-# INLINE toiso2 #-}
toiso2 ∷ (t ⇄⁻ u) ⇒ Iso2 t u
toiso2 = Iso2 isoto2 isofr2

{-# INLINE friso2 #-}
friso2 ∷ (t ⇄⁻ u) ⇒ Iso2 u t
friso2 = Iso2 isofr2 isoto2

class v ⇄⁼ w | v → w where
  isoto3 ∷ v →⁼ w
  isofr3 ∷ w →⁼ v
data Iso3 v w = Iso3
  { ito3 ∷ v →⁼ w
  , ifr3 ∷ w →⁼ v
  }

{-# INLINE toiso3 #-}
toiso3 ∷ (v ⇄⁼ w) ⇒ Iso3 v w
toiso3 = Iso3 isoto3 isofr3

{-# INLINE friso3 #-}
friso3 ∷ (v ⇄⁼ w) ⇒ Iso3 w v
friso3 = Iso3 isofr3 isoto3

class Category t where {refl ∷ t a a;(⊚) ∷ t b c → t a b → t a c}
class Symmetric t where {sym ∷ t a b → t b a}

instance Category (→) where 
  {-# INLINE refl #-}
  refl = id
  {-# INLINE (⊚) #-}
  (⊚) = (∘)

instance Category Iso where 
  {-# INLINE refl #-}
  refl = Iso id id
  {-# INLINE (⊚) #-}
  Iso gto gfrom ⊚ Iso fto ffrom = Iso (gto ∘ fto) (ffrom ∘ gfrom)
instance Category Iso2 where 
  {-# INLINE refl #-}
  refl = Iso2 id id
  {-# INLINE (⊚) #-}
  Iso2 gto gfrom ⊚ Iso2 fto ffrom = Iso2 (gto ∘ fto) (ffrom ∘ gfrom)
instance Category Iso3 where 
  {-# INLINE refl #-}
  refl = Iso3 id id
  {-# INLINE (⊚) #-}
  Iso3 gto gfrom ⊚ Iso3 fto ffrom = Iso3 (gto ∘ fto) (ffrom ∘ gfrom)

instance Symmetric Iso where 
  {-# INLINE sym #-}
  sym (Iso to from) = Iso from to
instance Symmetric Iso2 where 
  {-# INLINE sym #-}
  sym (Iso2 to from) = Iso2 from to
instance Symmetric Iso3 where 
  {-# INLINE sym #-}
  sym (Iso3 to from) = Iso3 from to
