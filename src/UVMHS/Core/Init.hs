module UVMHS.Core.Init
  (module UVMHS.Core.Init
  ,module GHC.Exts
  ,module Prelude
  ) where

import Prelude(Bool(..),($),undefined,otherwise,IO,Eq((==)),Ord(compare),Show(show),Ordering(..))
import GHC.Exts (type Constraint)

import qualified Prelude as HS

import qualified Data.Int as HS
import qualified Data.Word as HS
import qualified Data.Ratio as HS
import qualified Numeric.Natural as HS
import qualified Control.Exception as HS

import qualified Data.Text as Text

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence

-- 0[R]: $
-- 1[L]: :*
-- 2[R]: ≫= →
-- 3[I]: ≡
-- 4[L]: +
-- 5[L]: ×
-- 6[L]: ∘
-- 7[L]: #,^
-- 8[*]: :&
-- 9[L]: ⋅

infixl 1 :*
infixr 2 ⇰
infixl 4 ∨,⩔
infixl 5 ∧,⩓
infixl 6 ∘
infixr 8 :&

type ℕ = HS.Natural
type ℕ64 = HS.Word64
type ℕ32 = HS.Word32
type ℕ16 = HS.Word16
type ℕ8  = HS.Word8
type ℤ = HS.Integer
type ℤ64 = HS.Int64
type ℤ32 = HS.Int32
type ℤ16 = HS.Int16
type ℤ8  = HS.Int8
type ℚ = HS.Rational
type ℚᴾ = HS.Ratio ℕ
type 𝔻 = HS.Double
-- non-negative double
newtype 𝔻ᴾ = 𝔻ᴾ { un𝔻ᴾ ∷ 𝔻 }
  deriving (Eq,Ord,Show,HS.Num,HS.Fractional,HS.Floating,HS.Real)

data ℝ = Integer ℤ | Rational ℚ | Double 𝔻
  deriving (Eq,Ord,Show)
data ℝᴾ = Natural ℕ | Rationalᴾ ℚᴾ | Doubleᴾ 𝔻ᴾ
  deriving (Eq,Ord,Show)

type ℂ = HS.Char
type 𝕊 = Text.Text

data Void
type 𝔹 = HS.Bool
data a ∨ b = Inl a | Inr b
  deriving (Eq,Ord,Show)
data a ∧ b = a :* b
  deriving (Eq,Ord,Show)
data 𝑂 a = None | Some a
  deriving (Eq,Ord,Show)
data 𝐿 a = Nil | a :& 𝐿 a
  deriving (Eq,Ord)
newtype 𝐼 a = 𝐼 { un𝐼 ∷ ∀ b. (a → b → b) → b → b }
data 𝑆 a where 𝑆 ∷ ∀ s a. s → (s → 𝑂 (a ∧ s)) → 𝑆 a
newtype 𝑄 a = 𝑄 { un𝑄 ∷ Sequence.Seq a }
  deriving (Eq,Ord)
newtype 𝑃 a = 𝑃 { un𝑃 ∷ Set.Set a }
  deriving (Eq,Ord)
newtype k ⇰ v = 𝐷 { un𝐷 ∷ Map.Map k v }
  deriving (Eq,Ord)

data Lazy a = Lazy { unLazy ∷ ~a }

data (≟) (a ∷ k) (b ∷ k) ∷ ★ where
  Refl ∷ ∀ (a ∷ k). a ≟ a

data P (a ∷ k) = P
  deriving (Eq,Ord,Show)

data Nat = Z | S Nat
  deriving (Eq,Ord,Show)

data W (c ∷ Constraint) where W ∷ (c) ⇒ W c

deriving instance Eq (W c)
deriving instance Ord (W c)
deriving instance Show (W c)

{-# INLINE with #-}
with ∷ W c → ((c) ⇒ a) → a
with W x = x

data Ex (t ∷ k → ★) ∷ ★ where
  Ex ∷ ∀ (t ∷ k → ★) (a ∷ k). t a → Ex t

deriving instance (∀ a. Show (t a)) ⇒ Show (Ex t)

{-# INLINE unpack #-}
unpack ∷ ∀ (t ∷ k → ★) (b ∷ ★). Ex t → (∀ (a ∷ k). t a → b) → b
unpack (Ex x) f = f x

data Ex_C (c ∷ k → Constraint) (t ∷ k → ★) ∷ ★ where
  Ex_C ∷ ∀ (c ∷ k → Constraint) (t ∷ k → ★) (a ∷ k). (c a) ⇒ t a → Ex_C c t

deriving instance (∀ a. c a ⇒ Show (t a)) ⇒ Show (Ex_C c t)

{-# INLINE unpack_C #-}
unpack_C ∷ ∀ (k ∷ ★) (c ∷ k → Constraint) (t ∷ k → ★) (b ∷ ★). Ex_C c t → (∀ (a ∷ k). (c a) ⇒ t a → b) → b
unpack_C (Ex_C x) f = f x

{-# INLINE zabs #-}
zabs ∷ ℤ → ℕ
zabs = HS.fromIntegral ∘ HS.abs

{-# INLINE qabs #-}
qabs ∷ ℚ → ℚᴾ
qabs = HS.fromRational ∘ HS.abs

{-# INLINE numer #-}
numer ∷ ℚ → ℤ
numer = HS.numerator

{-# INLINE denom #-}
denom ∷ ℚ → ℕ
denom = HS.fromIntegral ∘ HS.denominator

{-# INLINE numerᴾ #-}
numerᴾ ∷ ℚᴾ → ℕ
numerᴾ = HS.numerator

{-# INLINE denomᴾ #-}
denomᴾ ∷ ℚᴾ → ℕ
denomᴾ = HS.denominator

{-# INLINE truncate #-}
truncate ∷ 𝔻 → ℤ
truncate = HS.truncate

{-# INLINE ceiling #-}
ceiling ∷ 𝔻 → ℤ
ceiling = HS.ceiling

{-# INLINE floor #-}
floor ∷ 𝔻 → ℤ
floor = HS.floor

{-# INLINE truncateᴾ #-}
truncateᴾ ∷ 𝔻ᴾ → ℕ
truncateᴾ = HS.truncate ∘ un𝔻ᴾ

{-# INLINE ceilingᴾ #-}
ceilingᴾ ∷ 𝔻ᴾ → ℕ
ceilingᴾ = HS.ceiling ∘ un𝔻ᴾ

{-# INLINE floorᴾ #-}
floorᴾ ∷ 𝔻ᴾ → ℕ
floorᴾ = HS.floor ∘ un𝔻ᴾ

-- Syntax --

{-# INLINE fromString #-}
fromString ∷ [ℂ] → 𝕊
fromString = Text.pack

{-# INLINE fromInteger #-}
fromInteger ∷ ℤ → ℕ
fromInteger = HS.fromIntegral

{-# INLINE negate #-}
negate ∷ ℕ → ℤ
negate n = HS.negate (HS.fromIntegral n)

{-# INLINE fromRational #-}
fromRational ∷ HS.Rational → 𝔻
fromRational = HS.fromRational

{-# INLINE fail #-}
fail ∷ [ℂ] → m a
fail = HS.error

{-# INLINE ifThenElse #-}
ifThenElse ∷ 𝔹 → a → a → a
ifThenElse b ~x ~y = case b of { True → let x' = x in x' ; False → let y' = y in y' }

-- Conversion --

{-# INLINE 𝕟64 #-}
𝕟64 ∷ ℕ → ℕ64
𝕟64 = HS.fromIntegral

{-# INLINE 𝕟32 #-}
𝕟32 ∷ ℕ → ℕ32
𝕟32 = HS.fromIntegral

{-# INLINE 𝕟16 #-}
𝕟16 ∷ ℕ → ℕ16
𝕟16 = HS.fromIntegral

{-# INLINE 𝕟8 #-}
𝕟8 ∷ ℕ → ℕ8
𝕟8 = HS.fromIntegral

{-# INLINE 𝕫 #-}
𝕫 ∷ ℕ → ℤ
𝕫 = HS.fromIntegral

{-# INLINE 𝕫64 #-}
𝕫64 ∷ ℕ → ℤ64
𝕫64 = HS.fromIntegral

{-# INLINE 𝕫32 #-}
𝕫32 ∷ ℕ → ℤ32
𝕫32 = HS.fromIntegral

{-# INLINE 𝕫16 #-}
𝕫16 ∷ ℕ → ℤ16
𝕫16 = HS.fromIntegral

{-# INLINE 𝕫8 #-}
𝕫8 ∷ ℕ → ℤ8
𝕫8 = HS.fromIntegral

{-# INLINE chars #-}
chars ∷ 𝕊 → [ℂ]
chars = Text.unpack

{-# INLINE fromChars #-}
fromChars ∷ [ℂ] → 𝕊
fromChars = Text.pack

{-# INLINE error #-}
error ∷ 𝕊 → a
error = HS.error ∘ chars

{-# INLINE assert #-}
assert ∷ 𝔹 → a → a
assert = HS.assert

-- Functions --

{-# INLINE id #-}
id ∷ a → a
id = \ x → x

{-# INLINE const #-}
const ∷ a → b → a
const x = \ _ → x

{-# INLINE (∘) #-}
(∘) ∷ (b → c) → (a → b) → a → c
g ∘ f = \ x → g (f x)

{-# INLINE (∘∘) #-}
(∘∘) ∷ (c → d) → (a → b → c) → (a → b → d)
(∘∘) = (∘) ∘ (∘)

{-# INLINE flip #-}
flip ∷ (a → b → c) → (b → a → c)
flip f = \ y x → f x y

{-# INLINE rotateR #-}
rotateR ∷ (a → b → c → d) → (c → a → b → d)
rotateR f = \ c a b → f a b c

{-# INLINE rotateL #-}
rotateL ∷ (a → b → c → d) → (b → c → a → d)
rotateL f = \ b c a → f a b c

{-# INLINE mirror #-}
mirror ∷ (a → b → c → d) → (c → b → a → d)
mirror f = \ c b a → f a b c

{-# INLINE on #-}
on ∷ (b → b → c) → (a → b) → (a → a → c)
on p f = \ x y → p (f x) (f y)

{-# INLINE curry #-}
curry ∷ (a → b → c) → a ∧ b → c
curry f (x :* y) = f x y

{-# INLINE uncurry #-}
uncurry ∷ (a ∧ b → c) → a → b → c
uncurry f x y = f (x :* y)

-- Bools --

not ∷ 𝔹 → 𝔹
not True = False
not False = True

(⩓) ∷ 𝔹 → 𝔹 → 𝔹
True ⩓ x = x
x ⩓ True = x
False ⩓ False = False

(⩔) ∷ 𝔹 → 𝔹 → 𝔹
False ⩔ x = x
x ⩔ False = x
True ⩔ True = True

{-# INLINE cond #-}
cond ∷ 𝔹 → a → a → a
cond = \case { True → \ x _ → x ; False → \ _ y → y }

-- Lists --

{-# INLINE stream𝐿 #-}
stream𝐿 ∷ 𝐿 a → 𝑆 a
stream𝐿 xs₀ = 𝑆 xs₀ g
  where
    g Nil = None
    g (x :& xs) = Some (x :* xs)

{-# INLINE list𝐼 #-}
list𝐼 ∷ 𝐼 a → 𝐿 a
list𝐼 = foldr𝐼 Nil (:&)

-- LazyLists --

{-# INLINE streamLL #-}
streamLL ∷ [a] → 𝑆 a
streamLL xs₀ = 𝑆 xs₀ g
  where
    g [] = None
    g (x:xs) = Some (x :* xs)

{-# INLINE lazyList𝐼 #-}
lazyList𝐼 ∷ 𝐼 a → [a]
lazyList𝐼 = foldr𝐼 [] (:)

-- Iterators --

{-# INLINE fold𝐼 #-}
fold𝐼 ∷ b → (a → b → b) → 𝐼 a → b
fold𝐼 i f (𝐼 g) = g f i

{-# INLINE foldk𝐼 #-}
foldk𝐼 ∷ b → (a → (b → b) → b → b) → 𝐼 a → b
foldk𝐼 i f (𝐼 g) = g f id i

{-# INLINE foldr𝐼 #-}
foldr𝐼 ∷ b → (a → b → b) → 𝐼 a → b
foldr𝐼 i f = foldk𝐼 i $ \ x k → k ∘ f x

{-# INLINE map𝐼 #-}
map𝐼 ∷ (a → b) → 𝐼 a → 𝐼 b
map𝐼 f (𝐼 g) = 𝐼 $ \ f' → g $ f' ∘ f

-- Streams --

{-# INLINE iter𝑆 #-}
iter𝑆 ∷ 𝑆 a → 𝐼 a
iter𝑆 (𝑆 s₀ g) = 𝐼 $ \ f i₀ →
  let loop s i = case g s of
        None → i
        Some (x :* s') → loop s' $ f x i
  in loop s₀ i₀

-- Compat --

class CHS a b | b → a where
  tohs ∷ a → b
  frhs ∷ b → a

instance {-# OVERLAPPABLE #-} (a ~ b) ⇒ CHS a b where 
  {-# INLINE tohs #-}
  tohs = id
  {-# INLINE frhs #-}
  frhs = id
instance {-# OVERLAPPING #-} CHS ℤ32 HS.Int where
  {-# INLINE tohs #-}
  tohs = HS.fromIntegral
  {-# INLINE frhs #-}
  frhs = HS.fromIntegral
instance {-# OVERLAPPING #-} (CHS a b) ⇒ CHS (𝐿 a) [b] where
  {-# INLINE tohs #-}
  tohs = lazyList𝐼 ∘ map𝐼 tohs ∘ iter𝑆 ∘ stream𝐿
  {-# INLINE frhs #-}
  frhs = list𝐼 ∘ map𝐼 frhs ∘ iter𝑆 ∘ streamLL
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂,CHS a₃ b₃) ⇒ CHS (a₁ ∧ a₂ ∧ a₃) (b₁,b₂,b₃) where
  {-# INLINE tohs #-}
  tohs (x :* y :* z) = (tohs x,tohs y,tohs z)
  {-# INLINE frhs #-}
  frhs (x,y,z) = frhs x :* frhs y :* frhs z
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂) ⇒ CHS (a₁ ∧ a₂) (b₁,b₂) where
  {-# INLINE tohs #-}
  tohs (x :* y) = (tohs x,tohs y)
  {-# INLINE frhs #-}
  frhs (x,y) = frhs x :* frhs y
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂) ⇒ CHS (a₁ ∨ a₂) (HS.Either b₁ b₂) where
  {-# INLINE tohs #-}
  tohs = \case
    Inl x → HS.Left $ tohs x
    Inr y → HS.Right $ tohs y
  {-# INLINE frhs #-}
  frhs = \case
    HS.Left x → Inl $ frhs x
    HS.Right y → Inr $ frhs y
instance {-# OVERLAPPING #-} (CHS a b) ⇒ CHS (𝑂 a) (HS.Maybe b) where
  {-# INLINE tohs #-}
  tohs = \case
    None → HS.Nothing
    Some x → HS.Just $ tohs x
  {-# INLINE frhs #-}
  frhs = \case
    HS.Nothing → None
    HS.Just x → Some $ frhs x
