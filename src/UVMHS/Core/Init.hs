module UVMHS.Core.Init
  ( module UVMHS.Core.Init
  , module Data.Coerce
  , module GHC.Exts
  , module GHC.Stack
  , module Prelude
  , module Data.String
  ) where

import Prelude
  ( undefined,otherwise
  , Bool(..),Eq((==)),Ord(compare),Show(show),Ordering(..),IO
  , fromInteger
  )
import Data.Coerce
  ( coerce
  , Coercible
  )
import GHC.Exts
  ( type Constraint
  )
import GHC.Stack
  ( type CallStack,callStack,withFrozenCallStack
  )

import qualified Control.Exception as HS

import Data.String
  ( fromString
  )

import qualified Prelude as HS
import qualified GHC.Generics as HS
import qualified GHC.Types as HS
import qualified GHC.Stack as HS

import qualified Data.Int as HS
import qualified Data.Ratio as HS
import qualified Data.Word as HS
import qualified Numeric.Natural as HS
import qualified Unsafe.Coerce as HS

import qualified Data.Text as Text

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence

import qualified Language.Haskell.TH.Syntax as TH

infixr 0 $

infixr 1 ⇰
infixr 1 ⇛
infixl 2 ⩔
infixl 3 ⩓
infixl 5 ∨
infixl 6 ∧
infixl 7 ∘
infixl 8 :*
infixr 8 :&

------------------------------------
-- Numeric and Boolean Base Types --
------------------------------------

type ℕ   = HS.Natural
type ℕ64 = HS.Word64
type ℕ32 = HS.Word32
type ℕ16 = HS.Word16
type ℕ8  = HS.Word8
type ℤ   = HS.Integer
type ℤ64 = HS.Int64
type ℤ32 = HS.Int32
type ℤ16 = HS.Int16
type ℤ8  = HS.Int8
type ℚ   = HS.Rational
type ℚᴾ  = HS.Ratio ℕ
type 𝔻   = HS.Double

-- non-negative double
newtype 𝔻ᴾ = 𝔻ᴾ { un𝔻ᴾ ∷ 𝔻 }
  deriving (Eq,Ord,Show,HS.Num,HS.Fractional,HS.Floating,HS.Real)

-- union of integer, rational and double
data ℝ = Integer ℤ | Rational ℚ | Double 𝔻
  deriving (Eq,Ord,Show)

-- non-negative variant of ^^
data ℝᴾ = Natural ℕ | Rationalᴾ ℚᴾ | Doubleᴾ 𝔻ᴾ
  deriving (Eq,Ord,Show)

-- bools
type 𝔹 = HS.Bool

not ∷ 𝔹 → 𝔹
not True = False
not False = True

(⩔) ∷ 𝔹 → 𝔹 → 𝔹
b₁ ⩔ ~b₂ = if b₁ then True else b₂

(⩓) ∷ 𝔹 → 𝔹 → 𝔹
b₁ ⩓ ~b₂ = if b₁ then b₂ else False

(⇛) ∷ 𝔹 → 𝔹 → 𝔹
(⇛) b₁ b₂ = not b₁ ⩔ b₂

cond ∷ 𝔹 → a → a → a
cond b ~x ~y = case b of { True → x ; False → y }

-- sometimes convenient to have the arguments in this order
elim𝔹 ∷ a → a → 𝔹 → a
elim𝔹 ~x ~y b = cond b x y

---------------------------
-- Char and String Types --
---------------------------

type ℂ = HS.Char
type 𝕊 = Text.Text

-----------------------
-- "Algebraic" Types --
-----------------------

data Void

exfalso ∷ Void → a
exfalso = \case

data a ∨ b = Inl a | Inr b
  deriving (Eq,Ord,Show)
data a ∧ b = a :* b
  deriving (Eq,Ord,Show)

----------------------
-- Collection Types --
----------------------

data 𝑂 a = None | Some a
  deriving (Eq,HS.Generic,Ord,Show)
data 𝐿 a = Nil | a :& 𝐿 a
  deriving (Eq,Ord,TH.Lift)

-- iterator type
--                           fold function               continuation
--                           ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓       ↓↓↓↓↓↓↓
newtype 𝐼 a = 𝐼 { un𝐼 ∷ ∀ b. (a → b → (b → b) → b) → b → (b → b) → b }
--                                ↑   ↑↑↑↑↑↑↑        ↑
--                      accumulator   continuation   accumulator


run𝐼 ∷ (b → b) → b → (a → b → (b → b) → b) → 𝐼 a → b
run𝐼 𝓀 i f xs = un𝐼 xs f i 𝓀

run𝐼On ∷ 𝐼 a → (b → b) → b → (a → b → (b → b) → b) → b
run𝐼On xs 𝓀 i f = un𝐼 xs f i 𝓀

foldk𝐼 ∷ b → (a → b → (b → b) → b) → 𝐼 a → b
foldk𝐼 = run𝐼 id

-- accumulate values in natural order
-- (i.e., from "left to right")
-- constant space overhead
fold𝐼 ∷ b → (a → b → b) → 𝐼 a → b
fold𝐼 i₀ f = run𝐼 id i₀ $ \ x i 𝓀 → 𝓀 $ f x i

-- accumulate values in reverse order
-- (i.e., from "right to left")
-- linear space overhead
foldr𝐼 ∷ b → (a → b → b) → 𝐼 a → b
foldr𝐼 i₀ f = run𝐼 id i₀ $ \ x i 𝓀 → f x $ 𝓀 i

map𝐼 ∷ (a → b) → 𝐼 a → 𝐼 b
map𝐼 f xs = 𝐼 HS.$ \ g → un𝐼 xs $ g ∘ f

null𝐼 ∷ 𝐼 a
null𝐼 = 𝐼 HS.$ const $ \ i 𝓀 → 𝓀 i

single𝐼 ∷ a → 𝐼 a
single𝐼 x = 𝐼 HS.$ \ f i 𝓀 → f x i 𝓀

list𝐼 ∷ 𝐼 a → 𝐿 a
list𝐼 = foldr𝐼 Nil (:&)

iter𝐿 ∷ 𝐿 a → 𝐼 a
iter𝐿 xs₀ = 𝐼 HS.$ \ f → flip $ \ 𝓀 →
  let loop xs i = case xs of
        Nil → 𝓀 i
        x :& xs' →
          f x i $ \ i' →
          loop xs' i'
  in loop xs₀

lazyList𝐼 ∷ 𝐼 a → [a]
lazyList𝐼 = foldr𝐼 [] (:)

iterLL ∷ [a] → 𝐼 a
iterLL xs₀ = 𝐼 HS.$ \ f → flip $ \ 𝓀 →
  let loop xs i = case xs of
        [] → 𝓀 i
        x:xs' →
          f x i $ \ i' →
          loop xs' i'
  in loop xs₀

-- stream
newtype 𝑆 a = 𝑆 { un𝑆 ∷ () → 𝑂 (a ∧ 𝑆 a) }
-- sequence (finger trees)
newtype 𝑄 a = 𝑄 { un𝑄 ∷ Sequence.Seq a }
  deriving (Eq,Ord)
-- set (BB-ω trees)
newtype 𝑃 a = 𝑃 { un𝑃 ∷ Set.Set a }
  deriving (Eq,Ord)
-- dictionary (BB-ω trees)
newtype k ⇰ v = 𝐷 { un𝐷 ∷ Map.Map k v }
  deriving (Eq,Ord)

------------------------
-- Other Useful Types --
------------------------

data Lazy a = Lazy { unLazy ∷ ~a }

data Nat = Z | S Nat
  deriving (Eq,Ord,Show)

data (≟) (a ∷ k) (b ∷ k) ∷ ★ where
  Refl ∷ ∀ (k ∷ ★) (a ∷ k). a ≟ a

data P (a ∷ k) = P
  deriving (Eq,Ord,Show)

-- Wrap a constraint in a value
data W (c ∷ Constraint) where W ∷ (c) ⇒ W c

coerce_UNSAFE ∷ a → b
coerce_UNSAFE = HS.unsafeCoerce

weq_UNSAFE ∷ P a → P b → W (a ~ b)
weq_UNSAFE P P = coerce_UNSAFE $ W @(() ~ ())

void_UNSAFE ∷ Void
void_UNSAFE = coerce_UNSAFE ()

deriving instance Eq (W c)
deriving instance Ord (W c)
deriving instance Show (W c)

with ∷ W c → ((c) ⇒ a) → a
with W x = x

-- Existentially quantified type
data Ex (t ∷ k → ★) ∷ ★ where
  Ex ∷ ∀ (k ∷ ★) (t ∷ k → ★) (a ∷ k). t a → Ex t

deriving instance (∀ a. Show (t a)) ⇒ Show (Ex t)

unpack ∷ ∀ (k ∷ ★) (t ∷ k → ★) (b ∷ ★). Ex t → (∀ (a ∷ k). t a → b) → b
unpack (Ex x) f = f x

-- Constrained existentially quantified type with
data Ex_C (c ∷ k → Constraint) (t ∷ k → ★) ∷ ★ where
  Ex_C ∷ ∀ (k ∷ ★) (c ∷ k → Constraint) (t ∷ k → ★) (a ∷ k). (c a) ⇒ t a → Ex_C c t

deriving instance (∀ a. c a ⇒ Show (t a)) ⇒ Show (Ex_C c t)

unpack_C ∷ ∀ (k ∷ ★) (c ∷ k → Constraint) (t ∷ k → ★) (b ∷ ★). Ex_C c t → (∀ (a ∷ k). (c a) ⇒ t a → b) → b
unpack_C (Ex_C x) f = f x

--------------------
-- Haskell Syntax --
--------------------

-- fromString ∷ [ℂ] → 𝕊
-- fromString = Text.pack

-- fromInteger ∷ ℤ → ℕ
-- fromInteger = HS.fromIntegral

negate ∷ ℕ → ℤ
negate n = HS.negate (HS.fromIntegral n)

fromRational ∷ HS.Rational → 𝔻
fromRational = HS.fromRational

fail ∷ ∀ (r ∷ HS.RuntimeRep) (a ∷ HS.TYPE r) m. (STACK) ⇒ [ℂ] → m a
fail = HS.error

fail𝕊 ∷ ∀ (r ∷ HS.RuntimeRep) (a ∷ HS.TYPE r) m. (STACK) ⇒ 𝕊 → m a
fail𝕊 = fail ∘ tohsChars

ifThenElse ∷ 𝔹 → a → a → a
ifThenElse = cond

-----------------------
-- Basic Conversions --
-----------------------

𝕤 ∷ [ℂ] → 𝕊
𝕤 = Text.pack

𝕟 ∷ ℕ → ℕ
𝕟 = id

𝕟64 ∷ ℕ → ℕ64
𝕟64 = HS.fromIntegral

𝕟32 ∷ ℕ → ℕ32
𝕟32 = HS.fromIntegral

𝕟16 ∷ ℕ → ℕ16
𝕟16 = HS.fromIntegral

𝕟8 ∷ ℕ → ℕ8
𝕟8 = HS.fromIntegral

𝕫 ∷ ℕ → ℤ
𝕫 = HS.fromIntegral

𝕫64 ∷ ℕ → ℤ64
𝕫64 = HS.fromIntegral

𝕫32 ∷ ℕ → ℤ32
𝕫32 = HS.fromIntegral

𝕫16 ∷ ℕ → ℤ16
𝕫16 = HS.fromIntegral

𝕫8 ∷ ℕ → ℤ8
𝕫8 = HS.fromIntegral

𝕢 ∷ ℕ → ℚ
𝕢 = HS.fromIntegral

𝕢ᴾ ∷ ℕ → ℚᴾ
𝕢ᴾ = HS.fromIntegral

𝕕ᴾ ∷ 𝔻 → 𝔻ᴾ
𝕕ᴾ = 𝔻ᴾ

tohsChars ∷ 𝕊 → [ℂ]
tohsChars = Text.unpack

frhsChars ∷ [ℂ] → 𝕊
frhsChars = Text.pack

---------------------
-- Call Stack Type --
---------------------

type STACK = HS.HasCallStack

error ∷ ∀ (r ∷ HS.RuntimeRep) (a ∷ HS.TYPE r). (STACK) ⇒ 𝕊 → a
error s = HS.error (tohsChars s)

assert ∷ (() → 𝔹) → ()
#ifdef __GLASGOW_HASKELL_ASSERTS_IGNORED__
assert = \ _ → ()
#else
assert = \ b → HS.assert (b ()) ()
#endif

------------------------------
-- Basic Function Functions --
------------------------------

($) ∷ ∀ r a (b ∷ HS.TYPE r). (a → b) → a → b
($) = \ f x → f x

id ∷ a → a
id = \ x → x

appto ∷ a → (a → b) → b
appto = \ x f → f x

const ∷ a → b → a
const = \ x _ → x

(∘) ∷ (b → c) → (a → b) → a → c
(∘) = \ g f x → g (f x)

(∘∘) ∷ (c → d) → (a → b → c) → (a → b → d)
(∘∘) = (∘) ∘ (∘)

(∘∘∘) ∷ (d → e) → (a → b → c → d) → a → b → c → e
(∘∘∘) = (∘∘) ∘ (∘)

flip ∷ (a → b → c) → (b → a → c)
flip f = \ y x → f x y

rotateR ∷ (a → b → c → d) → (c → a → b → d)
rotateR f = \ c a b → f a b c

rotateL ∷ (a → b → c → d) → (b → c → a → d)
rotateL f = \ b c a → f a b c

mirror ∷ (a → b → c → d) → (c → b → a → d)
mirror f = \ c b a → f a b c

on ∷ (b → b → c) → (a → b) → (a → a → c)
on p f = \ x y → p (f x) (f y)

uncurry ∷ (a → b → c) → a ∧ b → c
uncurry f = \ (x :* y) → f x y

curry ∷ (a ∧ b → c) → a → b → c
curry f = \ x y → f (x :* y)

----------------------
-- TEMPLATE HASKELL --
---------------------

type QIO = TH.Q

-----------------------------------
-- Conversion to Vanilla Haskell --
-----------------------------------

tohs𝑂F ∷ (a → b) → 𝑂 a → HS.Maybe b
tohs𝑂F f = \case
  None → HS.Nothing
  Some x → HS.Just $ f x

tohs𝑂 ∷ 𝑂 a → HS.Maybe a
tohs𝑂 = tohs𝑂F id

frhs𝑂F ∷ (a → b) → HS.Maybe a → 𝑂 b
frhs𝑂F f = \case
  HS.Nothing → None
  HS.Just x → Some $ f x

frhs𝑂 ∷ HS.Maybe a → 𝑂 a
frhs𝑂 = frhs𝑂F id

class CHS a b | b → a where
  tohs ∷ a → b
  frhs ∷ b → a

instance {-# OVERLAPPABLE #-} (a ~ b) ⇒ CHS a b where
  tohs = id
  frhs = id
instance {-# OVERLAPPING #-} CHS ℤ64 HS.Int where
  tohs = HS.fromIntegral
  frhs = HS.fromIntegral
instance {-# OVERLAPPING #-} (CHS a b) ⇒ CHS (𝐿 a) [b] where
  tohs = lazyList𝐼 ∘ map𝐼 tohs ∘ iter𝐿
  frhs = list𝐼 ∘ map𝐼 frhs ∘ iterLL
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂) ⇒ CHS (a₁ ∧ a₂) (b₁,b₂) where
  tohs (x :* y) = (tohs x,tohs y)
  frhs (x,y) = frhs x :* frhs y
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂,CHS a₃ b₃) ⇒ CHS (a₁ ∧ a₂ ∧ a₃) (b₁,b₂,b₃) where
  tohs (x :* y :* z) = (tohs x,tohs y,tohs z)
  frhs (x,y,z) = frhs x :* frhs y :* frhs z
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂,CHS a₃ b₃,CHS a₄ b₄) ⇒ CHS (a₁ ∧ a₂ ∧ a₃ ∧ a₄) (b₁,b₂,b₃,b₄) where
  tohs (w :* x :* y :* z) = (tohs w,tohs x,tohs y,tohs z)
  frhs (w,x,y,z) = frhs w :* frhs x :* frhs y :* frhs z
instance {-# OVERLAPPING #-} (CHS a₁ b₁,CHS a₂ b₂) ⇒ CHS (a₁ ∨ a₂) (HS.Either b₁ b₂) where
  tohs = \case
    Inl x → HS.Left $ tohs x
    Inr y → HS.Right $ tohs y
  frhs = \case
    HS.Left x → Inl $ frhs x
    HS.Right y → Inr $ frhs y
instance {-# OVERLAPPING #-} (CHS a b) ⇒ CHS (𝑂 a) (HS.Maybe b) where
  tohs = tohs𝑂F tohs
  frhs = frhs𝑂F frhs
