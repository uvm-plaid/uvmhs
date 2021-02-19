module UVMHS.Core.Data.String where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()

import qualified Data.Text                 as Text
import qualified Data.Text.Internal.Fusion as TextI
import qualified Data.Text.Lazy            as TextLazy
import qualified Data.Text.Lazy.Builder    as TextBuilder
import qualified Prelude                   as HS

instance Null 𝕊 where 
  {-# INLINE null #-}
  null = Text.empty
instance Append 𝕊 where 
  {-# INLINE (⧺) #-}
  (⧺) = Text.append
instance Monoid 𝕊

instance Single ℂ 𝕊 where 
  {-# INLINE single #-}
  single = Text.singleton

instance ToStream ℂ 𝕊 where 
  {-# INLINE stream #-}
  stream cs = 
    case TextI.stream cs of
      TextI.Stream f s₀ _ →
        let loop s = case f s of
              TextI.Done → None
              TextI.Skip s' → loop s'
              TextI.Yield x s' → Some (x :* s')
        in 𝑆 s₀ loop
instance ToIter ℂ 𝕊 where 
  {-# INLINE iter #-}
  iter = iter𝑆 ∘ stream

instance Lookup ℕ ℂ 𝕊 where 
  s ⋕? n 
    | (n > 0) ⩓ (n ≤ length𝕊 s) = Some $ Text.index s $ HS.fromIntegral $ n - 1
    | otherwise = None

instance Sized 𝕊 where size = length64𝕊

{-# INLINE empty𝕊 #-}
empty𝕊 ∷ 𝕊 → 𝔹
empty𝕊 = Text.null

{-# INLINE single𝕊 #-}
single𝕊 ∷ ℂ → 𝕊
single𝕊 = Text.singleton

{-# INLINE build𝕊 #-}
build𝕊 ∷ (ToIter ℂ t) ⇒ t → 𝕊
build𝕊 = Text.pack ∘ lazyList𝐼 ∘ iter

{-# INLINE build𝕊C #-}
build𝕊C ∷ (ToIter 𝕊 t) ⇒ t → 𝕊
build𝕊C = TextLazy.toStrict ∘ TextBuilder.toLazyText ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.fromText) ∘ iter

{-# INLINE build𝕊N #-}
build𝕊N ∷ (ToIter ℂ t) ⇒ ℕ64 → t → 𝕊
build𝕊N n = TextLazy.toStrict ∘ TextBuilder.toLazyTextWith (HS.fromIntegral n) ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.singleton) ∘ iter

{-# INLINE build𝕊CN #-}
build𝕊CN ∷ (ToIter 𝕊 t) ⇒ ℕ64 → t → 𝕊
build𝕊CN n = TextLazy.toStrict ∘ TextBuilder.toLazyTextWith (HS.fromIntegral n) ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.fromText) ∘ iter

{-# INLINE show𝕊 #-}
show𝕊 ∷ (Show a) ⇒ a → 𝕊
show𝕊 = fromChars ∘ HS.show

{-# INLINE read𝕊 #-}
read𝕊 ∷ (HS.Read a) ⇒ 𝕊 → a
read𝕊 = HS.read ∘ chars

{-# INLINE lower𝕊 #-}
lower𝕊 ∷ 𝕊 → 𝕊
lower𝕊 = Text.toLower

{-# INLINE upper𝕊 #-}
upper𝕊 ∷ 𝕊 → 𝕊
upper𝕊 = Text.toUpper

{-# INLINE isEmpty𝕊 #-}
isEmpty𝕊 ∷ 𝕊 → 𝔹
isEmpty𝕊 = Text.null

{-# INLINE length𝕊 #-}
length𝕊 ∷ 𝕊 → ℕ
length𝕊 = natΩ ∘ frhs ∘ Text.length

{-# INLINE length64𝕊 #-}
length64𝕊 ∷ 𝕊 → ℕ64
length64𝕊 = natΩ64 ∘ frhs ∘ Text.length

{-# INLINE splitOn𝕊 #-}
splitOn𝕊 ∷ 𝕊 → 𝕊 → 𝑆 𝕊
splitOn𝕊 i s = streamLL $ Text.splitOn i s
