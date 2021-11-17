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
  null = Text.empty
instance Append 𝕊 where 
  (⧺) = Text.append
instance Monoid 𝕊

instance Single ℂ 𝕊 where 
  single = Text.singleton

instance ToIter ℂ 𝕊 where 
  iter cs = 𝐼 HS.$ \ f → flip $ \ 𝓀 →
    case TextI.stream cs of
      TextI.Stream g s₀ _ →
        let loop s i = case g s of
              TextI.Done → 𝓀 i
              TextI.Skip s' → loop s' i
              TextI.Yield c s' →
                f c i $ \ i' →
                loop s' i'
        in loop s₀

instance Lookup ℕ ℂ 𝕊 where 
  s ⋕? n 
    | (n > 0) ⩓ (n ≤ length𝕊 s) = Some $ Text.index s $ HS.fromIntegral $ n - 1
    | otherwise = None

empty𝕊 ∷ 𝕊 → 𝔹
empty𝕊 = Text.null

single𝕊 ∷ ℂ → 𝕊
single𝕊 = Text.singleton

build𝕊C ∷ (ToIter ℂ t) ⇒ t → 𝕊
build𝕊C = Text.pack ∘ lazyList𝐼 ∘ iter

build𝕊S ∷ (ToIter 𝕊 t) ⇒ t → 𝕊
build𝕊S = TextLazy.toStrict ∘ TextBuilder.toLazyText ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.fromText) ∘ iter

build𝕊CN ∷ (ToIter ℂ t) ⇒ ℕ64 → t → 𝕊
build𝕊CN n = TextLazy.toStrict ∘ TextBuilder.toLazyTextWith (HS.fromIntegral n) ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.singleton) ∘ iter

build𝕊SN ∷ (ToIter 𝕊 t) ⇒ ℕ64 → t → 𝕊
build𝕊SN n = TextLazy.toStrict ∘ TextBuilder.toLazyTextWith (HS.fromIntegral n) ∘ foldr𝐼 HS.mempty (HS.mappend ∘ TextBuilder.fromText) ∘ iter

show𝕊 ∷ (Show a) ⇒ a → 𝕊
show𝕊 = frhsChars ∘ HS.show

read𝕊 ∷ (HS.Read a) ⇒ 𝕊 → a
read𝕊 = HS.read ∘ tohsChars

lower𝕊 ∷ 𝕊 → 𝕊
lower𝕊 = Text.toLower

upper𝕊 ∷ 𝕊 → 𝕊
upper𝕊 = Text.toUpper

isEmpty𝕊 ∷ 𝕊 → 𝔹
isEmpty𝕊 = Text.null

length𝕊 ∷ 𝕊 → ℕ
length𝕊 = natΩ ∘ frhs ∘ Text.length

length64𝕊 ∷ 𝕊 → ℕ64
length64𝕊 = natΩ64 ∘ frhs ∘ Text.length

splitOn𝕊 ∷ 𝕊 → 𝕊 → 𝐼 𝕊
splitOn𝕊 i s = iterLL $ Text.splitOn i s

replace𝕊 ∷ 𝕊 → 𝕊 → 𝕊 → 𝕊
replace𝕊 = Text.replace
