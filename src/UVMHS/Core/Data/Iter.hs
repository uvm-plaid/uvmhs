module UVMHS.Core.Data.Iter where

import UVMHS.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()
import UVMHS.Core.Data.List ()
import UVMHS.Core.Data.LazyList
import UVMHS.Core.Data.String
import UVMHS.Core.Data.Pair

instance (Show a) ⇒ Show (𝐼 a) where show = chars ∘ showWith𝐼 show𝕊

instance Null (𝐼 a) where null = empty𝐼
instance Append (𝐼 a) where (⧺) = append𝐼
instance Monoid (𝐼 a)
instance Functor 𝐼 where map = map𝐼
instance Return 𝐼 where return = single𝐼
instance Bind 𝐼 where (≫=) = bind𝐼
instance Monad 𝐼
instance FunctorM 𝐼 where mapM = mapM𝐼
instance Single a (𝐼 a) where single = single𝐼
instance ToIter a (𝐼 a) where iter = id

instance (Show a) ⇒ Show (𝐿 a) where show = chars ∘ showCollection "[" "]" "," show𝕊

instance 𝕊 ⇄ 𝐼 ℂ where
  isoto = iter ∘ chars
  isofr = string

empty𝐼 ∷ 𝐼 a
empty𝐼 = 𝐼 $ \ _ → id

single𝐼 ∷ a → 𝐼 a
single𝐼 x = 𝐼 $ \ f → f x

cons𝐼 ∷ a → 𝐼 a → 𝐼 a
cons𝐼 x (𝐼 g) = 𝐼 $ \ f → g f ∘ f x

snoc𝐼 ∷ 𝐼 a → a → 𝐼 a
snoc𝐼 (𝐼 g) x = 𝐼 $ \ f → f x ∘ g f

append𝐼 ∷ 𝐼 a → 𝐼 a → 𝐼 a
append𝐼 (𝐼 g₁) (𝐼 g₂) = 𝐼 $ \ f → g₂ f ∘ g₁ f

mjoin𝐼 ∷ 𝐼 (𝐼 a) → 𝐼 a
mjoin𝐼 = fold𝐼 empty𝐼 $ flip append𝐼

bind𝐼 ∷ 𝐼 a → (a → 𝐼 b) → 𝐼 b
bind𝐼 xs f = mjoin𝐼 $ map𝐼 f xs

mapM𝐼 ∷ (Monad m) ⇒ (a → m b) → 𝐼 a → m (𝐼 b)
mapM𝐼 f = fold𝐼 (return empty𝐼) $ \ x ysM → do
  ys ← ysM
  y ← f x
  return $ snoc𝐼 ys y

fold ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
fold i f = fold𝐼 i f ∘ iter

foldFrom ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldFrom = mirror fold

foldWith ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldWith = rotateR fold

foldk ∷ (ToIter a t) ⇒ b → (a → (b → b) → (b → b)) → t → b
foldk i f = foldk𝐼 i f ∘ iter

foldkFrom ∷ (ToIter a t) ⇒ t → (a → (b → b) → (b → b)) → b → b
foldkFrom = mirror foldk

foldkWith ∷ (ToIter a t) ⇒ t → b → (a → (b → b) → (b → b)) → b
foldkWith = rotateR foldk

foldr ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldr i f = foldr𝐼 i f ∘ iter

foldrFrom ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldrFrom = mirror foldr

foldrWith ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldrWith = rotateR foldr

mfold ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfold i f = fold (return i) (extend ∘ f)

mfoldFrom ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldFrom = mirror mfold

mfoldWith ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldWith = rotateR mfold

mfoldr ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldr i f = foldr (return i) (extend ∘ f)

mfoldrFrom ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldrFrom = mirror mfoldr

mfoldrWith ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldrWith = rotateR mfoldr

each ∷ (Monad m,ToIter a t) ⇒ (a → m ()) → t → m ()
each f = fold skip $ \ x yM → yM ≫ f x

eachWith ∷ (Monad m,ToIter a t) ⇒ t → (a → m ()) → m () 
eachWith = flip each

exec ∷ (Monad m,ToIter (m ()) t) ⇒ t → m () 
exec = each id

sum ∷ (ToIter a t,Additive a) ⇒ t → a
sum = fold zero (+)

product ∷ (ToIter a t,Multiplicative a) ⇒ t → a
product = fold one (×)

concat ∷ (Monoid a,ToIter a t) ⇒ t → a
concat = fold null $ flip (⧺)

compose ∷ (ToIter (a → a) t) ⇒ t → a → a
compose = fold id $ flip (∘)

mcompose ∷ (Monad m) ⇒ (ToIter (a → m a) t) ⇒ t → a → m a
mcompose = fold return $ flip (*∘)

wcompose ∷ (Comonad w) ⇒ (ToIter (w a → a) t) ⇒ t → w a → a
wcompose = fold extract $ flip (%∘)

joins ∷ (JoinLattice a,ToIter a t) ⇒ t → a
joins = fold bot (⊔)

meets ∷ (MeetLattice a,ToIter a t) ⇒ t → a
meets = fold top (⊓)

or ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
or = fold False (⩔)

and ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
and = fold True (⩓)

count ∷ (ToIter a t) ⇒ t → ℕ
count = fold 0 $ const succ

countWith ∷ (ToIter a t) ⇒ (a → 𝔹) → t → ℕ
countWith f = fold 0 $ \ x → case f x of
  True → succ
  False → id

reverse ∷ (ToIter a t) ⇒ t → 𝐼 a
reverse xs = 𝐼 $ \ (f ∷ a → b → b) (i ∷ b) → foldr i f xs

repeatI ∷ ℕ → (ℕ → a) → 𝐼 a
repeatI n₀ g = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  let loop ∷ ℕ → b → b
      loop n i
        | n ≡ n₀ = i
        | otherwise =
          let n' = succ n
          in loop n' (f (g n') i)
  in loop 0 i₀

repeat ∷ ℕ → a → 𝐼 a
repeat n = repeatI n ∘ const

build ∷ ∀ a. ℕ → a → (a → a) → 𝐼 a
build n₀ x₀ g = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  let loop ∷ ℕ → a → b → b
      loop n x i
        | n ≡ n₀ = i
        | otherwise =
            let x' = g x
            in loop (succ n) x' (f x' i)
  in loop 0 x₀ i₀

upTo ∷ ℕ → 𝐼 ℕ
upTo n = build n 0 succ

withIndex ∷ (ToIter a t) ⇒ t → 𝐼 (ℕ ∧ a)
withIndex xs = 𝐼 $ \ (f ∷ (ℕ ∧ a) → b → b) (i₀ ∷ b) →
  snd $ foldWith xs (1 :* i₀) $ \ (x ∷ a) (n :* i ∷ ℕ ∧ b) → succ n :* f (n :* x) i

withFirst ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withFirst xs = 𝐼 $ \ (f ∷ (𝔹 ∧ a) → b → b) (i₀ ∷ b) →
  snd $ foldWith xs (True :* i₀) $ \ (x ∷ a) (b :* i ∷ 𝔹 ∧ b) → False :* f (b :* x) i

mapFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapFirst f = map (\ (b :* x) → case b of {True → f x;False → x}) ∘ withFirst

mapAfterFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapAfterFirst f = map (\ (b :* x) → case b of {True → x;False → f x}) ∘ withFirst

withLast ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withLast = reverse ∘ withFirst ∘ reverse

mapLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapLast f = map (\ (b :* x) → case b of {True → f x;False → x}) ∘ withLast

mapBeforeLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapBeforeLast f = map (\ (b :* x) → case b of {True → x;False → f x}) ∘ withLast

filterMap ∷ (ToIter a t) ⇒ (a → 𝑂 b) → t → 𝐼 b
filterMap g xs = 𝐼 $ \ (f ∷ b → c → c) (i₀ ∷ c) →
  foldWith xs i₀ $ \ (x ∷ a) →
    case g x of
      None → id
      Some y → f y

filter ∷ (ToIter a t) ⇒ (a → 𝔹) → t → 𝐼 a
filter f = filterMap $ \ x → case f x of {True → Some x;False → None}

inbetween ∷ (ToIter a t) ⇒ a → t → 𝐼 a
inbetween xⁱ xs = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  foldWith (withFirst xs) i₀ $ \ (b :* x ∷ 𝔹 ∧ a) →
    case b of
      True → f x
      False → f x ∘ f xⁱ

execN ∷ (Monad m) ⇒ ℕ → m () → m ()
execN n = exec ∘ repeat n

applyN ∷ ℕ → b → (b → b) → b
applyN n i f = fold i (const f) $ upTo n

appendN ∷ (Monoid a) ⇒ ℕ → a → a 
appendN n x = applyN n null $ (⧺) x

alignLeftFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignLeftFill c n s = build𝕊 $ single s ⧺ repeat (n - length𝕊 s ⊓ n) (single𝕊 c)

alignLeft ∷ ℕ → 𝕊 → 𝕊
alignLeft = alignLeftFill ' '

alignRightFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignRightFill c n s = build𝕊 $ repeat (n - length𝕊 s ⊓ n) (single𝕊 c) ⧺ single s

alignRight ∷ ℕ → 𝕊 → 𝕊
alignRight = alignRightFill ' '

list ∷ (ToIter a t) ⇒ t → 𝐿 a
list = list𝐼 ∘ iter

string ∷ (ToIter ℂ t) ⇒ t → 𝕊
string = fromChars ∘ lazyList

showCollection ∷ (ToIter a t) ⇒ 𝕊 → 𝕊 → 𝕊 → (a → 𝕊) → t → 𝕊
showCollection l r i showA xs = concat
  [ l
  , concat $ inbetween i $ map showA $ iter xs
  , r
  ]

showWith𝐼 ∷ (a → 𝕊) → 𝐼 a → 𝕊
showWith𝐼 = showCollection "𝐼[" "]" ","

firstMaxByLT ∷ (ToIter a t) ⇒ (a → a → 𝔹) → t → 𝑂 a
firstMaxByLT f = fold None $ \ x xM →
  case xM of
    None → Some x
    Some x' → case f x' x of
      True → Some x
      False → Some x'
