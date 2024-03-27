module UVMHS.Core.Data.Iter where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()
import UVMHS.Core.Data.Choice
import UVMHS.Core.Data.List ()
import UVMHS.Core.Data.String
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.Stream

import qualified Prelude as HS
import qualified Data.List as HS


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

instance (Show a) ⇒ Show (𝑆 a) where show = tohsChars ∘ showCollection "𝑆[" "]" "," show𝕊
instance (Show a) ⇒ Show (𝐼 a) where show = tohsChars ∘ showCollection "𝐼[" "]" "," show𝕊
instance (Show a) ⇒ Show (𝐿 a) where show = tohsChars ∘ showCollection "[" "]" "," show𝕊

instance 𝕊 ⇄ 𝐼 ℂ where
  isoto = iter ∘ tohsChars
  isofr = string

empty𝐼 ∷ 𝐼 a
empty𝐼 = null𝐼

cons𝐼 ∷ a → 𝐼 a → 𝐼 a
cons𝐼 x xs = 𝐼 HS.$ \ f i 𝓀 → 
  f x i $ \ i' →
  un𝐼 xs f i' 𝓀

stream ∷ (ToIter a t) ⇒ t → 𝑆 a
stream = stream𝐼 ∘ iter

zipWith ∷ (ToIter a t₁,ToIter b t₂) ⇒ (a → b → c) → t₁ → t₂ → 𝐼 c
zipWith f xs ys = iter $ zipWith𝑆 f (stream xs) $ stream ys

zip ∷ (ToIter a t₁,ToIter b t₂) ⇒ t₁ → t₂ → 𝐼 (a ∧ b)
zip = zipWith (:*)

snoc𝐼 ∷ 𝐼 a → a → 𝐼 a
snoc𝐼 xs x = 𝐼 HS.$ \ f i 𝓀 → 
  un𝐼 xs f i $ \ i' →
  f x i' 𝓀

isEmpty ∷ (ToIter a t) ⇒ t → 𝔹
isEmpty xs = run𝐼On (iter xs) id True $ \ _ _ _ → False

firstElem ∷ (ToIter a t) ⇒ t → 𝑂 a
firstElem xs = run𝐼On (iter xs) id None $ \ x _ _ → Some x

append𝐼 ∷ 𝐼 a → 𝐼 a → 𝐼 a
append𝐼 xs ys = 𝐼 HS.$ \ f i 𝓀 →
  un𝐼 xs f i $ \ i' →
  un𝐼 ys f i' 𝓀

bind𝐼 ∷ ∀ a b. 𝐼 a → (a → 𝐼 b) → 𝐼 b
bind𝐼 xs f =
  𝐼 (
    \ (g ∷ b → c → (c → c) → c) (i₀ ∷ c) (k₀ ∷ c → c) →
        un𝐼 xs (\ (x ∷ a) (i ∷ c) (k ∷ c → c) →
          un𝐼 (f x) g i k)
        i₀ k₀
  )

mjoin𝐼 ∷ ∀ a. 𝐼 (𝐼 a) → 𝐼 a
mjoin𝐼 xss = bind𝐼 xss id

mapM𝐼 ∷ (Monad m) ⇒ (a → m b) → 𝐼 a → m (𝐼 b)
mapM𝐼 f = fold𝐼 (return empty𝐼) $ \ x ysM → do
  ys ← ysM
  y ← f x
  return $ snoc𝐼 ys y

fold ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
fold i f = fold𝐼 i f ∘ iter

foldFromWith ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldFromWith = fold

foldFromOn ∷ (ToIter a t) ⇒ b → t → (a → b → b) → b
foldFromOn = flip ∘ fold

foldOnFrom ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldOnFrom = rotateR fold

foldOnWith ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldOnWith = mirror fold

foldWithOn ∷ (ToIter a t) ⇒ (a → b → b) → t → b → b
foldWithOn = rotateL fold

foldWithFrom ∷ (ToIter a t) ⇒ (a → b → b) → b → t → b
foldWithFrom = flip fold

foldk ∷ (ToIter a t) ⇒ b → (a → b → (b → b) → b) → t → b
foldk i f = foldk𝐼 i f ∘ iter

foldkFromWith ∷ (ToIter a t) ⇒ b → (a → b → (b → b) → b) → t → b
foldkFromWith = foldk

foldkFromOn ∷ (ToIter a t) ⇒ b → t → (a → b → (b → b) → b) → b
foldkFromOn = flip ∘ foldk

foldkOnFrom ∷ (ToIter a t) ⇒ t → b → (a → b → (b → b) → b) → b
foldkOnFrom = rotateR foldk

foldkOnWith ∷ (ToIter a t) ⇒ t → (a → b → (b → b) → b) → b → b
foldkOnWith = mirror foldk

foldkWithOn ∷ (ToIter a t) ⇒ (a → b → (b → b) → b) → t → b → b
foldkWithOn = rotateL foldk

foldkWithFrom ∷ (ToIter a t) ⇒ (a → b → (b → b) → b) → b → t → b
foldkWithFrom = flip foldk

foldr ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldr i f = foldr𝐼 i f ∘ iter

foldrFromWith ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldrFromWith = foldr

foldrFromOn ∷ (ToIter a t) ⇒ b → t → (a → b → b) → b
foldrFromOn = flip ∘ foldr

foldrOnFrom ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldrOnFrom = rotateR foldr

foldrOnWith ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldrOnWith = mirror foldr

foldrWithOn ∷ (ToIter a t) ⇒ (a → b → b) → t → b → b
foldrWithOn = rotateL foldr

foldrWithFrom ∷ (ToIter a t) ⇒ (a → b → b) → b → t → b
foldrWithFrom = flip foldr

mfold ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfold i₀ f = foldkFromWith (return i₀) $ \ x iM 𝓀 → do i ← iM ; 𝓀 $ f x i

mfoldFromWith ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldFromWith = mfold

mfoldFromOn ∷ (Monad m,ToIter a t) ⇒ b → t → (a → b → m b) → m b
mfoldFromOn = flip ∘ mfold

mfoldOnFrom ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldOnFrom = rotateR mfold

mfoldOnWith ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldOnWith = mirror mfold

mfoldWithOn ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → t → b → m b
mfoldWithOn = rotateL mfold

mfoldWithFrom ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → b → t → m b
mfoldWithFrom = flip mfold

mfoldk ∷ (Monad m,ToIter a t) ⇒ b → (a → b → (m b → m b) → m b) → t → m b
mfoldk i₀ f = foldkFromWith (return i₀) $ \ x iM 𝓀 → do i ← iM ; f x i 𝓀

mfoldkFromWith ∷ (Monad m,ToIter a t) ⇒ b → (a → b → (m b → m b) → m b) → t → m b
mfoldkFromWith = mfoldk

mfoldkFromOn ∷ (Monad m,ToIter a t) ⇒ b → t → (a → b → (m b → m b) → m b) → m b
mfoldkFromOn = flip ∘ mfoldk

mfoldkOnFrom ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → (m b → m b) → m b) → m b
mfoldkOnFrom = rotateR mfoldk

mfoldkOnWith ∷ (Monad m,ToIter a t) ⇒ t → (a → b → (m b → m b) → m b) → b → m b
mfoldkOnWith = mirror mfoldk

mfoldkWithOn ∷ (Monad m,ToIter a t) ⇒ (a → b → (m b → m b) → m b) → t → b → m b
mfoldkWithOn = rotateL mfoldk

mfoldkWithFrom ∷ (Monad m,ToIter a t) ⇒ (a → b → (m b → m b) → m b) → b → t → m b
mfoldkWithFrom = flip mfoldk

mfoldr ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldr i₀ f = foldkFromWith (return i₀) $ \ x iM 𝓀 → do i ← 𝓀 iM ; f x i

mfoldrFromWith ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldrFromWith = mfoldr

mfoldrFromOn ∷ (Monad m,ToIter a t) ⇒ b → t → (a → b → m b) → m b
mfoldrFromOn = flip ∘ mfoldr

mfoldrOnFrom ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldrOnFrom = rotateR mfoldr

mfoldrOnWith ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldrOnWith = mirror mfoldr

mfoldrWithOn ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → t → b → m b
mfoldrWithOn = rotateL mfoldr

mfoldrWithFrom ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → b → t → m b
mfoldrWithFrom = flip mfoldr

eachWith ∷ (Monad m,ToIter a t) ⇒ (a → m ()) → t → m ()
eachWith f = mfoldFromWith () $ const ∘ f

eachOn ∷ (Monad m,ToIter a t) ⇒ t → (a → m ()) → m () 
eachOn = flip eachWith

eachkWith ∷ (Monad m,ToIter a t) ⇒ (a → (m () → m ()) → m ()) → t → m ()
eachkWith f = mfoldkFromWith () $ const ∘ f

eachkOn ∷ (Monad m,ToIter a t) ⇒ t → (a → (m () → m ()) → m ()) → m () 
eachkOn = flip eachkWith

exec ∷ (Monad m,ToIter (m ()) t) ⇒ t → m () 
exec = eachWith id

sum ∷ (ToIter a t,Additive a) ⇒ t → a
sum = fold zero (+)

product ∷ (ToIter a t,Multiplicative a) ⇒ t → a
product = fold one (×)

concat ∷ (Monoid a,ToIter a t) ⇒ t → a
concat = foldr null (⧺)

sequence ∷ (Seqoid a,ToIter a t) ⇒ t → a
sequence = foldr eps (▷)

compose ∷ (ToIter (a → a) t) ⇒ t → a → a
compose = foldr id (∘)

mcompose ∷ (Monad m) ⇒ (ToIter (a → m a) t) ⇒ t → a → m a
mcompose = foldr return (*∘)

wcompose ∷ (Comonad w) ⇒ (ToIter (w a → a) t) ⇒ t → w a → a
wcompose = foldr extract (%∘)

minsFrom ∷ (ToIter a t,Ord a) ⇒ a → t → a
minsFrom = foldWithFrom (⩎)

maxsFrom ∷ (ToIter a t,Ord a) ⇒ a → t → a
maxsFrom = foldWithFrom (⩏)

joinsFrom ∷ (ToIter a t,Join a) ⇒ a → t → a
joinsFrom = foldWithFrom (⊔)

joins ∷ (JoinLattice a,ToIter a t) ⇒ t → a
joins = joinsFrom bot

meetsFrom ∷ (ToIter a t,Meet a) ⇒ a → t → a
meetsFrom = foldWithFrom (⊓)

meets ∷ (MeetLattice a,ToIter a t) ⇒ t → a
meets = meetsFrom top

or ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
or = foldk False $ \ b₁ b₂ 𝓀 → if b₁ then True else 𝓀 b₂

orf ∷ (ToIter (a → 𝔹) t) ⇒ t → a → 𝔹
orf fs x = or $ map (appto x) $ iter fs

andf ∷ (ToIter (a → 𝔹) t) ⇒ t → a → 𝔹
andf fs x = and $ map (appto x) $ iter fs

and ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
and = foldk True $ \ b₁ b₂ 𝓀 → if b₁ then 𝓀 b₂ else False

count ∷ ∀ n t a. (Zero n,One n,Plus n,ToIter a t) ⇒ t → n
count = fold zero $ const succ

countWith ∷ ∀ n t a. (Zero n,One n,Plus n,ToIter a t) ⇒ (a → 𝔹) → t → n
countWith f = fold zero $ \ x → case f x of
  True → succ
  False → id

reverse ∷ (ToIter a t) ⇒ t → 𝐼 a
reverse xs = 𝐼 HS.$ \ f i₀ 𝓀₀ → un𝐼 (iter xs) (\ x 𝓀 m𝓀 → m𝓀 $ \ i → f x i 𝓀) 𝓀₀ id i₀

replicateI ∷ ∀ n a. (Eq n,Zero n,One n,Plus n) ⇒ n → (n → a) → 𝐼 a
replicateI n₀ g = 𝐼 HS.$ \ f → flip $ \ 𝓀 → 
  let loop n i
        | n ≡ n₀ = 𝓀 i
        | otherwise = 
            f (g n) i $ \ i' →
            loop (succ n) i'
  in loop zero

replicate ∷ ∀ n a. (Eq n,Zero n,One n,Plus n) ⇒ n → a → 𝐼 a
replicate n = replicateI n ∘ const

build ∷ ∀ n a. (Eq n,Zero n,One n,Plus n) ⇒ n → a → (a → a) → 𝐼 a
build n₀ x₀ g = 𝐼 HS.$ \ f → flip $ \ 𝓀 → 
  let loop n x i
        | n ≡ n₀ = 𝓀 i
        | otherwise = 
            f x i $ \ i' →
            loop (succ n) (g x) i'
  in loop zero x₀

range ∷ (Eq n,Zero n,One n,Plus n,Minus n) ⇒ n → n → 𝐼 n
range lb ub = build (ub - lb) lb succ

upto ∷ (Eq n,Zero n,One n,Plus n) ⇒ n → 𝐼 n
upto n = build n zero succ

reiter ∷ (ToIter a t) ⇒ s → (a → s → (s ∧ b)) → t → 𝐼 b
reiter s₀ f xs = 
  𝐼 HS.$ \ g i₀ 𝓀₀ → 
    snd $ run𝐼On (iter xs) (\ (s :* i) → s :* 𝓀₀ i) (s₀ :* i₀) $ \ x (s :* i) 𝓀 → 
        let s' :* y = f x s
        in (s' :*) $ g y i $ \ i' → 
          snd $ 𝓀 $ s' :* i'

withIndex ∷ ∀ n t a. (Zero n,One n,Plus n,ToIter a t) ⇒ t → 𝐼 (n ∧ a)
withIndex = reiter zero $ \ x i → (i + one) :* (i :* x)

withFirst ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withFirst = reiter True $ \ x b → False :* (b :* x)

mapFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapFirst f = reiter True $ \ x b → 
  let x' = if b then f x else x 
  in False :* x'

mapAfterFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapAfterFirst f = reiter True $ \ x b → 
  let x' = if b then x else f x 
  in False :* x'

keepN ∷ (ToIter a t,Eq n,Zero n,One n,Plus n) ⇒ n → t → 𝐼 a
keepN n₀ xs = 𝐼 HS.$ \ f i₀ 𝓀₀ → 
  let g x (n :* i) 𝓀 = (succ n :*) $
        if n ≡ n₀
        then 𝓀₀ i
        else f x i $ snd ∘ 𝓀 ∘ (succ n :*)
  in snd $ un𝐼 (iter xs) g (zero :* i₀) $ mapSnd 𝓀₀

withLast ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withLast = reverse ∘ withFirst ∘ reverse

mapLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapLast f = map (\ (b :* x) → case b of {True → f x;False → x}) ∘ withLast

mapLastOn ∷ (ToIter a t) ⇒ t → (a → a) → 𝐼 a
mapLastOn = flip mapLast

mapBeforeLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapBeforeLast f = map (\ (b :* x) → case b of {True → x;False → f x}) ∘ withLast

filterMap ∷ (ToIter a t) ⇒ (a → 𝑂 b) → t → 𝐼 b
filterMap f xs = 𝐼 HS.$ \ g →
  un𝐼 (iter xs) $ \ x i 𝓀 → 
    case f x of
      None → 𝓀 i
      Some y → g y i 𝓀

filterMapOn ∷ (ToIter a t) ⇒ t → (a → 𝑂 b) → 𝐼 b
filterMapOn = flip filterMap

filter ∷ (ToIter a t) ⇒ (a → 𝔹) → t → 𝐼 a
filter f = filterMap $ \ x → case f x of {True → Some x;False → None}

filterOn ∷ (ToIter a t) ⇒ t → (a → 𝔹) → 𝐼 a
filterOn = flip filter

inbetween ∷ (ToIter a t) ⇒ a → t → 𝐼 a
inbetween xⁱ xs = 𝐼 HS.$ \ f →
  un𝐼 (withFirst $ iter xs) $ \ (b :* x) i 𝓀 →
    if b 
    then f x i 𝓀
    else 
      f xⁱ i $ \ i' →
      f x i' 𝓀

alignLeftFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignLeftFill c n s = build𝕊S $ concat
  [ single𝐼 s
  , single𝐼 $ string $ replicate (n - length𝕊 s ⊓ n) c
  ]

alignLeft ∷ ℕ → 𝕊 → 𝕊
alignLeft = alignLeftFill ' '

alignRightFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignRightFill c n s = build𝕊S $ concat
  [ single𝐼 $ string $ replicate (n - length𝕊 s ⊓ n) c
  , single𝐼 s
  ]

alignRight ∷ ℕ → 𝕊 → 𝕊
alignRight = alignRightFill ' '

list ∷ (ToIter a t) ⇒ t → 𝐿 a
list = list𝐼 ∘ iter

lazyList ∷ (ToIter a t) ⇒ t → [a]
lazyList = lazyList𝐼 ∘ iter

string ∷ (ToIter ℂ t) ⇒ t → 𝕊
string = build𝕊C

stringS ∷ (ToIter 𝕊 t) ⇒ t → 𝕊
stringS = build𝕊S

truncate𝕊 ∷ ℕ64 → 𝕊 → 𝕊 → 𝕊
truncate𝕊 n t s =
  if natΩ64 (length𝕊 s) ≤ n
  then s
  else string $ keepN n s ⧺ iter t

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

sortWith ∷ (ToIter a t) ⇒ (a → a → Ordering) → t → 𝐿 a
sortWith f = list ∘ HS.sortBy f ∘ lazyList

sortOn ∷ (ToIter a t,Ord b) ⇒ (a → b) → t → 𝐿 a
sortOn f = sortWith $ (⋚) `on` f

sort ∷ (ToIter a t,Ord a) ⇒ t → 𝐿 a
sort = sortWith (⋚)

materialize ∷ (ToIter a t) ⇒ t → 𝐼 a
materialize = iter ∘ list

mapWhile ∷ (a → a) → (a → 𝔹) → 𝐼 a → 𝐼 a
mapWhile f p = reiter True $ \ x b → do
  if b ⩓ p x
  then True :* f x
  else False :* x

dropWhile ∷ (a → 𝔹) → 𝐼 a → 𝐼 a
dropWhile p xs₀ =
  let loop = \case
        None → null
        Some (x :* xs')
          | p x → loop $ un𝑆 xs' ()
          | otherwise → iter $ 𝑆 $ \ () → Some $ x :* xs'
  in loop $ un𝑆 (stream xs₀) ()

partition ∷ (a → b ∨ c) → 𝐿 a → 𝐿 b ∧ 𝐿 c
partition decide = foldrFromWith (Nil :* Nil) $
  elimChoice (mapFst ∘ (:&)) (mapSnd ∘ (:&)) ∘ decide

partition𝔹 ∷ (a → 𝔹) → 𝐿 a → 𝐿 a ∧ 𝐿 a
partition𝔹 decide = partition (\ a → elim𝔹 (Inl a) (Inr a) (decide a))

---------
-- All --
---------

instance All () where
  all = single ()

instance All 𝔹 where 
  all = iter [True,False]

instance (All a) ⇒ All (𝑂 a) where
  all = single None ⧺ map Some all

instance (All a,All b) ⇒ All (a ∨ b) where
  all = map Inl all ⧺ map Inr all

instance (All a,All b) ⇒ All (a ∧ b) where 
  all = do x ← all ; y ← all ; return $ x :* y


----
