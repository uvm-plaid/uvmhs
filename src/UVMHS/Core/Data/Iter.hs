module UVMHS.Core.Data.Iter where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()
import UVMHS.Core.Data.List ()
import UVMHS.Core.Data.String
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.Function

import qualified Data.List as HS

instance (Show a) ⇒ Show (𝐼 a) where 
  {-# INLINE show #-}
  show = chars ∘ showWith𝐼 show𝕊

instance Null (𝐼 a) where 
  {-# INLINE null #-}
  null = empty𝐼
instance Append (𝐼 a) where 
  {-# INLINE (⧺) #-}
  (⧺) = append𝐼
instance Monoid (𝐼 a)

instance Functor 𝐼 where 
  {-# INLINE map #-}
  map = map𝐼
instance Return 𝐼 where 
  {-# INLINE return #-}
  return = single𝐼
instance Bind 𝐼 where 
  {-# INLINE (≫=) #-}
  (≫=) = bind𝐼
instance Monad 𝐼
instance FunctorM 𝐼 where 
  {-# INLINE mapM #-}
  mapM = mapM𝐼
instance Single a (𝐼 a) where 
  {-# INLINE single #-}
  single = single𝐼
instance ToIter a (𝐼 a) where 
  {-# INLINE iter #-}
  iter = id

instance (Show a) ⇒ Show (𝐿 a) where 
  {-# INLINE show #-}
  show = chars ∘ showCollection "[" "]" "," show𝕊

instance 𝕊 ⇄ 𝐼 ℂ where
  {-# INLINE isoto #-}
  isoto = iter ∘ chars
  {-# INLINE isofr #-}
  isofr = string

{-# INLINE empty𝐼 #-}
empty𝐼 ∷ 𝐼 a
empty𝐼 = 𝐼 $ \ _ → id

{-# INLINE single𝐼 #-}
single𝐼 ∷ a → 𝐼 a
single𝐼 x = 𝐼 $ \ f → f x

{-# INLINE cons𝐼 #-}
cons𝐼 ∷ a → 𝐼 a → 𝐼 a
cons𝐼 x (𝐼 g) = 𝐼 $ \ f → g f ∘ f x

{-# INLINE snoc𝐼 #-}
snoc𝐼 ∷ 𝐼 a → a → 𝐼 a
snoc𝐼 (𝐼 g) x = 𝐼 $ \ f → f x ∘ g f

{-# INLINE append𝐼 #-}
append𝐼 ∷ 𝐼 a → 𝐼 a → 𝐼 a
append𝐼 (𝐼 g₁) (𝐼 g₂) = 𝐼 $ \ f → g₂ f ∘ g₁ f

{-# INLINE mjoin𝐼 #-}
mjoin𝐼 ∷ 𝐼 (𝐼 a) → 𝐼 a
mjoin𝐼 = fold𝐼 empty𝐼 $ flip append𝐼

{-# INLINE bind𝐼 #-}
bind𝐼 ∷ 𝐼 a → (a → 𝐼 b) → 𝐼 b
bind𝐼 xs f = mjoin𝐼 $ map𝐼 f xs

{-# INLINE mapM𝐼 #-}
mapM𝐼 ∷ (Monad m) ⇒ (a → m b) → 𝐼 a → m (𝐼 b)
mapM𝐼 f = fold𝐼 (return empty𝐼) $ \ x ysM → do
  ys ← ysM
  y ← f x
  return $ snoc𝐼 ys y

{-# INLINE fold #-}
fold ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
fold i f = fold𝐼 i f ∘ iter

{-# INLINE foldFromWith #-}
foldFromWith ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldFromWith = fold

{-# INLINE foldFromOn #-}
foldFromOn ∷ (ToIter a t) ⇒ b → t → (a → b → b) → b
foldFromOn = flip ∘ fold

{-# INLINE foldOnFrom #-}
foldOnFrom ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldOnFrom = rotateR fold

{-# INLINE foldOnWith #-}
foldOnWith ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldOnWith = mirror fold

{-# INLINE foldWithOn #-}
foldWithOn ∷ (ToIter a t) ⇒ (a → b → b) → t → b → b
foldWithOn = rotateL fold

{-# INLINE foldWithFrom #-}
foldWithFrom ∷ (ToIter a t) ⇒ (a → b → b) → b → t → b
foldWithFrom = flip fold

{-# INLINE foldk #-}
foldk ∷ (ToIter a t) ⇒ b → (a → (b → b) → (b → b)) → t → b
foldk i f = foldk𝐼 i f ∘ iter

{-# INLINE foldkFromWith #-}
foldkFromWith ∷ (ToIter a t) ⇒ b → (a → (b → b) → (b → b)) → t → b
foldkFromWith = foldk

{-# INLINE foldkFromOn #-}
foldkFromOn ∷ (ToIter a t) ⇒ b → t → (a → (b → b) → (b → b)) → b
foldkFromOn = flip ∘ foldk

{-# INLINE foldkOnFrom #-}
foldkOnFrom ∷ (ToIter a t) ⇒ t → b → (a → (b → b) → (b → b)) → b
foldkOnFrom = rotateR foldk

{-# INLINE foldkOnWith #-}
foldkOnWith ∷ (ToIter a t) ⇒ t → (a → (b → b) → (b → b)) → b → b
foldkOnWith = mirror foldk

{-# INLINE foldkWithOn #-}
foldkWithOn ∷ (ToIter a t) ⇒ (a → (b → b) → (b → b)) → t → b → b
foldkWithOn = rotateL foldk

{-# INLINE foldkWithFrom #-}
foldkWithFrom ∷ (ToIter a t) ⇒ (a → (b → b) → (b → b)) → b → t → b
foldkWithFrom = flip foldk

{-# INLINE foldr #-}
foldr ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldr i f = foldr𝐼 i f ∘ iter

{-# INLINE foldrFromWith #-}
foldrFromWith ∷ (ToIter a t) ⇒ b → (a → b → b) → t → b
foldrFromWith = foldr

{-# INLINE foldrFromOn #-}
foldrFromOn ∷ (ToIter a t) ⇒ b → t → (a → b → b) → b
foldrFromOn = flip ∘ foldr

{-# INLINE foldrOnFrom #-}
foldrOnFrom ∷ (ToIter a t) ⇒ t → b → (a → b → b) → b
foldrOnFrom = rotateR foldr

{-# INLINE foldrOnWith #-}
foldrOnWith ∷ (ToIter a t) ⇒ t → (a → b → b) → b → b
foldrOnWith = mirror foldr

{-# INLINE foldrWithOn #-}
foldrWithOn ∷ (ToIter a t) ⇒ (a → b → b) → t → b → b
foldrWithOn = rotateL foldr

{-# INLINE foldrWithFrom #-}
foldrWithFrom ∷ (ToIter a t) ⇒ (a → b → b) → b → t → b
foldrWithFrom = flip foldr

{-# INLINE mfold #-}
mfold ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfold i f = fold (return i) (extend ∘ f)

{-# INLINE mfoldFromWith #-}
mfoldFromWith ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldFromWith = mfold

{-# INLINE mfoldFromOn #-}
mfoldFromOn ∷ (Monad m,ToIter a t) ⇒ b → t → (a → b → m b) → m b
mfoldFromOn = flip ∘ mfold

{-# INLINE mfoldOnFrom #-}
mfoldOnFrom ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldOnFrom = rotateR mfold

{-# INLINE mfoldOnWith #-}
mfoldOnWith ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldOnWith = mirror mfold

{-# INLINE mfoldWithOn #-}
mfoldWithOn ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → t → b → m b
mfoldWithOn = rotateL mfold

{-# INLINE mfoldWithFrom #-}
mfoldWithFrom ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → b → t → m b
mfoldWithFrom = flip mfold

{-# INLINE mfoldr #-}
mfoldr ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldr i f = foldr (return i) (extend ∘ f)

{-# INLINE mfoldrFromWith #-}
mfoldrFromWith ∷ (Monad m,ToIter a t) ⇒ b → (a → b → m b) → t → m b
mfoldrFromWith = mfoldr

{-# INLINE mfoldrFromOn #-}
mfoldrFromOn ∷ (Monad m,ToIter a t) ⇒ b → t → (a → b → m b) → m b
mfoldrFromOn = flip ∘ mfoldr

{-# INLINE mfoldrOnFrom #-}
mfoldrOnFrom ∷ (Monad m,ToIter a t) ⇒ t → b → (a → b → m b) → m b
mfoldrOnFrom = rotateR mfoldr

{-# INLINE mfoldrOnWith #-}
mfoldrOnWith ∷ (Monad m,ToIter a t) ⇒ t → (a → b → m b) → b → m b
mfoldrOnWith = mirror mfoldr

{-# INLINE mfoldrWithOn #-}
mfoldrWithOn ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → t → b → m b
mfoldrWithOn = rotateL mfoldr

{-# INLINE mfoldrWithFrom #-}
mfoldrWithFrom ∷ (Monad m,ToIter a t) ⇒ (a → b → m b) → b → t → m b
mfoldrWithFrom = flip mfoldr

{-# INLINE eachWith #-}
eachWith ∷ (Monad m,ToIter a t) ⇒ (a → m ()) → t → m ()
eachWith f = fold skip $ \ x yM → yM ≫ f x

{-# INLINE eachOn #-}
eachOn ∷ (Monad m,ToIter a t) ⇒ t → (a → m ()) → m () 
eachOn = flip eachWith

{-# INLINE exec #-}
exec ∷ (Monad m,ToIter (m ()) t) ⇒ t → m () 
exec = eachWith id

{-# INLINE sum #-}
sum ∷ (ToIter a t,Additive a) ⇒ t → a
sum = fold zero (+)

{-# INLINE product #-}
product ∷ (ToIter a t,Multiplicative a) ⇒ t → a
product = fold one (×)

{-# INLINE concat #-}
concat ∷ (Monoid a,ToIter a t) ⇒ t → a
concat = foldr null (⧺)

{-# INLINE sequence #-}
sequence ∷ (Seqoid a,ToIter a t) ⇒ t → a
sequence = foldr eps (▷)

{-# INLINE compose #-}
compose ∷ (ToIter (a → a) t) ⇒ t → a → a
compose = foldr id (∘)

{-# INLINE mcompose #-}
mcompose ∷ (Monad m) ⇒ (ToIter (a → m a) t) ⇒ t → a → m a
mcompose = foldr return (*∘)

{-# INLINE wcompose #-}
wcompose ∷ (Comonad w) ⇒ (ToIter (w a → a) t) ⇒ t → w a → a
wcompose = foldr extract (%∘)

{-# INLINE joins #-}
joins ∷ (JoinLattice a,ToIter a t) ⇒ t → a
joins = fold bot (⊔)

{-# INLINE meets #-}
meets ∷ (MeetLattice a,ToIter a t) ⇒ t → a
meets = fold top (⊓)

{-# INLINE or #-}
or ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
or = fold False (⩔)

orf ∷ (ToIter (a → 𝔹) t) ⇒ t → a → 𝔹
orf fs x = or $ map (arg x) $ iter fs

andf ∷ (ToIter (a → 𝔹) t) ⇒ t → a → 𝔹
andf fs x = and $ map (arg x) $ iter fs

{-# INLINE and #-}
and ∷ (ToIter 𝔹 t) ⇒ t → 𝔹
and = fold True (⩓)

{-# INLINE count #-}
count ∷ (ToIter a t) ⇒ t → ℕ
count = fold 0 $ const succ

{-# INLINE countWith #-}
countWith ∷ (ToIter a t) ⇒ (a → 𝔹) → t → ℕ
countWith f = fold 0 $ \ x → case f x of
  True → succ
  False → id

{-# INLINE reverse #-}
reverse ∷ (ToIter a t) ⇒ t → 𝐼 a
reverse xs = 𝐼 $ \ (f ∷ a → b → b) (i ∷ b) → foldr i f xs

{-# INLINE repeatI #-}
repeatI ∷ ∀ n a. (Eq n,Zero n,One n,Plus n) ⇒ n → (n → a) → 𝐼 a
repeatI n₀ g = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  let loop ∷ n → b → b
      loop n i
        | n ≡ n₀ = i
        | otherwise = loop (succ n) (f (g n) i)
  in loop zero i₀

{-# INLINE repeat #-}
repeat ∷ ∀ n a. (Eq n,Zero n,One n,Plus n) ⇒ n → a → 𝐼 a
repeat n = repeatI n ∘ const

{-# INLINE build #-}
build ∷ ∀ n a. (Eq n,Zero n,One n,Additive n) ⇒ n → a → (a → a) → 𝐼 a
build n₀ x₀ g = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  let loop ∷ n → a → b → b
      loop n x i
        | n ≡ n₀ = i
        | otherwise = loop (succ n) (g x) (f x i)
  in loop zero x₀ i₀

{-# INLINE upTo #-}
upTo ∷ (Eq n,Zero n,One n,Additive n) ⇒ n → 𝐼 n
upTo n = build n zero succ

{-# INLINE withIndex #-}
withIndex ∷ (ToIter a t) ⇒ t → 𝐼 (ℕ ∧ a)
withIndex xs = 𝐼 $ \ (f ∷ (ℕ ∧ a) → b → b) (i₀ ∷ b) →
  snd $ foldOnFrom xs (0 :* i₀) $ \ (x ∷ a) (n :* i ∷ ℕ ∧ b) → succ n :* f (n :* x) i

{-# INLINE withFirst #-}
withFirst ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withFirst xs = 𝐼 $ \ (f ∷ (𝔹 ∧ a) → b → b) (i₀ ∷ b) →
  snd $ foldOnFrom xs (True :* i₀) $ \ (x ∷ a) (b :* i ∷ 𝔹 ∧ b) → False :* f (b :* x) i

{-# INLINE mapFirst #-}
mapFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapFirst f = map (\ (b :* x) → case b of {True → f x;False → x}) ∘ withFirst

{-# INLINE mapAfterFirst #-}
mapAfterFirst ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapAfterFirst f = map (\ (b :* x) → case b of {True → x;False → f x}) ∘ withFirst

{-# INLINE withLast #-}
withLast ∷ (ToIter a t) ⇒ t → 𝐼 (𝔹 ∧ a)
withLast = reverse ∘ withFirst ∘ reverse

{-# INLINE mapLast #-}
mapLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapLast f = map (\ (b :* x) → case b of {True → f x;False → x}) ∘ withLast

{-# INLINE mapBeforeLast #-}
mapBeforeLast ∷ (ToIter a t) ⇒ (a → a) → t → 𝐼 a
mapBeforeLast f = map (\ (b :* x) → case b of {True → x;False → f x}) ∘ withLast

{-# INLINE filterMap #-}
filterMap ∷ (ToIter a t) ⇒ (a → 𝑂 b) → t → 𝐼 b
filterMap g xs = 𝐼 $ \ (f ∷ b → c → c) (i₀ ∷ c) →
  foldOnFrom xs i₀ $ \ (x ∷ a) →
    case g x of
      None → id
      Some y → f y

{-# INLINE filter #-}
filter ∷ (ToIter a t) ⇒ (a → 𝔹) → t → 𝐼 a
filter f = filterMap $ \ x → case f x of {True → Some x;False → None}

{-# INLINE filterOn #-}
filterOn ∷ (ToIter a t) ⇒ t → (a → 𝔹) → 𝐼 a
filterOn = flip filter

{-# INLINE inbetween #-}
inbetween ∷ (ToIter a t) ⇒ a → t → 𝐼 a
inbetween xⁱ xs = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  foldOnFrom (withFirst xs) i₀ $ \ (b :* x ∷ 𝔹 ∧ a) →
    case b of
      True → f x
      False → f x ∘ f xⁱ

{-# INLINE execN #-}
execN ∷ (Monad m) ⇒ ℕ → m () → m ()
execN n = exec ∘ repeat n

{-# INLINE applyN #-}
applyN ∷ ℕ → b → (b → b) → b
applyN n i f = fold i (const f) $ upTo n

{-# INLINE appendN #-}
appendN ∷ (Monoid a) ⇒ ℕ → a → a 
appendN n x = applyN n null $ (⧺) x

{-# INLINE alignLeftFill #-}
alignLeftFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignLeftFill c n s = build𝕊C $ concat
  [ single𝐼 s
  , single𝐼 $ string $ repeat (n - length𝕊 s ⊓ n) c
  ]

{-# INLINE alignLeft #-}
alignLeft ∷ ℕ → 𝕊 → 𝕊
alignLeft = alignLeftFill ' '

{-# INLINE alignRightFill #-}
alignRightFill ∷ ℂ → ℕ → 𝕊 → 𝕊
alignRightFill c n s = build𝕊C $ concat
  [ single𝐼 $ string $ repeat (n - length𝕊 s ⊓ n) c
  , single𝐼 s
  ]

{-# INLINE alignRight #-}
alignRight ∷ ℕ → 𝕊 → 𝕊
alignRight = alignRightFill ' '

{-# INLINE list #-}
list ∷ (ToIter a t) ⇒ t → 𝐿 a
list = list𝐼 ∘ iter


{-# INLINE lazyList #-}
lazyList ∷ (ToIter a t) ⇒ t → [a]
lazyList = lazyList𝐼 ∘ iter

{-# INLINE string #-}
string ∷ (ToIter ℂ t) ⇒ t → 𝕊
string = build𝕊

{-# INLINE stringC #-}
stringC ∷ (ToIter 𝕊 t) ⇒ t → 𝕊
stringC = build𝕊C

{-# INLINE stringS #-}
stringS ∷ (ToIter ℂ t,Sized t) ⇒ t → 𝕊
stringS ss = build𝕊N (size ss) ss

{-# INLINE stringCS #-}
stringCS ∷ (ToIter 𝕊 t,Sized t) ⇒ t → 𝕊
stringCS ss = build𝕊CN (size ss) ss

{-# INLINE showCollection #-}
showCollection ∷ (ToIter a t) ⇒ 𝕊 → 𝕊 → 𝕊 → (a → 𝕊) → t → 𝕊
showCollection l r i showA xs = concat
  [ l
  , concat $ inbetween i $ map showA $ iter xs
  , r
  ]

{-# INLINE showWith𝐼 #-}
showWith𝐼 ∷ (a → 𝕊) → 𝐼 a → 𝕊
showWith𝐼 = showCollection "𝐼[" "]" ","

{-# INLINE firstMaxByLT #-}
firstMaxByLT ∷ (ToIter a t) ⇒ (a → a → 𝔹) → t → 𝑂 a
firstMaxByLT f = fold None $ \ x xM →
  case xM of
    None → Some x
    Some x' → case f x' x of
      True → Some x
      False → Some x'

{-# INLINE foldbp #-}
foldbp ∷ (ToIter a t) ⇒ b → c → (a → b → b ∧ (c → c)) → t → b ∧ c
foldbp i₀ j₀ f xs = 
  let i :* k = foldFromOn (i₀ :* id) xs $ \ x ((i' ∷ b) :* (k' ∷ c → c)) →
        let i'' :* k'' = f x i'
        in i'' :* (k' ∘ k'')
  in i :* k j₀

{-# INLINE foldbpOnFrom #-}
foldbpOnFrom ∷ (ToIter a t) ⇒ t → b → c → (a → b → b ∧ (c → c)) → b ∧ c
foldbpOnFrom xs i j f = foldbp i j f xs

instance All 𝔹 where 
  {-# INLINE all #-}
  all = iter [True,False]
instance (All a,All b) ⇒ All (a ∨ b) where 
  {-# INLINE all #-}
  all = map Inl (iter all) ⧺ map Inr (iter all)
instance (All a,All b) ⇒ All (a ∧ b) where 
  {-# INLINE all #-}
  all = do x ← iter all ; y ← iter all ; return $ x :* y

sortWith ∷ (ToIter a t) ⇒ (a → a → Ordering) → t → 𝐿 a
sortWith f = list ∘ HS.sortBy f ∘ lazyList

sortOn ∷ (ToIter a t,Ord b) ⇒ (a → b) → t → 𝐿 a
sortOn f = sortWith $ (⋚) `on` f

sort ∷ (ToIter a t,Ord a) ⇒ t → 𝐿 a
sort = sortWith (⋚)

materialize ∷ (ToIter a t) ⇒ t → 𝐼 a
materialize = iter ∘ list

--------
-- 𝐼C --
--------

data 𝐼C a = 𝐼C
  { 𝑖cCount ∷ ℕ64
  , 𝑖cIter ∷ 𝐼 a
  } deriving (Show)

instance Null   (𝐼C a) where null                  = 𝐼C zero null
instance Append (𝐼C a) where 𝐼C c₁ xs₁ ⧺ 𝐼C c₂ xs₂ = 𝐼C (c₁ + c₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼C a)

instance ToIter a (𝐼C a) where iter   = 𝑖cIter
instance Single a (𝐼C a) where single = 𝐼C one ∘ single
instance Sized    (𝐼C a) where size   = 𝑖cCount

instance Functor 𝐼C where map f (𝐼C c xs) = 𝐼C c $ map f xs

iterC ∷ (ToIter a t,Sized t) ⇒ t → 𝐼C a
iterC xs = 𝐼C (size xs) $ iter xs

---------
-- 𝐼A --
---------

data 𝐼A a = 𝐼A
  { 𝑖ASize ∷ ℕ64
  , 𝑖AIter ∷ 𝐼 a
  } deriving (Show)

instance Null   (𝐼A a) where null                  = 𝐼A zero null
instance Append (𝐼A a) where 𝐼A s₁ xs₁ ⧺ 𝐼A s₂ xs₂ = 𝐼A (s₁ + s₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼A a)

instance             ToIter a (𝐼A a) where iter     = 𝑖AIter
instance (Sized a) ⇒ Single a (𝐼A a) where single s = 𝐼A (size s) $ single s
instance             Sized    (𝐼A a) where size     = 𝑖ASize

iterA ∷ (ToIter a t,Sized a) ⇒ t → 𝐼A a
iterA xs = 𝐼A (sum $ map size $ iter xs) $ iter xs

---------
-- 𝐼CA --
---------

data 𝐼CA a = 𝐼CA
  { 𝑖CACount ∷ ℕ64
  , 𝑖CASize ∷ ℕ64
  , 𝑖CAIter ∷ 𝐼 a
  } deriving (Show)

instance Null   (𝐼CA a) where null                          = 𝐼CA zero zero null
instance Append (𝐼CA a) where 𝐼CA c₁ s₁ xs₁ ⧺ 𝐼CA c₂ s₂ xs₂ = 𝐼CA (c₁ + c₂) (s₁ + s₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼CA a)

instance             ToIter a (𝐼CA a) where iter     = 𝑖CAIter
instance (Sized a) ⇒ Single a (𝐼CA a) where single s = 𝐼CA one (size s) $ single s

iterCA ∷ (ToIter a t,Sized a,Sized t) ⇒ t → 𝐼CA a
iterCA xs = 𝐼CA (size xs) (sum $ map size $ iter xs) $ iter xs
