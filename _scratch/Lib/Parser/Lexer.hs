module UVMHS.Lib.Parser.Lexer where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.IterS

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError




{-
class Classified c t | t → c where classify ∷ t → c

data CharClass = Space | Letter | Number

data LexerState c t s = LexerState
  { lexerStateCharTable ∷ t ⇰ s ⇰ s
  , lexerStateClassTable ∷ c ⇰ s ⇰ s
  , lexerStateAccept ∷ 𝑂 (ℕ64 ∧ Formats)
  , lexerStateHopeless ∷ 𝔹
  }

data Lexer c t where
  NLexer ∷ ∀ c t. Lexer c t
  SLexer ∷ ∀ c t s. (Ord s) ⇒ LexerState c t s → Lexer c t

instance Null (Lexer c t) where
  null = NLexer
instance (Ord c,Ord t,Classified c t) ⇒ Append (Lexer c t) where
  NLexer ⧺ l = l
  l ⧺ NLexer = l
  SLexer (LexerState cht₁ clt₁ a₁ h₁) ⧺ SLexer (LexerState cht₂ clt₂ a₂ h₂) =
    let cht₁Keys = keys cht₁
        cht₂Keys = keys cht₂
        cht₁Only = cht₁Keys ∖ cht₂Keys
        cht₂Only = cht₂Keys ∖ cht₁Keys
        cht₁' = cht₁ ⩌ assoc (map (\ t → t :* (clt₁ ⋕! classify t)) $ iter cht₂Only)
        cht₂' = cht₂ ⩌ assoc (map (\ t → t :* (clt₂ ⋕! classify t)) $ iter cht₁Only)
        ctCombine tb₁ tb₂ = assoc $ do
          s₁ :* s₁' ← iter tb₁
          s₂ :* s₂' ← iter tb₂
          return $ (s₁ :* s₂) :* (s₁' :* s₂')
        cht = interWith ctCombine cht₁' cht₂'
        clt = interWith ctCombine clt₁ clt₂
        a = case (a₁,a₂) of
          (None,None) → None
          (None,Some nfm) → Some nfm
          (Some nfm,None) → Some nfm
          (Some (n₁ :* fm₁),Some (n₂ :* fm₂)) 
            | n₁ ≥ n₂ → Some (n₁ :* fm₁)
            | otherwise → Some (n₂ :* fm₂)
        h = h₁ ⩓ h₂
    in SLexer $ LexerState cht clt a h

-}

----------------
-- LexerState --
----------------

data LexerState t = LexerState
  { lexerStateContext ∷ ParserContext
  , lexerStateSuffix ∷ WindowL Doc Doc
  , lexerStateInput ∷ ParserInput t
  }
makeLenses ''LexerState
makePrettyRecord ''LexerState

lexerState₀ ∷ ParserInput t → LexerState t
lexerState₀ = LexerState null null

------------
-- LexerM --
------------

type LexerM t = StateT (LexerState t) 𝑂

runLexer ∷ LexerState t → LexerM t a → 𝑂 (LexerState t ∧ a)
runLexer = runStateT

lPluck ∷ LexerM t (ParserToken t)
lPluck = do
  pi ← getL lexerStateInputL
  ParserToken x sk tc ts :* pi' ← abort𝑂 $ advanceInput pi
  putL lexerStateInputL pi'
  modifyL lexerStateContextL $ \ pc → pc ⧺ tc
  if sk
    then do
      lPluck
    else do
      putL lexerStateSuffixL ts
      return $ ParserToken x sk tc ts

lShaped ∷ (t → 𝑂 a) → LexerM t a
lShaped f = do
  t ← lPluck
  abort𝑂 $ f $ parserTokenValue t

-----------
-- Lexer --
-----------

data LResult t a = LResult
  { lresultLevel ∷ ℕ64
  , lresultEntries ∷ Formats ⇰ LexerM t a
  }

mapLResultEntries ∷ (LexerM t a → LexerM u b) → LResult t a → LResult u b
mapLResultEntries f (LResult n ls) = LResult n $ map f ls

data Lexer t a = Lexer
  { lexerNext ∷ t ⇰ Lexer t a
  , lexerResults ∷ LResult t a
  }

instance Null (LResult t a) where
  null = LResult zero dø
instance Append (LResult t a) where
  r₁@(LResult n₁ ls₁) ⧺ r₂@(LResult n₂ ls₂)
    | n₁ > n₂ = r₁
    | n₁ < n₂ = r₂
    | otherwise = LResult n₁ $ unionWith (⎅) ls₁ ls₂
instance Monoid (LResult t a)

instance Functor (LResult t) where
  map f (LResult n ls) = LResult n $ mapp f ls

instance (Ord t) ⇒ MonadFail (Lexer t) where
  abort = Lexer dø null
  Lexer n₁ c₁ ⎅ Lexer n₂ c₂ = Lexer (unionWith (⎅) n₁ n₂) $ c₁ ⧺ c₂

instance Functor (Lexer t) where
  map ∷ ∀ a b. (a → b) → Lexer t a → Lexer t b
  map f (Lexer n c) = Lexer (mapp f n) $ map f c

instance Return (Lexer t) where 
  return ∷ ∀ a. a → Lexer t a
  return x = Lexer dø $ LResult zero $ null ↦ return x

instance (Ord t) ⇒ Bind (Lexer t) where
  (≫=) ∷ ∀ a b. Lexer t a → (a → Lexer t b) → Lexer t b
  Lexer n c ≫= k = Lexer (map (extend k) n) $ mapLResultEntries (extend $ frLexer ∘ k) c

instance (Ord t) ⇒ Monad (Lexer t)

toLexer ∷ ∀ t a. (Ord t) ⇒ ℕ64 → LexerM t a → Lexer t a
toLexer n l = Lexer dø $ LResult n $ null ↦ l

frLexer ∷ ∀ t a. (Ord t) ⇒ Lexer t a → LexerM t a
frLexer l₀ = loop l₀
  where
    loop ∷ Lexer t a → LexerM t a
    loop l = tries
      [ do t ← lPluck
           l' ← abort𝑂 $ lexerNext l ⋕? parserTokenValue t
           loop l'
      , tries $ mapOn (iter $ lresultEntries $ lexerResults l) $ \ (fmt :* lC) → do
          do modifyL lexerStateContextL $ formatParserContext fmt
             lC
      ]

tokenize ∷ ∀ t a. (Ord t) ⇒ Lexer t (𝔹 ∧ a) → 𝑆 (ParserToken t) → Doc ∨ 𝑆 (ParserToken a)
tokenize l ts = mapInr (stream ∘ vecS ∘ fst) $ loop null $ lexerState₀ $ parserInput₀ ts
  where
    loop ∷ WindowR Doc Doc → LexerState t → Doc ∨ (𝐼S (ParserToken a) ∧ WindowL Doc Doc)
    loop pp σ = case advanceInput $ lexerStateInput σ of
      None → return $ null :* null
      Some (t :* _) → case runLexer σ $ frLexer l of
        None → do
          let tc = parserTokenContext t
              le = map locRangeEnd $ parserContextLocRange tc
              d = parserContextError tc
              s = parserTokenSuffix t
          throw $ displaySourceError $ AddNull $ ParserError le d s $ single $ ParserErrorInfo pp null "<token>" null
        Some (σ' :* (sk :* x)) → do
          let pc = lexerStateContext σ'
          ts' :* ps' ← loop (pp ⧺ parserContextDisplayR pc) σ'
          let t' = ParserToken x sk pc ps'
          return $ (ts' ⧺ single t') :* (parserContextDisplayL pc ⧺ ps')

lunit ∷ ℕ64 → Formats → a → Lexer t a
lunit n fmt x = Lexer dø $ LResult n $ fmt ↦ return x

-- lOneThen ∷ (Ord t) ⇒ t → Lexer t a → Lexer t a
-- lOneThen t l = Lexer (t ↦ l) Null Null
-- 
-- lSatisfyThen ∷ (t → 𝔹) → (() → Lexer t a ) → Lexer t a
-- lSatisfyThen f l = Lexer dø (AddNull $ \ t → if f t then l () else null) Null

lword ∷ (Ord t,Eq t) ⇒ ℕ64 → Formats → (s → 𝐼 t) → s → Lexer t s
lword n fm to ts = foldrOnFrom (to ts) (lunit n fm ts) $ \ c cp → Lexer (c ↦ cp) null

lsat ∷ (Ord t) ⇒ ℕ64 → Formats → (t → 𝔹) → Lexer t t
lsat n fm f = toLexer n $ do
  t ← lShaped $ \ x → if f x then Some x else None
  modifyL lexerStateContextL $ formatParserContext fm
  return t

-- lMany ∷ (Ord t) ⇒ ℕ64 → Formats → 𝔹 → Lexer t a → b → (a → b → b) → Lexer t b
-- lMany n fm sk l i f =
--   let ~l' = map (const i) (lUnit n fm sk) ⧺ (map (\ (x :* xs) → f x xs) (l ⨟ l'))
--   in l'

-- lWhitespace ∷ ℕ64 → Lexer ℂ 𝕊
-- lWhitespace n = 
--   let l = (lSatisfyThen isSpace $ \ () → l) ⧺ lUnit n null True stringS
--   in lSatisfyThen isSpace $ const l

lWhitespace ∷ ℕ64 → Lexer ℂ 𝕊
lWhitespace n = string ^$ oneOrMore $ lsat n null isSpace

-- lName ∷ ℕ64 → Lexer ℂ 𝕊
-- lName n = toLexer n $ do
--   c ← lsat n isLetter
--   cs ← many $ lsat $ \ c → joins
--     [ isLetter c 
--     , isNumber c 
--     , c ∈ pow "_-'′"
--     ]
--   return $ string $ c :&  cs

-- lComment ∷ Parser ℂ 𝕊
-- lComment = do -- pNewContext "comment" $ do
--   s₁ ← pWord "--"
--   s₂ ← string ^$ pMany $ pSatisfies {- "not newline" -} $ \ c → c ≢ '\n'
--   s₃ ← single ^$ pLit '\n'
--   return $ s₁ ⧺ s₂ ⧺ s₃

-- lComment ∷ ℕ64 → Lexer ℂ 𝕊
-- lComment n = 
--   let nl = lWord n null iter stringS "\n"
--   in undefined







































-- instance (Null a) ⇒ Null (Lazy a) where
--   null = Lazy null
-- instance (Append a) ⇒ Append (Lazy a) where
--   ~(Lazy x) ⧺ ~(Lazy y) = Lazy (x ⧺ y)
-- 
-- instance Functor Lazy where
--   map f ~(Lazy x) = Lazy (f x)
-- 
-- class Sequence t where (⨟) ∷ t a → t b → t (a ∧ b)
-- 
-- -----------------
-- -- LexerResult --
-- -----------------
-- 
-- data LexerResult t a = LexerResult
--   { lexerResultLevel ∷ ℕ64
--   , lexerResultFormat ∷ Formats
--   , lexerResultSkip ∷ 𝔹
--   , lexerResultBuilder ∷ 𝐼S t → a
--   }
-- 
-- instance Append (LexerResult t a) where
--   lr₁ ⧺ lr₂
--     | lexerResultLevel lr₁ ≥ lexerResultLevel lr₂ = lr₁
--     | otherwise = lr₂
-- 
-- instance Functor (LexerResult t) where
--   map ∷ ∀ a b. (a → b) → LexerResult t a → LexerResult t b
--   map f (LexerResult n fm sk g) = LexerResult n fm sk $ f ∘ g
-- 
-- instance Sequence (LexerResult t) where
--   (⨟) ∷ ∀ a b. LexerResult t a → LexerResult t b → LexerResult t (a ∧ b)
--   LexerResult n₁ f₁ sk₁ g₁ ⨟ LexerResult n₂ f₂ sk₂ g₂ =
--     LexerResult (n₁ ⊓ n₂) (f₁ ⧺ f₂) (sk₁ ⩓ sk₂) $ \ ts → g₁ ts :* g₂ ts 
-- 
-- data Lexer t a = Lexer
--   { lexerNext ∷ t ⇰ Lazy (Lexer t a)
--   , lexerFallback ∷ AddNull (t → Lexer t a)
--   , lexerResult ∷ AddNull (LexerResult t a)
--   }
-- 
-- instance Null (Lexer t a) where
--   null ∷ Lexer t a
--   null = Lexer dø Null null
-- instance (Ord t) ⇒ Append (Lexer t a) where
--   (⧺) ∷ Lexer t a → Lexer t a → Lexer t a
--   Lexer n₁ fO₁ r₁ ⧺ Lexer n₂ fO₂ r₂ =
--     let nBoth = interWith (⧺) n₁ n₂
--         n₁Only = without (keys n₂) n₁
--         n₂Only = without (keys n₁) n₂
--         n₁Extra = case fO₂ of
--           Null → id
--           AddNull f₂ → mapK𝐷 $ \ t l → l ⧺ Lazy (f₂ t)
--         n₂Extra = case fO₁ of
--           Null → id
--           AddNull f₁ → mapK𝐷 $ \ t l → l ⧺ Lazy (f₁ t)
--         fBoth = fO₁ ⧺ fO₂
--     in Lexer (unionsWith (⧺) [nBoth,n₁Extra n₁Only,n₂Extra n₂Only]) fBoth $ r₁ ⧺ r₂
-- instance (Ord t) ⇒ Monoid (Lexer t a)
-- 
-- instance Functor (Lexer t) where
--   map ∷ ∀ a b. (a → b) → Lexer t a → Lexer t b
--   map f (Lexer n fO r) = Lexer (mapp (map f) n) (mapp (map f) fO) (map (map f) r)
-- 
-- instance (Ord t) ⇒ Sequence (Lexer t) where
--   (⨟) ∷ ∀ a b. Lexer t a → Lexer t b → Lexer t (a ∧ b)
--   Lexer n₁ fO₁ rO₁ ⨟ l@(Lexer n₂ fO₂ rO₂) =
--     let n₁' = mapp (\ l' → l' ⨟ l) n₁
--         fO₁' = mapp (\ l' → l' ⨟ l) fO₁
--         lₓ = Lexer dø Null rO₁
--         n₂' = mapp (\ l' → lₓ ⨟ l') n₂
--         fO₂' = mapp (\ l' → lₓ ⨟ l') fO₂
--         rO' = case (rO₁,rO₂) of
--           (AddNull r₁,AddNull r₂) → AddNull $ r₁ ⨟ r₂
--           _ → Null
--     in Lexer (n₁' ⧺ n₂') (fO₁' ⧺ fO₂') rO'
--   
-- 
-- ----------------
-- -- LexerState --
-- ----------------
-- 
-- data LexerState t = LexerState
--   { lexerStateContext ∷ ParserContext
--   , lexerStateSuffix ∷ WindowL Doc Doc
--   , lexerStateInput ∷ ParserInput t
--   }
-- makeLenses ''LexerState
-- makePrettyRecord ''LexerState
-- 
-- lexerState₀ ∷ ParserInput t → LexerState t
-- lexerState₀ = LexerState null null
-- 
-- type LexerM t = StateT (LexerState t) 𝑂
-- 
-- runLexer ∷ LexerState t → LexerM t a → 𝑂 (LexerState t ∧ a)
-- runLexer = runStateT
-- 
-- lAdvance ∷ LexerM t (ParserToken t)
-- lAdvance = do
--   pi ← getL lexerStateInputL
--   ParserToken x sk tc ts :* pi' ← abort𝑂 $ advanceInput pi
--   putL lexerStateInputL pi'
--   if sk
--     then do
--       modifyL lexerStateContextL $ \ pc → pc ⧺ tc
--       lAdvance
--     else do
--       return $ ParserToken x sk tc ts
-- 
-- lRecord ∷ ParserToken t → LexerM t ()
-- lRecord t = do
--   modifyL lexerStateContextL $ \ pc → pc ⧺ parserTokenContext t
--   putL lexerStateSuffixL $ parserTokenSuffix t
-- 
-- interpLexer ∷ ∀ t a. (Ord t) ⇒ Lexer t a → LexerM t (𝔹 ∧ a)
-- interpLexer l₀ = loop null l₀
--   where
--     loop ∷ 𝐼S t → Lexer t a → LexerM t (𝔹 ∧ a)
--     loop ts l = tries
--       [ do t ← lAdvance
--            let x = parserTokenValue t
--            lRecord t
--            l' ← abort𝑂 $ tries
--              [ unLazy ^$ lexerNext l ⋕? x
--              , elimAddNull None Some $ mapOn (lexerFallback l) $ \ f → f x
--              ]
--            loop (concat [ts,single $ parserTokenValue t]) l'
--       , do LexerResult _ fm sk f ← abort𝑂 $ elimAddNull None Some $ lexerResult l
--            modifyL lexerStateContextL $ formatParserContext fm
--            return $ sk :* f ts
--       ]
-- 
-- 
-- tokenize ∷ ∀ t a. (Ord t) ⇒ Lexer t a → 𝑆 (ParserToken t) → Doc ∨ 𝑆 (ParserToken a)
-- tokenize l ts = mapInr (stream ∘ vecS ∘ fst) $ loop null $ lexerState₀ $ parserInput₀ ts
--   where
--     loop ∷ WindowR Doc Doc → LexerState t → Doc ∨ (𝐼S (ParserToken a) ∧ WindowL Doc Doc)
--     loop pp σ = case advanceInput $ lexerStateInput σ of
--       None → return $ null :* null
--       Some (t :* _) → case runLexer σ $ interpLexer l of
--         None → do
--           let tc = parserTokenContext t
--               le = map locRangeEnd $ parserContextLocRange tc
--               d = parserContextError tc
--               s = parserTokenSuffix t
--           throw $ displaySourceError $ AddNull $ ParserError le d s $ single $ ParserErrorInfo pp null "<token>" null
--         Some (σ' :* (sk :* x)) → do
--           let pc = lexerStateContext σ'
--           ts' :* ps' ← loop (pp ⧺ parserContextDisplayR pc) σ'
--           let t' = ParserToken x sk pc ps'
--           return $ (ts' ⧺ single t') :* (parserContextDisplayL pc ⧺ ps')
-- 
-- lUnit ∷ ℕ64 → Formats → 𝔹 → (𝐼S t → a) → Lexer t a
-- lUnit n fm sk f = Lexer dø Null $ AddNull $ LexerResult n fm sk f
-- 
-- lOneThen ∷ (Ord t) ⇒ t → Lazy (Lexer t a) → Lexer t a
-- lOneThen t l = Lexer (t ↦ l) Null Null
-- 
-- lSatisfyThen ∷ (t → 𝔹) → (() → Lexer t a ) → Lexer t a
-- lSatisfyThen f l = Lexer dø (AddNull $ \ t → if f t then l () else null) Null
-- 
-- lWord ∷ (Ord t,Eq t) ⇒ ℕ64 → Formats → (s → 𝐼 t) → (𝐼S t → s) → s → Lexer t s
-- lWord n fm to fr ts = foldrOnFrom (to ts) (lUnit n fm False fr) $ \ c cp → lOneThen c $ Lazy cp
-- 
-- -- lSatisfies ∷ ℕ64 → Formats → (t → 𝔹) → Lexer t (𝐼S t)
-- -- lSatisfies n fm f = Lexer dø (AddNull $ \ x → if f x then lUnit n fm False else null) Null
-- 
-- -- lMany ∷ (Ord t) ⇒ ℕ64 → Formats → 𝔹 → Lexer t a → b → (a → b → b) → Lexer t b
-- -- lMany n fm sk l i f =
-- --   let ~l' = map (const i) (lUnit n fm sk) ⧺ (map (\ (x :* xs) → f x xs) (l ⨟ l'))
-- --   in l'
-- 
-- lWhitespace ∷ ℕ64 → Lexer ℂ 𝕊
-- lWhitespace n = 
--   let l = (lSatisfyThen isSpace $ \ () → l) ⧺ lUnit n null True stringS
--   in lSatisfyThen isSpace $ const l
-- 
-- lName ∷ ℕ64 → Lexer ℂ 𝕊
-- lName n =
--   let l = (lSatisfyThen (\ c → joins [isLetter c,isNumber c,c ∈ pow "_-'′"]) $ \ () → l) ⧺ lUnit n null False stringS
--   in lLexer dø (AddNull $ \ c → if isLetter c then l else null) Null
-- 
-- lComment ∷ ℕ64 → Lexer ℂ 𝕊
-- lComment n = 
--   let nl = lWord n null iter stringS "\n"
--   in undefined













-- LOH -- Make it look like old "fast" parser where when done you get a LexerM
-- not just an a ; get that working and benchmarked first.

-- 
-- 
-- 
-- 
-- -- import UVMHS.Core
-- -- 
-- -- import UVMHS.Lib.Pretty
-- -- 
-- -- import UVMHS.Lib.Parser.ParserContext
-- -- import UVMHS.Lib.Parser.ParserInput
-- -- 
-- -- ----------------
-- -- -- LexerState --
-- -- ----------------
-- -- 
-- -- data LexerState t = LexerState
-- --   { lexerStateContext ∷ ParserContext
-- --   , lexerStateSuffix ∷ WindowL Doc Doc
-- --   , lexerStateInput ∷ ParserInput t
-- --   }
-- -- makeLenses ''LexerState
-- -- makePrettyRecord ''LexerState
-- -- 
-- -- lexerState₀ ∷ ParserInput t → LexerState t
-- -- lexerState₀ = LexerState null
-- -- 
-- -- -----------
-- -- -- Lexer --
-- -- -----------
-- -- 
-- -- newtype Lexer t a = Lexer { unLexer ∷ StateT (LexerState t) 𝑂 a }
-- --   deriving 
-- --   ( Functor,Return,Bind,Monad
-- --   , MonadFail
-- --   , MonadState (LexerState t)
-- --   )
-- -- 
-- -- lAdvance ∷ Lexer t (AddBot Loc ∨ ParserToken t)
-- -- lAdvance = do
-- --   pi ← getL parserStateInputL
-- --   case advanceInput pi of
-- --     None → return $ Inl $ parserInputEndPos pi
-- --     Some (ParserToken x tc ts :* pi') → do
-- --       putL parserStateInputL pi'
-- --       pk ← getL parserStateSkipContextL
-- --       pc ← getL parserStateContextL
-- --       return $ Inr $ ParserToken x False (formatParserContext fmt tc) ts
-- -- import UVMHS.Core
-- -- 
-- -- import UVMHS.Lib.Pretty
-- -- 
-- -- import UVMHS.Lib.Parser.ParserContext
-- -- import UVMHS.Lib.Parser.ParserInput
-- -- 
-- -- ----------------
-- -- -- LexerState --
-- -- ----------------
-- -- 
-- -- data LexerState t = LexerState
-- --   { lexerStateContext ∷ ParserContext
-- --   , lexerStateSuffix ∷ WindowL Doc Doc
-- --   , lexerStateInput ∷ ParserInput t
-- --   }
-- -- makeLenses ''LexerState
-- -- makePrettyRecord ''LexerState
-- -- 
-- -- lexerState₀ ∷ ParserInput t → LexerState t
-- -- lexerState₀ = LexerState null
-- -- 
-- -- -----------
-- -- -- Lexer --
-- -- -----------
-- -- 
-- -- newtype Lexer t a = Lexer { unLexer ∷ StateT (LexerState t) 𝑂 a }
-- --   deriving 
-- --   ( Functor,Return,Bind,Monad
-- --   , MonadFail
-- --   , MonadState (LexerState t)
-- --   )
-- -- 
-- -- lAdvance ∷ Lexer t (AddBot Loc ∨ ParserToken t)
-- -- lAdvance = do
-- --   pi ← getL parserStateInputL
-- --   case advanceInput pi of
-- --     None → return $ Inl $ parserInputEndPos pi
-- --     Some (ParserToken x tc ts :* pi') → do
-- --       putL parserStateInputL pi'
-- --       pk ← getL parserStateSkipContextL
-- --       pc ← getL parserStateContextL
-- --       return $ Inr $ ParserToken x False (formatParserContext fmt tc) ts
