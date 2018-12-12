module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserInput

-----------------------------------
-- Fully Functor/Comonad general --
-----------------------------------

data MixesF t f a = MixesF
  { mixesFPrefix  ∷ Parser t (f a → a)
  , mixesFPostfix ∷ Parser t (f a → a)
  , mixesFInfix  ∷ Parser t (f a → f a → a)
  , mixesFInfixL ∷ Parser t (f a → f a → a)
  , mixesFInfixR ∷ Parser t (f a → f a → a)
  }

instance Null (MixesF t f a) where null = MixesF abort abort abort abort abort
instance Append (MixesF t f a) where
  MixesF pre₁ post₁ inf₁ infl₁ infr₁ ⧺ MixesF pre₂ post₂ inf₂ infl₂ infr₂ = 
    MixesF (pre₁ ⎅ pre₂) (post₁ ⎅ post₂) (inf₁ ⎅ inf₂) (infl₁ ⎅ infl₂) (infr₁ ⎅ infr₂)
instance Monoid (MixesF t f a)

data MixF t f a =
    MixFPrefix  ℕ (Parser t (f a → a))
  | MixFPostfix ℕ (Parser t (f a → a))
  | MixFInfix  ℕ (Parser t (f a → f a → a))
  | MixFInfixL ℕ (Parser t (f a → f a → a))
  | MixFInfixR ℕ (Parser t (f a → f a → a))
  | MixFTerminal (Parser t a)

data MixfixF t f a = MixfixF
  { mixfixFTerminals ∷ Parser t a
  , mixfixFLevels ∷ ℕ ⇰ MixesF t f a
  }
instance Null (MixfixF t f a) where null = MixfixF abort bot
instance Append (MixfixF t f a) where MixfixF ts₁ ls₁ ⧺ MixfixF ts₂ ls₂ = MixfixF (ts₁ ⎅ ts₂) (ls₁ ⧺ ls₂)
instance Monoid (MixfixF t f a)

mixF ∷ MixF t f a → MixfixF t f a
mixF (MixFPrefix l pre) = null {mixfixFLevels = dict [l ↦ null {mixesFPrefix = pre}]}
mixF (MixFPostfix l post) = null {mixfixFLevels = dict [l ↦ null {mixesFPostfix = post}]}
mixF (MixFInfix l inf) = null {mixfixFLevels = dict [l ↦ null {mixesFInfix = inf}]}
mixF (MixFInfixL l infl) = null {mixfixFLevels = dict [l ↦ null {mixesFInfixL = infl}]}
mixF (MixFInfixR l infr) = null {mixfixFLevels = dict [l ↦ null {mixesFInfixR = infr}]}
mixF (MixFTerminal term) = null {mixfixFTerminals = term}

-- PRE (PRE (x INFR (PRE (PRE y))))
-- PRE PRE x INFR PRE PRE y
-- 
-- ((((x POST) POST) INFL y) POST) POST
-- x POST POST INFL y POST POST

mixfixParserF ∷ 
  ∀ t f a. (Comonad f)
  ⇒ MixfixF t f a → (Parser t a → Parser t (f a)) → Parser t (f a)
mixfixParserF (MixfixF terms levels₀) fld = loop levels₀
  where
    loop ∷ ℕ ⇰ MixesF t f a → Parser t (f a)
    loop levels = case dmin levels of
      None → fld terms
      Some ((i :* mixes) :* levels') →
        let msg = "lvl " ⧺ alignRightFill '0' 3 (pprender i)
        in 
        fld $ buildLevelDirected msg mixes $ 
        fld $ buildLevelNondirected msg mixes $ 
        loop levels'
    buildLevelNondirected ∷ 𝕊 → MixesF t f a → Parser t (f a) → Parser t a
    buildLevelNondirected msg mixes nextLevel = do
      x ← nextLevel
      tries
        [ pErr (msg ⧺ " infix") $ levelInfAfterOne x nextLevel mixes
        , return $ extract x
        ]
    buildLevelDirected ∷ 𝕊 → MixesF t f a → Parser t (f a) → Parser t a
    buildLevelDirected msg mixes nextLevel = tries
      [ do
          x ← nextLevel
          tries
            [ pErr (msg ⧺ " infixl") $ levelInflAfterOne x nextLevel mixes
            , pErr (msg ⧺ " infixr") $ levelInfrAfterOne x nextLevel mixes
            , return $ extract x
            ]
      , pErr (msg ⧺ " infixr") $ levelInfrNotAfterOne nextLevel mixes
      ]
    levelInflAfterOne ∷ f a → Parser t (f a) → MixesF t f a → Parser t a
    levelInflAfterOne x nextLevel mixes = do
      fxs ← pOneOrMore $ tries
        [ mixesFPostfix mixes
        , do
            f ← mixesFInfixL mixes
            x₂ ← nextLevel
            return $ \ x₁ → f x₁ x₂
        ]
      return $ wcompose (reverse fxs) x
    _levelInfr ∷ Parser t (f a) → MixesF t f a → Parser t a
    _levelInfr nextLevel mixes = do
      fxs ← pOneOrMore $ tries
        [ mixesFPrefix mixes
        , do
            x₁ ← nextLevel
            f ← mixesFInfixR mixes
            return $ \ x₂ → f x₁ x₂
        ]
      x ← nextLevel
      return $ wcompose fxs x
    levelInfrAfterOne ∷ f a → Parser t (f a) → MixesF t f a → Parser t a
    levelInfrAfterOne x₁ nextLevel mixes = do
      f ← mixesFInfixR mixes
      levelInfrAfterOneCombo (\ x₂ → f x₁ x₂) nextLevel mixes
    levelInfrNotAfterOne ∷ Parser t (f a) → MixesF t f a → Parser t a
    levelInfrNotAfterOne nextLevel mixes = do
      f ← mixesFPrefix mixes
      levelInfrAfterOneCombo f nextLevel mixes
    levelInfrAfterOneCombo ∷ (f a → a) → Parser t (f a) → MixesF t f a → Parser t a
    levelInfrAfterOneCombo f nextLevel mixes = do
      fxs ∷ 𝐿 (f a → a) ← pMany $ tries
        [ mixesFPrefix mixes
        , do
            x₁ ← nextLevel
            f' ← mixesFInfixR mixes
            return $ \ x₂ → f' x₁ x₂
        ]
      x₂ ← nextLevel
      return $ wcompose (f:&fxs) x₂
    levelInfAfterOne ∷ f a → Parser t (f a) → MixesF t f a → Parser t a
    levelInfAfterOne x₁ nextLevel mixes = do
      f ← mixesFInfix mixes
      x₂ ← nextLevel
      return $ f x₁ x₂

---------------
-- Non-fancy --
---------------

data Mixes t a = Mixes
  { mixesPrefix  ∷ Parser t (a → a)
  , mixesPostfix ∷ Parser t (a → a)
  , mixesInfix  ∷ Parser t (a → a → a)
  , mixesInfixL ∷ Parser t (a → a → a)
  , mixesInfixR ∷ Parser t (a → a → a)
  }

instance Null (Mixes t a) where null = Mixes abort abort abort abort abort
instance Append (Mixes t a) where 
  Mixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ Mixes pre₂ post₂ inf₂ infl₂ infr₂ = 
    Mixes (pre₁ ⎅ pre₂) (post₁ ⎅ post₂) (inf₁ ⎅ inf₂) (infl₁ ⎅ infl₂) (infr₁ ⎅ infr₂)
instance Monoid (Mixes t a)

mixesPure ∷ Mixes t a → MixesF t ID a
mixesPure (Mixes pre post inf infl infr) =
  MixesF
  (map kextract pre)
  (map kextract post)
  (map kextract2 inf)
  (map kextract2 infl)
  (map kextract2 infr)

data Mixfix t a = Mixfix 
  { mixfixTerminals ∷ Parser t a
  , mixfixLevels ∷ ℕ ⇰ Mixes t a
  }

instance Null (Mixfix t a) where null = Mixfix abort bot
instance Append (Mixfix t a) where Mixfix ts₁ ls₁ ⧺ Mixfix ts₂ ls₂ = Mixfix (ts₁ ⎅ ts₂) (ls₁ ⧺ ls₂)
instance Monoid (Mixfix t a)

data Mix t a =
    MixPrefix  ℕ (Parser t (a → a))
  | MixPostfix ℕ (Parser t (a → a))
  | MixInfix  ℕ (Parser t (a → a → a))
  | MixInfixL ℕ (Parser t (a → a → a))
  | MixInfixR ℕ (Parser t (a → a → a))
  | MixTerminal (Parser t a)

mix ∷ Mix t a → Mixfix t a
mix (MixPrefix l pre) = null {mixfixLevels = dict [l ↦ null {mixesPrefix = pre}]}
mix (MixPostfix l post) = null {mixfixLevels = dict [l ↦ null {mixesPostfix = post}]}
mix (MixInfix l inf) = null {mixfixLevels = dict [l ↦ null {mixesInfix = inf}]}
mix (MixInfixL l infl) = null {mixfixLevels = dict [l ↦ null {mixesInfixL = infl}]}
mix (MixInfixR l infr) = null {mixfixLevels = dict [l ↦ null {mixesInfixR = infr}]}
mix (MixTerminal term) = null {mixfixTerminals = term}

mixfixPure ∷ Mixfix t a → MixfixF t ID a
mixfixPure (Mixfix terminals levels) = MixfixF terminals $ map mixesPure levels

mixfixParser ∷ Mixfix t a → Parser t a
mixfixParser mixfix = unID ^$ mixfixParserF (mixfixPure mixfix) (map ID)

mixfixParserWithContext ∷ 𝕊 → MixfixF t (Annotated FullContext) a → Parser t (Annotated FullContext a)
mixfixParserWithContext s mixfix = mixfixParserF mixfix $ pWithContext s
