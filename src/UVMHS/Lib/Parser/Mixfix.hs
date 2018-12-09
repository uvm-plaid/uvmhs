module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Core

-----------------------------------
-- Fully Functor/Comonad general --
-----------------------------------

data MixesF t f a = MixesF
  { mixesFPre  ∷ Parser t (f a → a)
  , mixesFPost ∷ Parser t (f a → a)
  , mixesFInf  ∷ Parser t (f a → f a → a)
  , mixesFInfl ∷ Parser t (f a → f a → a)
  , mixesFInfr ∷ Parser t (f a → f a → a)
  }

instance Null (MixesF t f a) where null = MixesF abort abort abort abort abort
instance Append (MixesF t f a) where
  MixesF pre₁ post₁ inf₁ infl₁ infr₁ ⧺ MixesF pre₂ post₂ inf₂ infl₂ infr₂ = 
    MixesF (pre₁ ⎅ pre₂) (post₁ ⎅ post₂) (inf₁ ⎅ inf₂) (infl₁ ⎅ infl₂) (infr₁ ⎅ infr₂)
instance Monoid (MixesF t f a)

data MixF t f a =
    PreF  ℕ (Parser t (f a → a))
  | PostF ℕ (Parser t (f a → a))
  | InfF  ℕ (Parser t (f a → f a → a))
  | InflF ℕ (Parser t (f a → f a → a))
  | InfrF ℕ (Parser t (f a → f a → a))
  | TerminalF (Parser t a)

data MixfixF t f a = MixfixF
  { mixfixFTerminals ∷ Parser t a
  , mixfixFLevels ∷ ℕ ⇰ MixesF t f a
  }
instance Null (MixfixF t f a) where null = MixfixF abort bot
instance Append (MixfixF t f a) where MixfixF ts₁ ls₁ ⧺ MixfixF ts₂ ls₂ = MixfixF (ts₁ ⎅ ts₂) (ls₁ ⧺ ls₂)

mixF ∷ MixF t f a → MixfixF t f a
mixF (PreF l pre) = null {mixfixFLevels = dict [l ↦ null {mixesFPre = pre}]}
mixF (PostF l post) = null {mixfixFLevels = dict [l ↦ null {mixesFPost = post}]}
mixF (InfF l inf) = null {mixfixFLevels = dict [l ↦ null {mixesFInf = inf}]}
mixF (InflF l infl) = null {mixfixFLevels = dict [l ↦ null {mixesFInfl = infl}]}
mixF (InfrF l infr) = null {mixfixFLevels = dict [l ↦ null {mixesFInfr = infr}]}
mixF (TerminalF term) = null {mixfixFTerminals = term}

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
      None → fld $ terms
      Some ((i :꘍ mixes) :꘍ levels') →
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
        [ mixesFPost mixes
        , do
            f ← mixesFInfl mixes
            x₂ ← nextLevel
            return $ \ x₁ → f x₁ x₂
        ]
      return $ wcompose (reverse fxs) x
    _levelInfr ∷ Parser t (f a) → MixesF t f a → Parser t a
    _levelInfr nextLevel mixes = do
      fxs ← pOneOrMore $ tries
        [ mixesFPre mixes
        , do
            x₁ ← nextLevel
            f ← mixesFInfr mixes
            return $ \ x₂ → f x₁ x₂
        ]
      x ← nextLevel
      return $ wcompose fxs x
    levelInfrAfterOne ∷ f a → Parser t (f a) → MixesF t f a → Parser t a
    levelInfrAfterOne x₁ nextLevel mixes = do
      f ← mixesFInfr mixes
      levelInfrAfterOneCombo (\ x₂ → f x₁ x₂) nextLevel mixes
    levelInfrNotAfterOne ∷ Parser t (f a) → MixesF t f a → Parser t a
    levelInfrNotAfterOne nextLevel mixes = do
      f ← mixesFPre mixes
      levelInfrAfterOneCombo f nextLevel mixes
    levelInfrAfterOneCombo ∷ (f a → a) → Parser t (f a) → MixesF t f a → Parser t a
    levelInfrAfterOneCombo f nextLevel mixes = do
      fxs ∷ 𝐿 (f a → a) ← pMany $ tries
        [ mixesFPre mixes
        , do
            x₁ ← nextLevel
            f' ← mixesFInfr mixes
            return $ \ x₂ → f' x₁ x₂
        ]
      x₂ ← nextLevel
      return $ wcompose (f:&fxs) x₂
    levelInfAfterOne ∷ f a → Parser t (f a) → MixesF t f a → Parser t a
    levelInfAfterOne x₁ nextLevel mixes = do
      f ← mixesFInf mixes
      x₂ ← nextLevel
      return $ f x₁ x₂

---------------
-- Non-fancy --
---------------

data Mixes t a = Mixes
  { mixesPre  ∷ Parser t (a → a)
  , mixesPost ∷ Parser t (a → a)
  , mixesInf  ∷ Parser t (a → a → a)
  , mixesInfl ∷ Parser t (a → a → a)
  , mixesInfr ∷ Parser t (a → a → a)
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
    Pre  ℕ (Parser t (a → a))
  | Post ℕ (Parser t (a → a))
  | Inf  ℕ (Parser t (a → a → a))
  | Infl ℕ (Parser t (a → a → a))
  | Infr ℕ (Parser t (a → a → a))
  | Terminal (Parser t a)

mix ∷ Mix t a → Mixfix t a
mix (Pre l pre) = null {mixfixLevels = dict [l ↦ null {mixesPre = pre}]}
mix (Post l post) = null {mixfixLevels = dict [l ↦ null {mixesPost = post}]}
mix (Inf l inf) = null {mixfixLevels = dict [l ↦ null {mixesInf = inf}]}
mix (Infl l infl) = null {mixfixLevels = dict [l ↦ null {mixesInfl = infl}]}
mix (Infr l infr) = null {mixfixLevels = dict [l ↦ null {mixesInfr = infr}]}
mix (Terminal term) = null {mixfixTerminals = term}

mixfixPure ∷ Mixfix t a → MixfixF t ID a
mixfixPure (Mixfix terminals levels) = MixfixF terminals $ map mixesPure levels

mixfixParser ∷ Mixfix t a → Parser t a
mixfixParser mixfix = unID ^$ mixfixParserF (mixfixPure mixfix) (map ID)
