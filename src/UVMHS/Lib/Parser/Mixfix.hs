module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core

import UVMHS.Lib.Annotated

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Parser

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

instance Null (MixesF t f a) where null = MixesF null null null null null
instance (Ord t) ⇒ Append (MixesF t f a) where
  MixesF pre₁ post₁ inf₁ infl₁ infr₁ ⧺ MixesF pre₂ post₂ inf₂ infl₂ infr₂ =
    MixesF (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) $ infr₁ ⧺ infr₂
instance (Ord t) ⇒ Monoid (MixesF t f a)

data MixfixF t f a = MixfixF
  { mixfixFTerminals ∷ Parser t a
  , mixfixFLevels ∷ ℕ64 ⇰ MixesF t f a
  }
instance Null (MixfixF t f a) where null = MixfixF null null
instance (Ord t) ⇒ Append (MixfixF t f a) where MixfixF ts₁ ls₁ ⧺ MixfixF ts₂ ls₂ = MixfixF (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (MixfixF t f a)

onlyTerminalsF ∷ MixfixF t f a → MixfixF t f a
onlyTerminalsF m = MixfixF (mixfixFTerminals m) null

fmixPrefix ∷ ℕ64 → Parser t (f a → a) → MixfixF t f a
fmixPrefix l p = null { mixfixFLevels = dict [ l ↦♭ null {mixesFPrefix = p} ] }

fmixPostfix ∷ ℕ64 → Parser t (f a → a) → MixfixF t f a
fmixPostfix l p = null { mixfixFLevels = dict [ l ↦♭ null {mixesFPostfix = p} ] }

fmixInfix ∷ ℕ64 → Parser t (f a → f a → a) → MixfixF t f a
fmixInfix l p = null { mixfixFLevels = dict [ l ↦♭ null {mixesFInfix = p} ] }

fmixInfixL ∷ ℕ64 → Parser t (f a → f a → a) → MixfixF t f a
fmixInfixL l p = null { mixfixFLevels = dict [ l ↦♭ null {mixesFInfixL = p} ] }

fmixInfixR ∷ ℕ64 → Parser t (f a → f a → a) → MixfixF t f a
fmixInfixR l p = null { mixfixFLevels = dict [ l ↦♭ null {mixesFInfixR = p} ] }

fmixTerminal ∷ Parser t a → MixfixF t f a
fmixTerminal p = null { mixfixFTerminals = p}

-- PRE PRE x INFR PRE PRE y
-- ≈
-- PRE (PRE (x INFR (PRE (PRE y))))
--
-- x POST POST INFL y POST POST
-- ≈
-- ((((x POST) POST) INFL y) POST) POST

fmixfix ∷
  ∀ t f a. (Ord t,Comonad f)
  ⇒ (Parser t (f a) → Parser t (f a))
  → (Parser t (f a) → Parser t (f a))
  → (Parser t a → Parser t (f a))
  → MixfixF t f a
  → Parser t (f a)
fmixfix new bracket cxt (MixfixF terms levels₀) = loop levels₀
  where
    loop ∷ ℕ64 ⇰ MixesF t f a → Parser t (f a)
    loop levels = case dminView levels of
      None → new $ cxt terms
      Some ((i :* mixes) :* levels') →
        let msg = "lvl " ⧺ alignRightFill '0' 3 (show𝕊 i)
        in
        new $ cxt $ buildLevelDirected msg mixes $
        new $ cxt $ buildLevelNondirected msg mixes $
        loop levels'
    buildLevelNondirected ∷ 𝕊 → MixesF t f a → Parser t (f a) → Parser t a
    buildLevelNondirected msg mixes nextLevel = do
      x ← nextLevel
      concat
        [ pErr (msg ⧺ " infix") $ levelInfAfterOne x mixes nextLevel
        , return $ extract x
        ]
    buildLevelDirected ∷ 𝕊 → MixesF t f a → Parser t (f a) → Parser t a
    buildLevelDirected msg mixes nextLevel = concat
      [ do
          x ← nextLevel
          concat
            [ pErr (msg ⧺ " infixl") $ levelInflAfterOne x mixes nextLevel
            , pErr (msg ⧺ " infixr") $ levelInfrAfterOne x mixes nextLevel
            , return $ extract x
            ]
      , pErr (msg ⧺ " infixr") $ levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfAfterOne ∷ f a → MixesF t f a → Parser t (f a) → Parser t a
    levelInfAfterOne x₁ mixes nextLevel = do
      f ← mixesFInfix mixes
      x₂ ← nextLevel
      return $ f x₁ x₂
    levelInflAfterOne ∷ f a → MixesF t f a → Parser t (f a) → Parser t a
    levelInflAfterOne x₁ mixes nextLevel = do
      x₁' ← cxt $ concat
        [ do f ← mixesFInfixL mixes
             x₂ ← nextLevel
             return $ f x₁ x₂
        , do f ← mixesFPostfix mixes
             return $ f x₁
        ]
      concat
        [ levelInflAfterOne x₁' mixes nextLevel
        , return $ extract x₁'
        ]
    levelInfrAfterOne ∷ f a → MixesF t f a → Parser t (f a) → Parser t a
    levelInfrAfterOne x₁ mixes nextLevel = do
      f ← mixesFInfixR mixes
      x₂ ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x₁ x₂
    levelInfr ∷ MixesF t f a → Parser t (f a) → Parser t a
    levelInfr mixes nextLevel = concat
      [ do x₁ ← nextLevel
           concat
             [ levelInfrAfterOne x₁ mixes nextLevel
             , return $ extract x₁
             ]
      , levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfrNotAfterOne ∷ MixesF t f a → Parser t (f a) → Parser t a
    levelInfrNotAfterOne mixes nextLevel = do
      f ← mixesFPrefix mixes
      x ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x

fmixfixWithContext ∷ ∀ t a. (Ord t) ⇒ 𝕊 → MixfixF t (𝐴 SrcCxt) a → Parser t (𝐴 SrcCxt a)
fmixfixWithContext s = fmixfix (pNewContext s) pNewExpressionContext pWithContextRendered

fmixfixWithContextSet ∷ ∀ t a. (Ord t) ⇒ 𝕊 → MixfixF t (𝐴 (𝑃 SrcCxt)) a → Parser t (𝐴 (𝑃 SrcCxt) a)
fmixfixWithContextSet s = fmixfix (pNewContext s) pNewExpressionContext (map (mapATag single) ∘ pWithContextRendered)

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

instance Null (Mixes t a) where null = Mixes null null null null null
instance (Ord t) ⇒ Append (Mixes t a) where
  Mixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ Mixes pre₂ post₂ inf₂ infl₂ infr₂ =
    Mixes (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) (infr₁ ⧺ infr₂)
instance (Ord t) ⇒ Monoid (Mixes t a)

data Mixfix t a = Mixfix
  { mixfixTerminals ∷ Parser t a
  , mixfixLevels ∷ ℕ64 ⇰ Mixes t a
  }

instance Null (Mixfix t a) where null = Mixfix null bot
instance (Ord t) ⇒ Append (Mixfix t a) where Mixfix ts₁ ls₁ ⧺ Mixfix ts₂ ls₂ = Mixfix (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (Mixfix t a)

mixPrefix ∷ ℕ64 → Parser t (a → a) → Mixfix t a
mixPrefix l p = null { mixfixLevels = dict [ l ↦♭ null {mixesPrefix = p} ] }

mixPostfix ∷ ℕ64 → Parser t (a → a) → Mixfix t a
mixPostfix l p = null { mixfixLevels = dict [ l ↦♭ null {mixesPostfix = p} ] }

mixInfix ∷ ℕ64 → Parser t (a → a → a) → Mixfix t a
mixInfix l p = null { mixfixLevels = dict [ l ↦♭ null {mixesInfix = p} ] }

mixInfixL ∷ ℕ64 → Parser t (a → a → a) → Mixfix t a
mixInfixL l p = null { mixfixLevels = dict [ l ↦♭ null {mixesInfixL = p} ] }

mixInfixR ∷ ℕ64 → Parser t (a → a → a) → Mixfix t a
mixInfixR l p = null { mixfixLevels = dict [ l ↦♭ null {mixesInfixR = p} ] }

mixTerminal ∷ Parser t a → Mixfix t a
mixTerminal p = null { mixfixTerminals = p}

mixesPure ∷ (Ord t) ⇒ Mixes t a → MixesF t ID a
mixesPure (Mixes pre post inf infl infr) =
  MixesF
  (map kextract pre)
  (map kextract post)
  (map kextract2 inf)
  (map kextract2 infl)
  (map kextract2 infr)

mixfixPure ∷ (Ord t) ⇒ Mixfix t a → MixfixF t ID a
mixfixPure (Mixfix terminals levels) = MixfixF terminals $ map mixesPure levels

mixfix ∷ (Ord t) ⇒ Mixfix t a → Parser t a
mixfix mix = unID ^$ fmixfix id id (map ID) (mixfixPure mix)

mixfixWithContext ∷ (Ord t) ⇒ 𝕊 → Mixfix t a → Parser t (𝐴 SrcCxt a)
mixfixWithContext s = pNewContext s ∘ pWithContextRendered ∘ mixfix
