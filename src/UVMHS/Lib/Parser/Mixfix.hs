module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core

import UVMHS.Lib.Annotated

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.GenParser

-----------------------------------
-- Fully Functor/Comonad general --
-----------------------------------

data GenMixes t a b = GenMixes
  { genMixesPrefix  ∷ GenParser t (b → GenParser t a)
  , genMixesPostfix ∷ GenParser t (b → GenParser t a)
  , genMixesInfix  ∷ GenParser t (b → b → GenParser t a)
  , genMixesInfixL ∷ GenParser t (b → b → GenParser t a)
  , genMixesInfixR ∷ GenParser t (b → b → GenParser t a)
  }

instance Null (GenMixes t a b) where null = GenMixes null null null null null
instance (Ord t) ⇒ Append (GenMixes t a b) where
  GenMixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ GenMixes pre₂ post₂ inf₂ infl₂ infr₂ =
    GenMixes (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) $ infr₁ ⧺ infr₂
instance (Ord t) ⇒ Monoid (GenMixes t a b)

data GenMixfix t a b = GenMixfix
  { genMixfixTerminals ∷ GenParser t a
  , genMixfixLevels ∷ ℕ64 ⇰ GenMixes t a b
  }
instance Null (GenMixfix t a b) where null = GenMixfix null null
instance (Ord t) ⇒ Append (GenMixfix t a b) where GenMixfix ts₁ ls₁ ⧺ GenMixfix ts₂ ls₂ = GenMixfix (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (GenMixfix t a b)

gmixOnlyTerms ∷ GenMixfix t a b → GenMixfix t a b
gmixOnlyTerms m = GenMixfix (genMixfixTerminals m) null

gmixPrefix ∷ ℕ64 → GenParser t (b → GenParser t a) → GenMixfix t a b
gmixPrefix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesPrefix = p} ] }

gmixPostfix ∷ ℕ64 → GenParser t (b → GenParser t a) → GenMixfix t a b
gmixPostfix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesPostfix = p} ] }

gmixInfix ∷ ℕ64 → GenParser t (b → b → GenParser t a) → GenMixfix t a b
gmixInfix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfix = p} ] }

gmixInfixL ∷ ℕ64 → GenParser t (b → b → GenParser t a) → GenMixfix t a b
gmixInfixL l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfixL = p} ] }

gmixInfixR ∷ ℕ64 → GenParser t (b → b → GenParser t a) → GenMixfix t a b
gmixInfixR l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfixR = p} ] }

gmixTerminal ∷ GenParser t a → GenMixfix t a b
gmixTerminal p = null { genMixfixTerminals = p}

-- PRE PRE x INFR PRE PRE y
-- ≈
-- PRE (PRE (x INFR (PRE (PRE y))))
--
-- x POST POST INFL y POST POST
-- ≈
-- ((((x POST) POST) INFL y) POST) POST

gmixfix ∷
  ∀ t a b. (Ord t)
  ⇒ (∀ c. GenParser t c → GenParser t c)
  → (∀ c. GenParser t c → GenParser t c)
  → (GenParser t a → GenParser t b)
  → GenMixfix t a b
  → GenParser t b
gmixfix new bracket cxt (GenMixfix terms levels₀) = loop levels₀
  where
    loop ∷ ℕ64 ⇰ GenMixes t a b → GenParser t b
    loop levels = case dminView levels of
      None → new $ cxt terms
      Some ((i :* mixes) :* levels') →
        let msg = "lvl " ⧺ alignRightFill '0' 3 (show𝕊 i)
        in
        new $ buildLevelDirected msg mixes $
        new $ buildLevelNondirected msg mixes $
        loop levels'
    buildLevelNondirected ∷ 𝕊 → GenMixes t a b → GenParser t b → GenParser t b
    buildLevelNondirected msg mixes nextLevel = do
      x ← nextLevel
      concat
        [ gpErr (msg ⧺ " infix") $ levelInfAfterOne x mixes nextLevel
        , return $ {- extract -} x
        ]
    buildLevelDirected ∷ 𝕊 → GenMixes t a b → GenParser t b → GenParser t b
    buildLevelDirected msg mixes nextLevel = concat
      [ do
          x ← nextLevel
          concat
            [ gpErr (msg ⧺ " infixl") $ levelInflAfterOne x mixes nextLevel
            , gpErr (msg ⧺ " infixr") $ levelInfrAfterOne x mixes nextLevel
            , return $ {- extract -} x
            ]
      , gpErr (msg ⧺ " infixr") $ levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfAfterOne ∷ b → GenMixes t a b → GenParser t b → GenParser t b
    levelInfAfterOne x₁ mixes nextLevel = do
      f ← genMixesInfix mixes
      x₂ ← nextLevel
      cxt $ f x₁ x₂
    levelInflAfterOne ∷ b → GenMixes t a b → GenParser t b → GenParser t b
    levelInflAfterOne x₁ mixes nextLevel = do
      x₁' ← cxt $ concat
        [ do f ← genMixesInfixL mixes
             x₂ ← nextLevel
             f x₁ x₂
        , do f ← genMixesPostfix mixes
             f x₁
        ]
      concat
        [ levelInflAfterOne x₁' mixes nextLevel
        , return $ {- extract -} x₁'
        ]
    levelInfrAfterOne ∷ b → GenMixes t a b → GenParser t b → GenParser t b
    levelInfrAfterOne x₁ mixes nextLevel = do
      f ← genMixesInfixR mixes
      x₂ ← bracket $ levelInfr mixes nextLevel
      cxt $ f x₁ x₂
    levelInfr ∷ GenMixes t a b → GenParser t b → GenParser t b
    levelInfr mixes nextLevel = concat
      [ do x₁ ← nextLevel
           concat
             [ levelInfrAfterOne x₁ mixes nextLevel
             , return $ {- extract -} x₁
             ]
      , levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfrNotAfterOne ∷ GenMixes t a b → GenParser t b → GenParser t b
    levelInfrNotAfterOne mixes nextLevel = do
      f ← genMixesPrefix mixes
      x ← bracket $ levelInfr mixes nextLevel
      cxt $ f x

gmixfixWithContext ∷ ∀ t a b. (Ord t) ⇒  𝕊 → (𝐴 SrcCxt a → b) →GenMixfix t a b → GenParser t b
gmixfixWithContext s mk = gmixfix (gpNewContext s) gpNewExpressionContext $ mk ^∘ gpWithContextRendered
