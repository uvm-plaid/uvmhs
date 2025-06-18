module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core

import UVMHS.Lib.Annotated

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.GenParser

-----------------------------------
-- Fully Functor/Comonad general --
-----------------------------------

data GenMixesF t f a = GenMixesF
  { genMixesFPrefix  ∷ GenParser t (f a → a)
  , genMixesFPostfix ∷ GenParser t (f a → a)
  , genMixesFInfix  ∷ GenParser t (f a → f a → a)
  , genMixesFInfixL ∷ GenParser t (f a → f a → a)
  , genMixesFInfixR ∷ GenParser t (f a → f a → a)
  }

instance Null (GenMixesF t f a) where null = GenMixesF null null null null null
instance (Ord t) ⇒ Append (GenMixesF t f a) where
  GenMixesF pre₁ post₁ inf₁ infl₁ infr₁ ⧺ GenMixesF pre₂ post₂ inf₂ infl₂ infr₂ =
    GenMixesF (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) $ infr₁ ⧺ infr₂
instance (Ord t) ⇒ Monoid (GenMixesF t f a)

data GenMixfixF t f a = GenMixfixF
  { genMixfixFTerminals ∷ GenParser t a
  , genMixfixFLevels ∷ ℕ64 ⇰ GenMixesF t f a
  }
instance Null (GenMixfixF t f a) where null = GenMixfixF null null
instance (Ord t) ⇒ Append (GenMixfixF t f a) where GenMixfixF ts₁ ls₁ ⧺ GenMixfixF ts₂ ls₂ = GenMixfixF (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (GenMixfixF t f a)

gfmixOnlyTerms ∷ GenMixfixF t f a → GenMixfixF t f a
gfmixOnlyTerms m = GenMixfixF (genMixfixFTerminals m) null

gfmixPrefix ∷ ℕ64 → GenParser t (f a → a) → GenMixfixF t f a
gfmixPrefix l p = null { genMixfixFLevels = dict [ l ↦♭ null {genMixesFPrefix = p} ] }

gfmixPostfix ∷ ℕ64 → GenParser t (f a → a) → GenMixfixF t f a
gfmixPostfix l p = null { genMixfixFLevels = dict [ l ↦♭ null {genMixesFPostfix = p} ] }

gfmixInfix ∷ ℕ64 → GenParser t (f a → f a → a) → GenMixfixF t f a
gfmixInfix l p = null { genMixfixFLevels = dict [ l ↦♭ null {genMixesFInfix = p} ] }

gfmixInfixL ∷ ℕ64 → GenParser t (f a → f a → a) → GenMixfixF t f a
gfmixInfixL l p = null { genMixfixFLevels = dict [ l ↦♭ null {genMixesFInfixL = p} ] }

gfmixInfixR ∷ ℕ64 → GenParser t (f a → f a → a) → GenMixfixF t f a
gfmixInfixR l p = null { genMixfixFLevels = dict [ l ↦♭ null {genMixesFInfixR = p} ] }

gfmixTerminal ∷ GenParser t a → GenMixfixF t f a
gfmixTerminal p = null { genMixfixFTerminals = p}

-- PRE PRE x INFR PRE PRE y
-- ≈
-- PRE (PRE (x INFR (PRE (PRE y))))
--
-- x POST POST INFL y POST POST
-- ≈
-- ((((x POST) POST) INFL y) POST) POST

gfmixfix ∷
  ∀ t f a. (Ord t,Comonad f)
  ⇒ (GenParser t (f a) → GenParser t (f a))
  → (GenParser t (f a) → GenParser t (f a))
  → (GenParser t a → GenParser t (f a))
  → GenMixfixF t f a
  → GenParser t (f a)
gfmixfix new bracket cxt (GenMixfixF terms levels₀) = loop levels₀
  where
    loop ∷ ℕ64 ⇰ GenMixesF t f a → GenParser t (f a)
    loop levels = case dminView levels of
      None → new $ cxt terms
      Some ((i :* mixes) :* levels') →
        let msg = "lvl " ⧺ alignRightFill '0' 3 (show𝕊 i)
        in
        new $ cxt $ buildLevelDirected msg mixes $
        new $ cxt $ buildLevelNondirected msg mixes $
        loop levels'
    buildLevelNondirected ∷ 𝕊 → GenMixesF t f a → GenParser t (f a) → GenParser t a
    buildLevelNondirected msg mixes nextLevel = do
      x ← nextLevel
      concat
        [ gpErr (msg ⧺ " infix") $ levelInfAfterOne x mixes nextLevel
        , return $ extract x
        ]
    buildLevelDirected ∷ 𝕊 → GenMixesF t f a → GenParser t (f a) → GenParser t a
    buildLevelDirected msg mixes nextLevel = concat
      [ do
          x ← nextLevel
          concat
            [ gpErr (msg ⧺ " infixl") $ levelInflAfterOne x mixes nextLevel
            , gpErr (msg ⧺ " infixr") $ levelInfrAfterOne x mixes nextLevel
            , return $ extract x
            ]
      , gpErr (msg ⧺ " infixr") $ levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfAfterOne ∷ f a → GenMixesF t f a → GenParser t (f a) → GenParser t a
    levelInfAfterOne x₁ mixes nextLevel = do
      f ← genMixesFInfix mixes
      x₂ ← nextLevel
      return $ f x₁ x₂
    levelInflAfterOne ∷ f a → GenMixesF t f a → GenParser t (f a) → GenParser t a
    levelInflAfterOne x₁ mixes nextLevel = do
      x₁' ← cxt $ concat
        [ do f ← genMixesFInfixL mixes
             x₂ ← nextLevel
             return $ f x₁ x₂
        , do f ← genMixesFPostfix mixes
             return $ f x₁
        ]
      concat
        [ levelInflAfterOne x₁' mixes nextLevel
        , return $ extract x₁'
        ]
    levelInfrAfterOne ∷ f a → GenMixesF t f a → GenParser t (f a) → GenParser t a
    levelInfrAfterOne x₁ mixes nextLevel = do
      f ← genMixesFInfixR mixes
      x₂ ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x₁ x₂
    levelInfr ∷ GenMixesF t f a → GenParser t (f a) → GenParser t a
    levelInfr mixes nextLevel = concat
      [ do x₁ ← nextLevel
           concat
             [ levelInfrAfterOne x₁ mixes nextLevel
             , return $ extract x₁
             ]
      , levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfrNotAfterOne ∷ GenMixesF t f a → GenParser t (f a) → GenParser t a
    levelInfrNotAfterOne mixes nextLevel = do
      f ← genMixesFPrefix mixes
      x ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x

-- Instantiate with Annotation Comonad --

gfmixfixWithContext ∷ ∀ t c a. (Ord t) ⇒ (SrcCxt → c) → 𝕊 → GenMixfixF t (𝐴 c) a → GenParser t (𝐴 c a)
gfmixfixWithContext f s = gfmixfix (gpNewContext s) gpNewExpressionContext (map (mapATag f) ∘ gpWithContextRendered)

---------------
-- Non-fancy --
---------------

data GenMixes t a = GenMixes
  { genMixesPrefix  ∷ GenParser t (a → a)
  , genMixesPostfix ∷ GenParser t (a → a)
  , genMixesInfix  ∷ GenParser t (a → a → a)
  , genMixesInfixL ∷ GenParser t (a → a → a)
  , genMixesInfixR ∷ GenParser t (a → a → a)
  }

instance Null (GenMixes t a) where null = GenMixes null null null null null
instance (Ord t) ⇒ Append (GenMixes t a) where
  GenMixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ GenMixes pre₂ post₂ inf₂ infl₂ infr₂ =
    GenMixes (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) (infr₁ ⧺ infr₂)
instance (Ord t) ⇒ Monoid (GenMixes t a)

data GenMixfix t a = GenMixfix
  { genMixfixTerminals ∷ GenParser t a
  , genMixfixLevels ∷ ℕ64 ⇰ GenMixes t a
  }

instance Null (GenMixfix t a) where null = GenMixfix null bot
instance (Ord t) ⇒ Append (GenMixfix t a) where GenMixfix ts₁ ls₁ ⧺ GenMixfix ts₂ ls₂ = GenMixfix (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (GenMixfix t a)

gmixOnlyTerms ∷ GenMixfix t a → GenMixfix t a
gmixOnlyTerms m = GenMixfix (genMixfixTerminals m) null

gmixPrefix ∷ ℕ64 → GenParser t (a → a) → GenMixfix t a
gmixPrefix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesPrefix = p} ] }

gmixPostfix ∷ ℕ64 → GenParser t (a → a) → GenMixfix t a
gmixPostfix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesPostfix = p} ] }

gmixInfix ∷ ℕ64 → GenParser t (a → a → a) → GenMixfix t a
gmixInfix l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfix = p} ] }

gmixInfixL ∷ ℕ64 → GenParser t (a → a → a) → GenMixfix t a
gmixInfixL l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfixL = p} ] }

gmixInfixR ∷ ℕ64 → GenParser t (a → a → a) → GenMixfix t a
gmixInfixR l p = null { genMixfixLevels = dict [ l ↦♭ null {genMixesInfixR = p} ] }

gmixTerminal ∷ GenParser t a → GenMixfix t a
gmixTerminal p = null { genMixfixTerminals = p}

pureGMixesF ∷ (Ord t) ⇒ GenMixes t a → GenMixesF t ID a
pureGMixesF (GenMixes pre post inf infl infr) =
  GenMixesF
  (map kextract pre)
  (map kextract post)
  (map kextract2 inf)
  (map kextract2 infl)
  (map kextract2 infr)

pureMixfixF ∷ (Ord t) ⇒ GenMixfix t a → GenMixfixF t ID a
pureMixfixF (GenMixfix terminals levels) = GenMixfixF terminals $ map pureGMixesF levels

gmixfix ∷ (Ord t) ⇒ GenMixfix t a → GenParser t a
gmixfix mix = unID ^$ gfmixfix id id (map ID) (pureMixfixF mix)

gmixfixWithContext ∷ (Ord t) ⇒ (SrcCxt → c) → 𝕊 → GenMixfix t a → GenParser t (𝐴 c a)
gmixfixWithContext f s = map (mapATag f) ∘ gpNewContext s ∘ gpWithContextRendered ∘ gmixfix
