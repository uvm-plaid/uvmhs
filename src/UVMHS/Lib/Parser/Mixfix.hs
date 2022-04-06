module UVMHS.Lib.Parser.Mixfix where

import UVMHS.Core

import UVMHS.Lib.Annotated

import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.CParser

-------------------
-- STATIC LEVELS --
-------------------

pLET,pSEP,pASC,pARR,pOR,pAND,pCMP,pCONS ∷ ℕ64
pBWAND,pBWOR,pBSHFT ∷ ℕ64
pPLUS,pTIMES,pNEG,pPOW,pFAC,pAPP,pIDX ∷ ℕ64

pLET    = 005  --  let fun
pSEP    = 006  --  , ;
pASC    = 007  --  e : τ
pARR    = 010  --  →
pOR     = 020  --  ∨
pAND    = 030  --  ∧
pCMP    = 040  --  ==
pCONS   = 041  --  ∷

pBWAND  = 045  -- ⊻ ⩔
pBWOR   = 046  -- ⩓
pBSHFT  = 047  -- ⋙ ⋘

pPLUS   = 050  --  + - ⩔ ⊻
pTIMES  = 060  --  * / ⩓
pNEG    = 065  --  - (negation)
pPOW    = 070  --  ^ ⋙ ⋘
pFAC    = 090  --  !

pAPP    = 200 --  ␣

pIDX    = 300 --  x.y x@y

-----------------------------------
-- Fully Functor/Comonad general --
-----------------------------------

data MixesF t f a = MixesF
  { mixesFPrefix  ∷ CParser t (f a → a)
  , mixesFPostfix ∷ CParser t (f a → a)
  , mixesFInfix  ∷ CParser t (f a → f a → a)
  , mixesFInfixL ∷ CParser t (f a → f a → a)
  , mixesFInfixR ∷ CParser t (f a → f a → a)
  }

instance Null (MixesF t f a) where null = MixesF null null null null null
instance (Ord t) ⇒ Append (MixesF t f a) where
  MixesF pre₁ post₁ inf₁ infl₁ infr₁ ⧺ MixesF pre₂ post₂ inf₂ infl₂ infr₂ = 
    MixesF (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) $ infr₁ ⧺ infr₂
instance (Ord t) ⇒ Monoid (MixesF t f a)

data MixfixF t f a = MixfixF
  { mixfixFTerminals ∷ CParser t a
  , mixfixFLevels ∷ ℕ64 ⇰ MixesF t f a
  }
instance Null (MixfixF t f a) where null = MixfixF null null
instance (Ord t) ⇒ Append (MixfixF t f a) where MixfixF ts₁ ls₁ ⧺ MixfixF ts₂ ls₂ = MixfixF (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (MixfixF t f a)

onlyTerminalsF ∷ MixfixF t f a → MixfixF t f a
onlyTerminalsF m = MixfixF (mixfixFTerminals m) null

fmixPrefix ∷ ℕ64 → CParser t (f a → a) → MixfixF t f a
fmixPrefix l p = null { mixfixFLevels = dict [ l ↦ null {mixesFPrefix = p} ] }

fmixPostfix ∷ ℕ64 → CParser t (f a → a) → MixfixF t f a
fmixPostfix l p = null { mixfixFLevels = dict [ l ↦ null {mixesFPostfix = p} ] }

fmixInfix ∷ ℕ64 → CParser t (f a → f a → a) → MixfixF t f a
fmixInfix l p = null { mixfixFLevels = dict [ l ↦ null {mixesFInfix = p} ] }

fmixInfixL ∷ ℕ64 → CParser t (f a → f a → a) → MixfixF t f a
fmixInfixL l p = null { mixfixFLevels = dict [ l ↦ null {mixesFInfixL = p} ] }

fmixInfixR ∷ ℕ64 → CParser t (f a → f a → a) → MixfixF t f a
fmixInfixR l p = null { mixfixFLevels = dict [ l ↦ null {mixesFInfixR = p} ] }

fmixTerminal ∷ CParser t a → MixfixF t f a
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
  ⇒ (CParser t (f a) → CParser t (f a)) 
  → (CParser t (f a) → CParser t (f a)) 
  → (CParser t a → CParser t (f a)) 
  → MixfixF t f a 
  → CParser t (f a)
fmixfix new bracket cxt (MixfixF terms levels₀) = loop levels₀
  where
    loop ∷ ℕ64 ⇰ MixesF t f a → CParser t (f a)
    loop levels = case dminView levels of
      None → new $ cxt terms
      Some ((i :* mixes) :* levels') →
        let msg = "lvl " ⧺ alignRightFill '0' 3 (show𝕊 i)
        in 
        new $ cxt $ buildLevelDirected msg mixes $ 
        new $ cxt $ buildLevelNondirected msg mixes $ 
        loop levels'
    buildLevelNondirected ∷ 𝕊 → MixesF t f a → CParser t (f a) → CParser t a
    buildLevelNondirected msg mixes nextLevel = do
      x ← nextLevel
      concat
        [ cpErr (msg ⧺ " infix") $ levelInfAfterOne x mixes nextLevel
        , return $ extract x
        ]
    buildLevelDirected ∷ 𝕊 → MixesF t f a → CParser t (f a) → CParser t a
    buildLevelDirected msg mixes nextLevel = concat
      [ do
          x ← nextLevel
          concat
            [ cpErr (msg ⧺ " infixl") $ levelInflAfterOne x mixes nextLevel
            , cpErr (msg ⧺ " infixr") $ levelInfrAfterOne x mixes nextLevel
            , return $ extract x
            ]
      , cpErr (msg ⧺ " infixr") $ levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfAfterOne ∷ f a → MixesF t f a → CParser t (f a) → CParser t a
    levelInfAfterOne x₁ mixes nextLevel = do
      f ← mixesFInfix mixes
      x₂ ← nextLevel
      return $ f x₁ x₂
    levelInflAfterOne ∷ f a → MixesF t f a → CParser t (f a) → CParser t a
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
    levelInfrAfterOne ∷ f a → MixesF t f a → CParser t (f a) → CParser t a
    levelInfrAfterOne x₁ mixes nextLevel = do
      f ← mixesFInfixR mixes
      x₂ ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x₁ x₂
    levelInfr ∷ MixesF t f a → CParser t (f a) → CParser t a
    levelInfr mixes nextLevel = concat
      [ do x₁ ← nextLevel
           concat
             [ levelInfrAfterOne x₁ mixes nextLevel
             , return $ extract x₁
             ]
      , levelInfrNotAfterOne mixes nextLevel
      ]
    levelInfrNotAfterOne ∷ MixesF t f a → CParser t (f a) → CParser t a
    levelInfrNotAfterOne mixes nextLevel = do
      f ← mixesFPrefix mixes
      x ← bracket $ cxt $ levelInfr mixes nextLevel
      return $ f x

fmixfixWithContext ∷ ∀ t a. (Ord t) ⇒ 𝕊 → MixfixF t (𝐴 SrcCxt) a → CParser t (𝐴 SrcCxt a)
fmixfixWithContext s = fmixfix (cpNewContext s) cpNewExpressionContext cpWithContextRendered

---------------
-- Non-fancy --
---------------

data Mixes t a = Mixes
  { mixesPrefix  ∷ CParser t (a → a)
  , mixesPostfix ∷ CParser t (a → a)
  , mixesInfix  ∷ CParser t (a → a → a)
  , mixesInfixL ∷ CParser t (a → a → a)
  , mixesInfixR ∷ CParser t (a → a → a)
  }

instance Null (Mixes t a) where null = Mixes null null null null null
instance (Ord t) ⇒ Append (Mixes t a) where 
  Mixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ Mixes pre₂ post₂ inf₂ infl₂ infr₂ = 
    Mixes (pre₁ ⧺ pre₂) (post₁ ⧺ post₂) (inf₁ ⧺ inf₂) (infl₁ ⧺ infl₂) (infr₁ ⧺ infr₂)
instance (Ord t) ⇒ Monoid (Mixes t a)

data Mixfix t a = Mixfix 
  { mixfixTerminals ∷ CParser t a
  , mixfixLevels ∷ ℕ64 ⇰ Mixes t a
  }

instance Null (Mixfix t a) where null = Mixfix null bot
instance (Ord t) ⇒ Append (Mixfix t a) where Mixfix ts₁ ls₁ ⧺ Mixfix ts₂ ls₂ = Mixfix (ts₁ ⧺ ts₂) (ls₁ ⧺ ls₂)
instance (Ord t) ⇒ Monoid (Mixfix t a)

mixPrefix ∷ ℕ64 → CParser t (a → a) → Mixfix t a
mixPrefix l p = null { mixfixLevels = dict [ l ↦ null {mixesPrefix = p} ] }

mixPostfix ∷ ℕ64 → CParser t (a → a) → Mixfix t a
mixPostfix l p = null { mixfixLevels = dict [ l ↦ null {mixesPostfix = p} ] }

mixInfix ∷ ℕ64 → CParser t (a → a → a) → Mixfix t a
mixInfix l p = null { mixfixLevels = dict [ l ↦ null {mixesInfix = p} ] }

mixInfixL ∷ ℕ64 → CParser t (a → a → a) → Mixfix t a
mixInfixL l p = null { mixfixLevels = dict [ l ↦ null {mixesInfixL = p} ] }

mixInfixR ∷ ℕ64 → CParser t (a → a → a) → Mixfix t a
mixInfixR l p = null { mixfixLevels = dict [ l ↦ null {mixesInfixR = p} ] }

mixTerminal ∷ CParser t a → Mixfix t a
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

mixfix ∷ (Ord t) ⇒ Mixfix t a → CParser t a
mixfix mix = unID ^$ fmixfix id id (map ID) (mixfixPure mix) 

mixfixWithContext ∷ (Ord t) ⇒ 𝕊 → Mixfix t a → CParser t (𝐴 SrcCxt a)
mixfixWithContext s = cpNewContext s ∘ cpWithContextRendered ∘ mixfix
