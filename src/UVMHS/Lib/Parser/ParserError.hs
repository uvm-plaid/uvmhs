module UVMHS.Lib.Parser.ParserError where

import UVMHS.Core

import UVMHS.Lib.Window
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.Loc

data ParserErrorInfo = ParserErrorInfo
  { parserErrorInfoPrefix ∷ WindowR Doc Doc
  , parserErrorInfoContext ∷ WindowR Doc Doc
  , parserErrorInfoStackHead ∷ 𝕊
  , parserErrorInfoStackTail ∷ 𝐼 𝕊
  }

data ParserError t = ParserError
  { parserErrorTokenLoc ∷ AddBot Loc
  , parserErrorTokenContext ∷ WindowR Doc Doc
  , parserErrorTokenSuffix ∷ WindowL Doc Doc
  , parserErrorFailures ∷ 𝐼 ParserErrorInfo
  }

instance Append (ParserError t) where
  e₁@(ParserError l₁ d₁ s₁ f₁) ⧺ e₂@(ParserError l₂ _ _ f₂) =
    case l₁ ⋚ l₂ of
      LT → e₂
      EQ → ParserError l₁ d₁ s₁ $ f₁ ⧺ f₂
      GT → e₁

data ParserErrorStackTraces = ParserErrorStackTraces 
  { parserErrorStackTracesMessages ∷ 𝑃 𝕊
  , parserErrorStackTracesChain ∷ 𝕊 ⇰ ParserErrorStackTraces
  } deriving (Eq, Ord)
makeLenses ''ParserErrorStackTraces
makePrettyRecord ''ParserErrorStackTraces

instance Bot ParserErrorStackTraces where
  bot = ParserErrorStackTraces bot bot
instance Join ParserErrorStackTraces where
  ParserErrorStackTraces fin₁ ch₁ ⊔ ParserErrorStackTraces fin₂ ch₂ = ParserErrorStackTraces (fin₁ ⊔ fin₂) (ch₁ ⊔ ch₂)
instance JoinLattice ParserErrorStackTraces

stackTraces ∷ 𝕊 → 𝐼 𝕊 → ParserErrorStackTraces
stackTraces fin msgs = foldrOnFrom msgs (ParserErrorStackTraces (single fin) bot) $ \ msg tr → 
  ParserErrorStackTraces bot $ msg ↦ tr

parserErrorFailuresMap ∷ 𝐼 ParserErrorInfo → (𝕊 ∧ 𝔹) ⇰ WindowR Doc Doc ∧ WindowR Doc Doc ∧ ParserErrorStackTraces
parserErrorFailuresMap eis = 
  fold bot (unionWith $ \ (c' :* p' :* t₁) (_ :* _ :* t₂) → c' :* p' :* (t₁ ⊔ t₂)) $ 
    mapOn eis $ \ (ParserErrorInfo p c sh st) → 
      (ppRender (concat c) :* overflowR c) ↦ (p :* c :* stackTraces sh st)

displaySourceError ∷ 𝕊 → AddNull (ParserError t) → Doc
displaySourceError so peM = ppVertical $ concat
  [ return $ ppHeader "Parse Failure"
  , return $ ppHeader "Source:"
  , if so ≡ null then mzero else return $ ppHorizontal [ppErr ">",ppBD $ ppString so]
  , case peM of
      Null → return $ ppErr "> No Reported Errors"
      AddNull (ParserError l tc ts fs) → concat
        [ return $ ppHorizontal 
            [ ppErr ">"
            , concat 
                [ ppString "line:"
                , elimAddBot (ppString "?") (pretty ∘ succ ∘ locRow) l
                ]
            , concat 
                [ ppString "column:"
                , elimAddBot (ppString "?") (pretty ∘ succ ∘ locCol) l
                ]
            ]
        , return $ ppHeader "One of:"
        , inbetween (ppHeader "OR") $ mapOn (map snd $ iter $ parserErrorFailuresMap fs) $ \ (pp :* pc :* ets) →
            ppVertical 
              [ concat
                  [ renderWindowR pp
                  , ppUT '^' green $ renderWindowR pc
                  , ppUT '^' red $ renderWindowR tc
                  , renderWindowL ts
                  ]
              , displayErrorTraces ets
              ]
        ]
  ]

displayErrorTraces ∷ ParserErrorStackTraces → Doc
displayErrorTraces (ParserErrorStackTraces final chain) = ppVertical $ concat
  [ case isEmpty final of
      True → null 
      False → return $ ppHorizontal $ concat
        [ single $ ppFG red $ ppString "Expected"
        , inbetween (ppFG red $ ppString "OR") $ map ppString $ iter final
        ]
  , mapOn (iter chain) $ \ (msg :* tr) → ppVertical
      [ ppHorizontal
          [ ppFG darkGreen $ ppString "Parsing"
          , ppString msg
          ]
      , concat [ppSpace $ 𝕟64 2,ppAlign $ displayErrorTraces tr]
      ]
  ]    
