module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy

-- =============== --
-- SIMPLE VARIABLE --
-- =============== --

data 𝕎 = 𝕎
  { markVar ∷ 𝑂 ℕ64
  , nameVar ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕎

var ∷ 𝕊 → 𝕎
var = 𝕎 None

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕎
gensymVar ℓ s = do
  n ← nextL ℓ
  return $ 𝕎 (Some n) s

-------------
-- PARSING --
-------------

syntaxVar ∷ LexerBasicSyntax
syntaxVar = concat
  [ null { lexerBasicSyntaxPuns = pow ["#"] }
  ]

cpVar ∷ CParser TokenBasic 𝕎
cpVar = do
  x ← cpShaped $ view nameTBasicL
  nO ← cpOptional $ do
    void $ cpSyntax "#"
    cpNat64
  return $ 𝕎 nO x

cpVarWS ∷ CParser TokenWSBasic 𝕎
cpVarWS = do
  x ← cpShaped $ view nameTWSBasicL
  nO ← cpOptional $ do
    void $ cpSyntaxWS "#"
    failEff ∘ natO64 *$ cpIntegerWS
  return $ 𝕎 nO x

syntaxDVar ∷ LexerBasicSyntax
syntaxDVar = concat
  [ null { lexerBasicSyntaxPuns = pow ["|_","_|","⌊","⌋","INF","∞"] }
  ]

cpDVarRaw ∷ CParser TokenBasic ℕ64
cpDVarRaw = cpNat64

cpDVarRawInf ∷ CParser TokenBasic (𝑂 ℕ64)
cpDVarRawInf = concat
  [ Some ^$ cpDVarRaw
  , do void $ concat $ map cpSyntax ["INF","∞"]
       return None
  ]

cpDVar ∷ CParser TokenBasic ℕ64
cpDVar = do 
  void $ concat $ map cpSyntax ["|_","⌊"]
  n ← cpDVar
  void $ concat $ map cpSyntax ["_|","⌋"]
  return n

---------------------
-- PRETTY PRINTING --
---------------------

instance Pretty 𝕎 where
  pretty (𝕎 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

-------------
-- FUZZING --
-------------

instance Fuzzy 𝕎 where
  fuzzy = do
    nO ← fuzzy
    return $ 𝕎 nO "x"

-- =============== --
-- SCOPED VARIABLE --
-- =============== --

data 𝕏 =
    D_SVar ℕ64    -- nameless variable
  | N_SVar ℕ64 𝕎  -- named (+ nameless index for that name)
                 -- λ x. λ x. x↑0
                 --        └───┘
                 -- λ x. λ x. x↑1
                 --   └────────┘
  | G_SVar 𝕎      -- global variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕏

znsvar ∷ 𝕎 → 𝕏
znsvar = N_SVar 0

znsvarL ∷ 𝕏 ⌲ 𝕎
znsvarL = prism znsvar $ \case
  N_SVar n x | n≡0 → Some x
  _ → None

gensymSVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕏
gensymSVar ℓ s = znsvar ^$ gensymVar ℓ s

-------------
-- PARSING --
-------------

cpZNSVar ∷ CParser TokenBasic 𝕏
cpZNSVar = znsvar ^$ cpVar

cpGSVar ∷ CParser TokenBasic 𝕏
cpGSVar = G_SVar ^$ cpVar

cpNSVarWS ∷ CParser TokenWSBasic 𝕏
cpNSVarWS = znsvar ^$ cpVarWS

cpGSVarWS ∷ CParser TokenWSBasic 𝕏
cpGSVarWS = G_SVar ^$ cpVarWS

syntaxSVar ∷ LexerBasicSyntax
syntaxSVar = concat
  [ syntaxVar
  , syntaxDVar
  , null { lexerBasicSyntaxPuns = pow ["INF","∞",":",":g"] }
  ]

cpSVarNGVar ∷ CParser TokenBasic ((ℕ64 ∧ 𝕎) ∨ 𝕎)
cpSVarNGVar = do
  x ← cpVar
  concat
    [ do n ← ifNone 0 ^$ cpOptional $ do
           void $ cpSyntax ":"
           n ← cpNat64
           return n
         return $ Inl $ n :* x
    , do void $ cpSyntax ":g"
         return $ Inr x
    ]

cpSVarNGVarInf ∷ CParser TokenBasic ((𝑂 ℕ64 ∧ 𝕎) ∨ 𝕎)
cpSVarNGVarInf = do
  x ← cpVar
  concat
    [ do n ← ifNone (Some 0) ^$ cpOptional $ do
           void $ cpSyntax ":"
           concat
             [ Some ^$ cpNat64
             , do void $ concat $ map cpSyntax ["INF","∞"]
                  return None
             ]
         return $ Inl $ n :* x
    , do void $ cpSyntax ":g"
         return $ Inr x
    ]

cpSVarRaw ∷ CParser TokenBasic 𝕏
cpSVarRaw = concat
  [ do n ← cpDVarRaw
       return $ D_SVar n
  , do nww ← cpSVarNGVar
       return $ case nww of
         Inl (n :* w) → N_SVar n w
         Inr w        → G_SVar w
  ]

cpSVarRawInf ∷ CParser TokenBasic (𝕏 ∨ 𝑂 𝕎)
cpSVarRawInf = concat
  [ do nO ← cpDVarRawInf
       case nO of
         None → return $ Inr None
         Some n → return $ Inl $ D_SVar n
  , do nww ← cpSVarNGVarInf
       return $ case nww of
         Inl (nO :* w) → case nO of
            None → Inr $ Some w
            Some n → Inl $ N_SVar n w
         Inr w → Inl $ G_SVar w
  ]

cpSVar ∷ CParser TokenBasic 𝕏
cpSVar = concat
  [ do n ← cpDVar
       return $ D_SVar n
  , do nww ← cpSVarNGVar
       return $ case nww of
         Inl (n :* w) → N_SVar n w
         Inr w        → G_SVar w
  ]

---------------------
-- PRETTY PRINTING --
---------------------

ppDVar ∷ ℕ64 → Doc
ppDVar n = concat [ppPun "⌊",pretty n,ppPun "⌋"]

ppNVar ∷ Doc → Doc → Doc
ppNVar n x = concat [x,ppPun ":",n]

instance Pretty 𝕏 where
  pretty = \case
    N_SVar n x → if n ≡ 0 then pretty x else ppNVar (pretty n) $ pretty x
    D_SVar n → ppDVar n
    G_SVar x → concat [pretty x,ppPun ":g"]

-------------
-- FUZZING --
-------------

instance Fuzzy 𝕏 where
  fuzzy = wrchoose
    [ (:*) one $ \ () → D_SVar ^$ fuzzy
    , (:*) one $ \ () → return N_SVar ⊡ fuzzy ⊡ fuzzy
    , (:*) one $ \ () → G_SVar ^$ fuzzy
    ]

-- ======== --
-- SVarView --
-- ======== --

class SVarView s e | e→s where
  svarL ∷ s → e ⌲ 𝕏

svarScopeL ∷ ∀ s e. (SVarView s e) ⇒ s → 𝑂 𝕎 → e ⌲ ℕ64
svarScopeL s xO = 
  let ctor ∷ ℕ64 → e
      ctor = case xO of
        None → \ n → construct (svarL s) $ D_SVar n
        Some x → \ n → construct (svarL s) $ N_SVar n x
      dtor ∷ e → 𝑂 ℕ64
      dtor = case xO of
        None → \ e → view (d_SVarL ⊚ svarL s) e
        Some x → \ e → do
          n :* x' ← view (n_SVarL ⊚ svarL s) e
          guard $ x ≡ x'
          return n
  in prism ctor dtor
    
