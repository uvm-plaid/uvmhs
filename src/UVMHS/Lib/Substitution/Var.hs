module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

-- =============== --
-- SIMPLE VARIABLE --
-- =============== --

data Name = Name
  { varMark ∷ 𝑂 ℕ64
  , varName ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''Name

var ∷ 𝕊 → Name
var = Name None

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m Name
gensymVar ℓ s = do
  n ← nextL ℓ
  return $ Name (Some n) s

-------------
-- PARSING --
-------------

syntaxVar ∷ LexerBasicSyntax
syntaxVar = concat
  [ null { lexerBasicSyntaxPuns = pow ["#"] }
  ]

cpVar ∷ CParser TokenBasic Name
cpVar = do
  x ← cpShaped $ view nameTBasicL
  nO ← cpOptional $ do
    void $ cpSyntax "#"
    cpNat64
  return $ Name nO x

cpVarWS ∷ CParser TokenWSBasic Name
cpVarWS = do
  x ← cpShaped $ view nameTWSBasicL
  nO ← cpOptional $ do
    void $ cpSyntaxWS "#"
    failEff ∘ natO64 *$ cpIntegerWS
  return $ Name nO x

syntaxDVar ∷ LexerBasicSyntax
syntaxDVar = concat
  [ null { lexerBasicSyntaxPuns = pow ["INF","∞"] }
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
  void $ cpSyntax ":"
  cpDVarRaw

cpDVarInf ∷ CParser TokenBasic (𝑂 ℕ64)
cpDVarInf = do
  void $ cpSyntax ":"
  cpDVarRawInf


---------------------
-- PRETTY PRINTING --
---------------------

instance Pretty Name where
  pretty (Name nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

-------------
-- FUZZING --
-------------

instance Fuzzy Name where
  fuzzy = do
    nO ← fuzzy
    return $ Name nO "x"

instance Shrinky Name where
  shrink = const null

-- =============== --
-- SCOPED VARIABLE --
-- =============== --

data 𝕏 =
    D_SVar ℕ64    -- nameless variable
  | N_SVar ℕ64 Name  -- named (+ nameless index for that name)
                 -- λ x. λ x. x↑0
                 --        └───┘
                 -- λ x. λ x. x↑1
                 --   └────────┘
  | G_SVar Name      -- global variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕏

znsvar ∷ Name → 𝕏
znsvar = N_SVar 0

znsvarL ∷ 𝕏 ⌲ Name
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

cpZNSVarWS ∷ CParser TokenWSBasic 𝕏
cpZNSVarWS = znsvar ^$ cpVarWS

cpGSVarWS ∷ CParser TokenWSBasic 𝕏
cpGSVarWS = G_SVar ^$ cpVarWS

syntaxSVar ∷ LexerBasicSyntax
syntaxSVar = concat
  [ syntaxVar
  , syntaxDVar
  , null { lexerBasicSyntaxPuns = pow ["INF","∞",":",":g"] }
  ]

cpSVarNGVarTail ∷ CParser TokenBasic (𝑂 ℕ64)
cpSVarNGVarTail = concat
  [ do n ← ifNone 0 ^$ cpOptional $ do
         void $ cpSyntax ":"
         n ← cpNat64
         return n
       return $ Some n
  , do void $ cpSyntax ":g"
       return None
  ]

cpSVarNGVar ∷ CParser TokenBasic ((ℕ64 ∧ Name) ∨ Name)
cpSVarNGVar = do
  w ← cpVar
  nO ← cpSVarNGVarTail
  return $ case nO of
    Some n → Inl $ n :* w
    None → Inr w

cpSVarNGVarInfTail ∷ CParser TokenBasic (𝑂 (𝑂 ℕ64))
cpSVarNGVarInfTail = concat
  [ do nO ← ifNone (Some 0) ^$ cpOptional $ do
         void $ cpSyntax ":"
         concat
           [ Some ^$ cpNat64
           , do void $ concat $ map cpSyntax ["INF","∞"]
                return None
           ]
       return $ Some nO
  , do void $ cpSyntax ":g"
       return $ None
  ]

cpSVarNGVarInf ∷ CParser TokenBasic ((𝑂 ℕ64 ∧ Name) ∨ Name)
cpSVarNGVarInf = do
  w ← cpVar
  nOO ← cpSVarNGVarInfTail
  return $ case nOO of
    Some (Some n) → Inl $ Some n :* w
    Some None     → Inl $ None :* w
    None          → Inr w

cpSVarRaw ∷ CParser TokenBasic 𝕏
cpSVarRaw = concat
  [ do n ← cpDVarRaw
       return $ D_SVar n
  , do nww ← cpSVarNGVar
       return $ case nww of
         Inl (n :* w) → N_SVar n w
         Inr w        → G_SVar w
  ]

cpSVarRawInf ∷ CParser TokenBasic (𝕏 ∨ 𝑂 Name)
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

cpSVarInf ∷ CParser TokenBasic (𝕏 ∨ 𝑂 Name)
cpSVarInf = concat
  [ do nO ← cpDVarInf
       return $ case nO of
         None → Inr None
         Some n → Inl $ D_SVar n
  , do nww ← cpSVarNGVarInf
       return $ case nww of
         Inl (nO :* w) → case nO of
           None → Inr $ Some w
           Some n → Inl $ N_SVar n w
         Inr w → Inl $ G_SVar w

  ]

---------------------
-- PRETTY PRINTING --
---------------------

ppDVar ∷ 𝕊 → Doc
ppDVar n = concat [ppPun ":",ppString n]

ppNVar ∷ Doc → Doc → Doc
ppNVar n x = concat [x,ppPun ":",n]

instance Pretty 𝕏 where
  pretty = \case
    N_SVar n x → if n ≡ 0 then pretty x else ppNVar (pretty n) $ pretty x
    D_SVar n → ppDVar $ show𝕊 n
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

instance Shrinky 𝕏 where
  shrink = \case
    D_SVar n → D_SVar ^$ shrink n
    N_SVar n w → do
      n' ← shrink n
      return $ N_SVar n' w
    G_SVar _ → null

-- ======== --
-- SVarView --
-- ======== --

class SVarView s e | e→s where
  svarL ∷ s → e ⌲ 𝕏

svarScopeL ∷ ∀ s e. (SVarView s e) ⇒ s → 𝑂 Name → e ⌲ ℕ64
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
