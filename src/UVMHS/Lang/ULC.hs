module UVMHS.Lang.ULC where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Annotated
import UVMHS.Lib.Substitution
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky
import UVMHS.Lib.THLiftInstances ()

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote  as TH

import GHC.Generics as HS
import Control.Monad.Fail as HS

newtype ULCExp 𝒸 = ULCExp { unULCExp ∷ 𝐴 𝒸 (ULCExp_R 𝒸) }
  deriving (Eq,Generic,Ord,Show)

onULCExp ∷ (𝐴 𝒸 (ULCExp_R 𝒸) → 𝐴 𝒸' (ULCExp_R 𝒸')) → ULCExp 𝒸 → ULCExp 𝒸'
onULCExp f = ULCExp ∘ f ∘ unULCExp

data ULCExp_R 𝒸 =
    Var_ULC (UVar () (ULCExp 𝒸))
  | Lam_ULC (𝑂 Name) (ULCExp 𝒸)
  | App_ULC (ULCExp 𝒸) (ULCExp 𝒸)
  deriving (Eq,HS.Generic,Ord,Show)
makePrisms ''ULCExp_R

type ULCExpSrc = ULCExp (𝑃 SrcCxt)
type ULCExpRaw = ULCExp ()

wfULC ∷ ULCExp 𝒸 → 𝔹
wfULC = pipe (aval ∘ unULCExp) $ \case
  Var_ULC y → wfUVar y
  Lam_ULC _wO e → wfULC e
  App_ULC e₁ e₂ → and [wfULC e₁,wfULC e₂]

canonULC ∷ (Null 𝒸,Show 𝒸) ⇒ ULCExp 𝒸 → ULCExp 𝒸
canonULC = onULCExp $ mapAVal $ \case
  Var_ULC x → Var_ULC $ canonUVar canonULC x
  Lam_ULC xO e → Lam_ULC xO $ canonULC e
  App_ULC e₁ e₂ → App_ULC (canonULC e₁) $ canonULC e₂

syntaxULC ∷ Syntax
syntaxULC = concat
  [ syntaxUVar
  , syntaxPuns ["(",")","->","→"] 
  , syntaxKeys ["lam","λ"]
  ]

lULCExp ∷ Lexer
lULCExp = mkLexer $ LexerArgs False syntaxULC

pULCExp ∷ Parser ULCExpSrc
pULCExp = ULCExp ^$ mixfix single "exp" $ concat
  [ mixTerminal $ do
      pTokSyntax "("
      e ← pULCExp
      pTokSyntax ")"
      return $ aval $ unULCExp e
  , mixTerminal $ do
      x ← pUVar $ \ () → pULCExp
      return $ Var_ULC x
  , mixPrefix pLET $ do
      concat $ map pTokSyntax ["lam","λ"]
      xO ← optional $ pName
      concat $ map pTokSyntax ["->","→"]
      return $ \ e → Lam_ULC xO $ ULCExp e
  , mixInfixL pAPP $ return $ \ e₁ e₂ →
      App_ULC (ULCExp e₁) $ ULCExp e₂
  ]

instance (Show 𝒸) ⇒ Pretty (ULCExp 𝒸) where pretty = pretty ∘ aval ∘ unULCExp

instance (Show 𝒸) ⇒ Pretty (ULCExp_R 𝒸) where
  pretty = \case
    Var_ULC x → pretty x
    Lam_ULC xO e → flip (ppPreSep pLET) (pretty e) $ ppHorizontal $ concat
      [ single𝐼 $ ppKey "λ"
      , elim𝑂 null (single ∘ ppBdrFmt ∘ pretty) xO
      , single𝐼 $ ppKey "→"
      ]
    App_ULC e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂

instance Shrinky (ULCExp 𝒸) where
  shrink (ULCExp (𝐴 𝒸 e)) = ULCExp ∘ 𝐴 𝒸 ^$ shrink e
instance Shrinky (ULCExp_R 𝒸) where
  shrink = \case
    Var_ULC x → Var_ULC ^$ shrink x
    Lam_ULC xO e → concat
      [ single $ aval $ unULCExp e
      , do (xO',e') ← shrink (xO,e) ; return $ Lam_ULC xO' e'
      ]
    App_ULC e₁ e₂ → concat
      [ single $ aval $ unULCExp e₁
      , single $ aval $ unULCExp e₂
      , do (e₁',e₂') ← shrink (e₁,e₂) ; return $ App_ULC e₁' e₂'
      ]

deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCExp 𝒸)
deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCExp_R 𝒸)

ulc ∷ TH.QuasiQuoter
ulc = TH.QuasiQuoter qe qp qt qd
  where
    qe s = do
      l ← TH.location
      let lS = concat [frhsChars $ TH.loc_module l,":",show𝕊 $ fst $ frhs $ TH.loc_start l]
      case lexParse lULCExp pULCExp lS $ string s of
        Inl (Inl err) → do
          -- [hack] call to `replaced𝕊` required to make the whole error show
          -- up when using ghcid
          HS.fail $ tohsChars $ replace𝕊 "\n" "\n        " $ ppRender $ ppVertical
            [ ppHeader "[Lexing Failure]"
            , err
            ]
        Inl (Inr err) → 
          -- [hack] call to `replace𝕊` is required to make the whole error show
          -- up when using ghcid
          HS.fail $ tohsChars $ replace𝕊 "\n" "\n        " $ ppRender $ ppVertical
            [ ppHeader "[Parsing Failure]"
            , err
            ]
        Inr e → [| e |]
    qp = const $ HS.fail "quoting patterns not supported"
    qt = const $ HS.fail "quoting types not supported"
    qd = const $ HS.fail "quoting declarations not supported"

instance Fuzzy ULCExpRaw where
  fuzzy = do
    d ← askL fuzzyEnvDepthL
    ULCExp ∘ 𝐴 () ^$ wrchoose
      [ (:*) one $ \ () → Var_ULC ^$ fuzzy
      , (:*) d   $ \ () → rchoose
          [ \ () → do
                xO ← fuzzy
                e ← fuzzyRec
                return $ Lam_ULC xO e

          , \ () → do
              e₁ ← fuzzyRec
              e₂ ← fuzzyRec
              return $ App_ULC e₁ e₂
          ]
      ]

instance (Null 𝒸) ⇒ SVarView () (ULCExp 𝒸) where
  svarL () = 
    let ctor ∷ SVar → ULCExp 𝒸
        ctor = ULCExp ∘ 𝐴 null ∘ Var_ULC ∘ construct svar_UVarL
        dtor ∷ ULCExp 𝒸 → 𝑂 SVar
        dtor e = view (svar_UVarL ⊚ var_ULCL) $ aval $ unULCExp e
    in prism ctor dtor

instance (Null 𝒸,Show 𝒸) ⇒ Substy () (ULCExp 𝒸) (ULCExp 𝒸) where
  substy = pipe unULCExp $ \ (𝐴 𝒸 e₀) → ULCExp ^$ case e₀ of
    Var_ULC x → unULCExp ^$ substyUVar (ULCExp ∘  𝐴 𝒸 ∘ Var_ULC) () x
    Lam_ULC xO e → ureset $ do
      case xO of
        None → substyDBdr ()
        Some x → substyBdr () (ULCExp ∘ 𝐴 𝒸 ∘ Var_ULC ∘ uvar_SVar) x
      e' ← substy e
      return $ 𝐴 𝒸 $ Lam_ULC xO e'
    App_ULC e₁ e₂ → do
      e₁' ← substy e₁
      e₂' ← substy e₂
      return $ 𝐴 𝒸 $ App_ULC e₁' e₂'
