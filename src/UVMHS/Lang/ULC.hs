module UVMHS.Lang.ULC where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Annotated
import UVMHS.Lib.Variables
import UVMHS.Lib.Substitution
import UVMHS.Lib.Rand
import UVMHS.Lib.THLiftInstances ()

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote  as TH

import Control.Monad.Fail as HS

newtype ULCExp 𝒸 = ULCExp { unULCExp ∷ 𝐴 𝒸 (ULCExp_R 𝒸) }
  deriving (Eq,Ord,Show)
data ULCExp_R 𝒸 =
    Var_ULC 𝕐
  | Lam_ULC (𝑂 𝕏) (ULCExp 𝒸)
  | App_ULC (ULCExp 𝒸) (ULCExp 𝒸)
  deriving (Eq,Ord,Show)

type ULCExpSrc = ULCExp SrcCxt
type ULCExpRaw = ULCExp ()

lexULCExp ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexULCExp = lexerBasic (list ["(",")","->","→","^","↑"]) (list ["lam","λ"]) null null

pULCExp ∷ CParser TokenBasic ULCExpSrc
pULCExp = ULCExp ^$ fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      void $ cpSyntax "("
      e ← pULCExp
      void $ cpSyntax ")"
      return $ aval $ unULCExp e
  , fmixTerminal $ do
      n ← failEff ∘ natO64 *$ cpInteger
      return $ Var_ULC $ DVar n
  , fmixTerminal $ do
      x ← cpVar
      n ← ifNone 0 ^$ cpOptional $ do
        void $ concat $ map cpSyntax ["^","↑"]
        failEff ∘ natO64 *$ cpInteger
      return $ Var_ULC $ NVar n x
  , fmixPrefix pLET $ do
      void $ concat $ map cpSyntax ["lam","λ"]
      xO ← cpOptional $ cpVar
      void $ concat $ map cpSyntax ["->","→"]
      return $ \ e → Lam_ULC xO $ ULCExp e
  , fmixInfixL pAPP $ return $ \ e₁ e₂ → 
      App_ULC (ULCExp e₁) $ ULCExp e₂
  ]

instance Pretty (ULCExp 𝒸) where pretty = pretty ∘ aval ∘ unULCExp

instance Pretty (ULCExp_R 𝒸) where
  pretty = \case
    Var_ULC x → pretty x
    Lam_ULC xO e → flip (ppPreSep pLET) (pretty e) $ ppHorizontal $ concat
      [ single𝐼 $ ppKey "λ"
      , elim𝑂 null (single ∘ ppBdrFmt ∘ pretty) xO
      , single𝐼 $ ppKey "→"
      ]
    App_ULC e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂

deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCExp 𝒸)
deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCExp_R 𝒸)

ulc ∷ TH.QuasiQuoter
ulc = TH.QuasiQuoter qe qp qt qd
  where
    qe s = do
      let sourceName = ""
      ts ← io $ tokenizeIO lexULCExp sourceName $ tokens $ frhsChars s
      let eC = parse pULCExp sourceName ts
      case eC of
        Inl r → do
          TH.reportError $ tohsChars $ ppRenderNofmt r
          HS.fail "Parse Failure"
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
                e ← fuzzyRec fuzzy
                return $ Lam_ULC xO e

          , \ () → do
              e₁ ← fuzzyRec fuzzy
              e₂ ← fuzzyRec fuzzy
              return $ App_ULC e₁ e₂
          ]
      ]

instance Substy () (ULCExp 𝒸) (ULCExp 𝒸) where
  substy = pipe unULCExp $ \ (𝐴 𝒸 e₀) → ULCExp ^$ case e₀ of
    Var_ULC x → unULCExp ^$ substy𝕐 () (ULCExp ∘  𝐴 𝒸 ∘ Var_ULC) x
    Lam_ULC xO e → ureset $ do
      case xO of
        None → substyDBdr ()
        Some x → substyBdr () x $ ULCExp ∘ 𝐴 𝒸 ∘ Var_ULC
      e' ← substy e
      return $ 𝐴 𝒸 $ Lam_ULC xO e'
    App_ULC e₁ e₂ → do
      e₁' ← substy e₁
      e₂' ← substy e₂
      return $ 𝐴 𝒸 $ App_ULC e₁' e₂'
