module UVMHS.Lang.ULCD where

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

newtype ULCDExp 𝒸 = ULCDExp { unULCDExp ∷ 𝐴 𝒸 (ULCDExp_R 𝒸) }
  deriving (Eq,Ord,Show)
data ULCDExp_R 𝒸 =
    Var_ULCD 𝕐
  | Lam_ULCD (𝑂 𝕏) (ULCDExp 𝒸)
  | App_ULCD (ULCDExp 𝒸) (ULCDExp 𝒸)
  deriving (Eq,Ord,Show)

type ULCDExpSrc = ULCDExp SrcCxt
type ULCDExpRaw = ULCDExp ()

lexULCDExp ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexULCDExp = lexerBasic (list ["(",")","->","→","^","↑"]) (list ["lam","λ"]) null null

pULCDExp ∷ CParser TokenBasic ULCDExpSrc
pULCDExp = ULCDExp ^$ fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      void $ cpSyntax "("
      e ← pULCDExp
      void $ cpSyntax ")"
      return $ aval $ unULCDExp e
  , fmixTerminal $ do
      n ← failEff ∘ natO64 *$ cpInteger
      return $ Var_ULCD $ DVar n
  , fmixTerminal $ do
      x ← cpVar
      n ← ifNone 0 ^$ cpOptional $ do
        void $ concat $ map cpSyntax ["^","↑"]
        failEff ∘ natO64 *$ cpInteger
      return $ Var_ULCD $ NVar n x
  , fmixPrefix pLET $ do
      void $ concat $ map cpSyntax ["lam","λ"]
      xO ← cpOptional $ cpVar
      void $ concat $ map cpSyntax ["->","→"]
      return $ \ e → Lam_ULCD xO $ ULCDExp e
  , fmixInfixL pAPP $ return $ \ e₁ e₂ → 
      App_ULCD (ULCDExp e₁) $ ULCDExp e₂
  ]

instance Pretty (ULCDExp 𝒸) where pretty = pretty ∘ aval ∘ unULCDExp

instance Pretty (ULCDExp_R 𝒸) where
  pretty = \case
    Var_ULCD x → pretty x
    Lam_ULCD xO e → flip (ppPreSep pLET) (pretty e) $ ppHorizontal $ concat
      [ single𝐼 $ ppKey "λ"
      , elim𝑂 null (single ∘ ppBdrFmt ∘ pretty) xO
      , single𝐼 $ ppKey "→"
      ]
    App_ULCD e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂

deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCDExp 𝒸)
deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCDExp_R 𝒸)

ulcd ∷ TH.QuasiQuoter
ulcd = TH.QuasiQuoter qe qp qt qd
  where
    qe s = do
      let sourceName = ""
      ts ← io $ tokenizeIO lexULCDExp sourceName $ tokens $ frhsChars s
      let eC = parse pULCDExp sourceName ts
      case eC of
        Inl r → do
          TH.reportError $ tohsChars $ ppRenderNofmt r
          HS.fail "Parse Failure"
        Inr e → [| e |]
    qp = const $ HS.fail "quoting patterns not supported"
    qt = const $ HS.fail "quoting types not supported"
    qd = const $ HS.fail "quoting declarations not supported"

instance Fuzzy ULCDExpRaw where
  fuzzy = do
    d ← askL fuzzyEnvDepthL
    ULCDExp ∘ 𝐴 () ^$ wrchoose
      [ (:*) one $ \ () → Var_ULCD ^$ fuzzy
      , (:*) d   $ \ () → rchoose
          [ \ () → do
                xO ← fuzzy
                e ← fuzzyRec fuzzy
                return $ Lam_ULCD xO e

          , \ () → do
              e₁ ← fuzzyRec fuzzy
              e₂ ← fuzzyRec fuzzy
              return $ App_ULCD e₁ e₂
          ]
      ]

instance Substy () (ULCDExp 𝒸) (ULCDExp 𝒸) where
  substy = pipe unULCDExp $ \ (𝐴 𝒸 e₀) → ULCDExp ^$ case e₀ of
    Var_ULCD x → unULCDExp ^$ substy𝕐 () (ULCDExp ∘  𝐴 𝒸 ∘ Var_ULCD) x
    Lam_ULCD xO e → ureset $ do
      case xO of
        None → substyDBdr ()
        Some x → substyBdr () x
      e' ← substy e
      return $ 𝐴 𝒸 $ Lam_ULCD xO e'
    App_ULCD e₁ e₂ → do
      e₁' ← substy e₁
      e₂' ← substy e₂
      return $ 𝐴 𝒸 $ App_ULCD e₁' e₂'
