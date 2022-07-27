module UVMHS.Lang.ULCD where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.Annotated
import UVMHS.Lib.Window
import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.OldVariables

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote  as TH

import Control.Monad.Fail as HS

newtype ULCDExp 𝒸 = ULCDExp { unULCDExp ∷ 𝐴 𝒸 (ULCDExp_R 𝒸) }
  deriving (Eq,Ord,Show)
data ULCDExp_R 𝒸 =
    Var_ULCD 𝕐
  | Lam_ULCD (ULCDExp 𝒸)
  | App_ULCD (ULCDExp 𝒸) (ULCDExp 𝒸)
  deriving (Eq,Ord,Show)

type ULCDExpS = ULCDExp SrcCxt
type ULCDExpR = ULCDExp ()

lexULCDExp ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexULCDExp = lexerBasic (list ["(",")","->","→"]) (list ["lam","λ"]) null null

pULCDExp ∷ CParser TokenBasic ULCDExpS
pULCDExp = ULCDExp ^$ fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      void $ cpToken $ SyntaxTBasic "("
      e ← pULCDExp
      void $ cpToken $ SyntaxTBasic ")"
      return $ aval $ unULCDExp e
  , fmixTerminal $ do
      i ← cpInteger
      return $ Var_ULCD $ BoundVar $ natΩ64 i
  , fmixTerminal $ do
      x ← cpVar
      return $ Var_ULCD $ NamedVar x
  , fmixPrefix pLET $ do
      void $ concat $ map cpSyntax ["lam","λ"]
      void $ concat $ map cpSyntax ["->","→"]
      return $ \ e → Lam_ULCD $ ULCDExp e
  , fmixInfixL pAPP $ return $ \ e₁ e₂ → 
      App_ULCD (ULCDExp e₁) $ ULCDExp e₂
  ]

instance Pretty (ULCDExp 𝒸) where pretty = pretty ∘ aval ∘ unULCDExp

instance Pretty (ULCDExp_R 𝒸) where
  pretty = \case
    Var_ULCD x → pretty x
    Lam_ULCD e → ppPreSep pLET (ppKey "λ →") $ pretty e
    App_ULCD e₁ e₂ → ppInfl pAPP (ppSpace one) (pretty e₁) $ pretty e₂

deriving instance (TH.Lift a) ⇒ TH.Lift (AddBT a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (WindowL i a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (WindowR i a)
deriving instance TH.Lift SrcCxt
deriving instance TH.Lift LocRange
deriving instance (TH.Lift 𝒸,TH.Lift a) ⇒ TH.Lift (𝐴 𝒸 a)
deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCDExp 𝒸)
deriving instance TH.Lift Loc
deriving instance TH.Lift 𝕏
deriving instance TH.Lift 𝕐
deriving instance (TH.Lift 𝒸) ⇒ TH.Lift (ULCDExp_R 𝒸)
deriving instance (TH.Lift a,TH.Lift b) ⇒ TH.Lift (a ∧ b)
deriving instance TH.Lift Annotation
deriving instance TH.Lift Formats
deriving instance TH.Lift Color
deriving instance TH.Lift Color3Bit
deriving instance TH.Lift ChunkI
deriving instance (TH.Lift a) ⇒ TH.Lift (𝑂 a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (𝑇 i a)

instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (𝑇V i a) where
  liftTyped t = do
    let t' = fold𝑇VWith single annote t
    [|| fold𝑇With single annote t' ||]

instance (TH.Lift a) ⇒ TH.Lift (𝐼 a) where
  liftTyped xs = do
    let xs' = list xs
    [|| iter xs' ||]

instance TH.Lift Doc where
  liftTyped d = do
    let d' = ppBake d
    [|| ppEmbed d' ||]

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
