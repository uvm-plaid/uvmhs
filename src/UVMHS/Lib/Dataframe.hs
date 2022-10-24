module UVMHS.Lib.Dataframe where

import UVMHS.Core

import UVMHS.Lib.Pretty

import qualified Data.Vector          as Vector
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv             as CSV
import qualified Data.Text.Encoding   as Text
import qualified Prelude              as HS
import qualified Text.Read            as HS

data 𝐹Type =
    U_𝐹T
  | B_𝐹T
  | N_𝐹T
  | Z_𝐹T
  | D_𝐹T
  | S_𝐹T
  deriving (Eq,Ord,Show)

makePrisms ''𝐹Type

data 𝐹Val =
    U_𝐹V
  | B_𝐹V 𝔹
  | N_𝐹V ℕ64
  | Z_𝐹V ℤ64
  | D_𝐹V 𝔻
  | S_𝐹V 𝕊
  deriving (Eq,Ord,Show)

makePrisms ''𝐹Val

data 𝐹GR = 𝐹GR
  { dataFrameGRRows ∷ ℕ64
  , dataFrameGRColP ∷ 𝑃 𝕊
  , dataFrameGRColV ∷ 𝕍 𝕊
  , dataFrameGRKeys ∷ 𝑃 𝕊
  , dataFrameGRData ∷ 𝐹Val ⇰ 𝕍 (𝕊 ⇰ 𝐹Val)
  } deriving (Eq,Ord,Show)

data 𝐹R = 𝐹R
  { dataFrameRRows ∷ ℕ64
  , dataFrameRColP ∷ 𝑃 𝕊
  , dataFrameRColV ∷ 𝕍 𝕊
  , dataFrameRData ∷ 𝕍 (𝕊 ⇰ 𝐹Val)
  } deriving (Eq,Ord,Show)

-- 𝐹GR --

product𝐹GR ∷ 𝐹GR → 𝐹GR → 𝑂 𝐹GR
product𝐹GR (𝐹GR _rows₁ colp₁ colv₁ ss₁ vsvss₁) (𝐹GR _rows₂ colp₂ colv₂ ss₂ vsvss₂) =
  let colp₁' = pow $ mapOn (iter colp₁) $ flip (⧺) "_L"
      colp₂' = pow $ mapOn (iter colp₂) $ flip (⧺) "_R"
      colv₁' = mapOn colv₁ $ flip (⧺) "_L"
      colv₂' = mapOn colv₂ $ flip (⧺) "_R"
  in
  if not $ isEmpty $ colp₁' ∩ colp₂'
  then None
  else Some $
    let colp = colp₁' ∪ colp₂'
        colv = colv₁' ⧺ colv₂'
        ss   = ss₁ ∪ ss₂
        vsvss = interWithOn vsvss₁ vsvss₂ $ \ svss₁ svss₂ → vec $ do
          svs₁ ← iter svss₁
          svs₂ ← iter svss₂
          return $ assoc $ concat
            [ mapOn (iter svs₁) $ \ (s :* v) → (s ⧺ "_L") :* v
            , mapOn (iter svs₂) $ \ (s :* v) → (s ⧺ "_R") :* v
            ]
        rows = sum $ map csize $ values vsvss
    in 𝐹GR rows colp colv ss vsvss

-- 𝐹R --

group𝐹R ∷ 𝕊 → 𝐹R → 𝑂 𝐹GR
group𝐹R s (𝐹R rows colp colv svss)
  | s ∉ colp = None
  | otherwise = Some $
    let vsvss = map vec $ concat $ mapOn (iter svss) $ \ svs →
          (svs ⋕! s) ↦ single𝐼 (without (single s) svs)
        colp' = colp ∖ single s
        colv' = vec $ filter (≢ s) colv
        ss  = single s
    in 𝐹GR rows colp' colv' ss vsvss

ungroup𝐹R ∷ 𝐹GR → 𝐹R
ungroup𝐹R (𝐹GR rows colp colv ss vsvss) =
  let svss = vec $ do
        v :* svs ← iter vsvss
        iter $ mapOn svs $ (⩌) $ assoc $ mapOn (iter ss) $ \ s → s :* v
      colp' = colp ∪ ss
      colv' = vec ss ⧺ colv 
  in 𝐹R rows colp' colv' svss

innerJoin𝐹R ∷ 𝕊 → 𝕊 → 𝐹R → 𝐹R → 𝑂 𝐹R
innerJoin𝐹R lid rid 𝑓₁ 𝑓₂ = do
  𝑓g₁ ← group𝐹R lid 𝑓₁
  𝑓g₂ ← group𝐹R rid 𝑓₂
  𝑓g₃ ← product𝐹GR 𝑓g₁ 𝑓g₂
  return $ ungroup𝐹R 𝑓g₃

parse𝐹Val ∷ 𝕊 → 𝐹Type → IO 𝐹Val
parse𝐹Val s = \case
  U_𝐹T → do
    if | s ≡ "()" → return U_𝐹V
       | otherwise → failIO $ "fail parse (unit):" ⧺ s
  B_𝐹T → do
    if | s ≡ "true"  → return $ B_𝐹V True
       | s ≡ "false" → return $ B_𝐹V False
       | otherwise   → failIO $ "fail parse (bool): " ⧺ s
  N_𝐹T → do
    case HS.readMaybe $ lazyList s of
      HS.Just n  → return $ N_𝐹V n
      HS.Nothing → failIO $ "fail parse (nat): " ⧺ s
  Z_𝐹T → do
    case HS.readMaybe $ lazyList s of
      HS.Just i  → return $ Z_𝐹V i
      HS.Nothing → failIO $ "fail parse (int): " ⧺ s
  D_𝐹T → do
    case HS.readMaybe $ lazyList s of
      HS.Just d  → return $ D_𝐹V d
      HS.Nothing → failIO $ "fail parse (dbl) " ⧺ s
  S_𝐹T → return $ S_𝐹V s

parseDataFrame ∷ 𝕊 → IO 𝐹R
parseDataFrame s = do
  sss ∷ 𝕍 (𝕍 𝕊) ← elimChoice (failIO ∘ string) (return ∘ map (map (Text.decodeUtf8 ∘ BSL.toStrict) ∘ 𝕍) ∘ 𝕍) $ frhs $ 
    CSV.decode @(Vector.Vector BSL.ByteString) CSV.NoHeader $ BSL.fromStrict $ Text.encodeUtf8 s
  cols ← ifNoneM (failIO "bad1") $ list ^$ sss ⋕? 0
  typs ← ifNoneM (failIO "bad2") $ list ^$ sss ⋕? 1
  let sss' = vecF (csize sss - 2) $ \ i → sss ⋕! (i + 2)
      rows = csize sss'
  typs' ← ifNoneM (failIO "bad3") $ mapMOn typs $ flip lup $ dict
    [ "bool"   ↦ B_𝐹T
    , "nat"    ↦ N_𝐹T
    , "int"    ↦ Z_𝐹T
    , "dbl" ↦ D_𝐹T
    , "string" ↦ S_𝐹T
    ]
  coltyps ← ifNoneM (failIO "bad4") $ zipSameLength cols typs'
  svss ← mapMOn sss' $ \ ss → do
    stss ← ifNoneM (failIO "unexpected row") $ zipSameLength coltyps $ list ss
    assoc ^$ mapMOn stss $ \ ((key :* t) :* sᵢ) → do
      v ← parse𝐹Val sᵢ t
      return $ key :* v
  return $ 𝐹R rows (pow cols) (vec cols) svss

instance Pretty 𝐹Val where
  pretty = \case
    U_𝐹V → pretty ()
    B_𝐹V b → pretty b
    N_𝐹V n → pretty n
    Z_𝐹V i → pretty i
    D_𝐹V d → pretty d
    S_𝐹V s → ppString s

instance Pretty 𝐹GR where
  pretty (𝐹GR _rows colp colv _ss vsvss) = ppRecord (ppPun "↦") $ mapOn (iter vsvss) $ \ (v :* svss) → do
    (:*) (ppLitFmt $ pretty v) $ pretty $ 𝐹R (csize svss) colp colv svss

instance Pretty 𝐹R where
  pretty (𝐹R rows _colp colv svss) =
    let svss' = mapp ppshow svss
        colWidths = mapOn colv $ \ col → 
          (:*) col $ joins 
            [ csize col
            , joins $ mapOn svss' $ \ svs → csize $ svs ⋕! col
            ]
    in 
    ppVertical
      [ ppHorizontal $ inbetween (ppComment "|") $ mapOn colWidths $ \ (col :* width) → ppCon $ alignLeft (nat width) col
      , ppComment $ string $ replicate (sum [sum $ map snd colWidths,((count colWidths ⊔ 1) - 1) × 3]) '-'
      , ppVertical $ mapOn svss' $ \ svs → 
          ppHorizontal $ inbetween (ppComment "|") $ mapOn (colWidths) $ \ (col :* width) → ppLit $ alignLeft (nat width) $ svs ⋕! col
      , concat [ppForceBreak,ppComment $ "⇈ ROWS: " ⧺ show𝕊 rows]
      ]
