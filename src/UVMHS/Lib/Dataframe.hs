module UVMHS.Lib.Dataframe where

import UVMHS.Core

import UVMHS.Lib.Pretty

import qualified Data.Vector          as Vector
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv             as CSV
import qualified Data.Text.Encoding   as Text
import qualified Prelude              as HS
import qualified Text.Read            as HS

data FrameType =
    B_FT
  | N_FT
  | Z_FT
  | D_FT
  | S_FT
  deriving (Eq,Ord,Show)
makePrisms ''FrameType

frameTypeCode ∷ FrameType → 𝕊
frameTypeCode = \case
  B_FT → "bool"
  N_FT → "nat"
  Z_FT → "int"
  D_FT → "dbl"
  S_FT → "str"

data FrameVal = 
    B_FV 𝔹
  | N_FV ℕ64
  | Z_FV ℤ64
  | D_FV 𝔻
  | S_FV 𝕊
  deriving (Eq,Ord,Show)
makePrisms ''FrameVal
makePrettyUnion ''FrameVal

data FrameCol =
    B_FC (𝕌 𝔹)
  | N_FC (𝕌 ℕ64)
  | Z_FC (𝕌 ℤ64)
  | D_FC (𝕌 𝔻)
  | S_FC (𝕍 𝕊)
  deriving (Eq,Ord,Show)
makePrisms ''FrameCol

frameColType ∷ FrameCol → FrameType
frameColType = \case
  B_FC _ → B_FT
  N_FC _ → N_FT
  Z_FC _ → Z_FT
  D_FC _ → D_FT
  S_FC _ → S_FT

frameColPack ∷ FrameType → 𝐼C FrameVal → 𝑂 FrameCol
frameColPack t vs = case t of
  B_FT → map (B_FC ∘ uvecC) $ exchange $ map (view b_FVL) vs
  N_FT → map (N_FC ∘ uvecC) $ exchange $ map (view n_FVL) vs
  Z_FT → map (Z_FC ∘ uvecC) $ exchange $ map (view z_FVL) vs
  D_FT → map (D_FC ∘ uvecC) $ exchange $ map (view d_FVL) vs
  S_FT → map (S_FC ∘ vecC) $ exchange $ map (view s_FVL) vs

frameColUnpack ∷ FrameCol → 𝐼C FrameVal
frameColUnpack = \case
  B_FC vs → B_FV ^$ iterC vs
  N_FC vs → N_FV ^$ iterC vs
  Z_FC vs → Z_FV ^$ iterC vs
  D_FC vs → D_FV ^$ iterC vs
  S_FC vs → S_FV ^$ iterC vs

frameColIndex ∷ ℕ64 → FrameCol → 𝑂 FrameVal
frameColIndex n = \case
  B_FC vs → B_FV ^$ vs ⋕? n
  N_FC vs → N_FV ^$ vs ⋕? n
  Z_FC vs → Z_FV ^$ vs ⋕? n
  D_FC vs → D_FV ^$ vs ⋕? n
  S_FC vs → S_FV ^$ vs ⋕? n

data FrameGrouping v =
    B_FG (𝔹 ⇰ v)
  | N_FG (ℕ64 ⇰ v)
  | Z_FG (ℤ64 ⇰ v)
  | D_FG (𝔻 ⇰ v)
  | S_FG (𝕊 ⇰ v)
  deriving (Eq,Ord,Show)
makePrettyUnion ''FrameGrouping

instance Functor FrameGrouping where
  map f = \case
   B_FG kvs → B_FG $ map f kvs
   N_FG kvs → N_FG $ map f kvs
   Z_FG kvs → Z_FG $ map f kvs
   D_FG kvs → D_FG $ map f kvs
   S_FG kvs → S_FG $ map f kvs

instance FunctorM FrameGrouping where
  mapM f = \case
   B_FG kvs → B_FG ^$ mapM f kvs
   N_FG kvs → N_FG ^$ mapM f kvs
   Z_FG kvs → Z_FG ^$ mapM f kvs
   D_FG kvs → D_FG ^$ mapM f kvs
   S_FG kvs → S_FG ^$ mapM f kvs

frameGroupingInterWithM
  ∷ (Monad m,MonadFail m) 
  ⇒ (v₁ → v₂ → m v₃) 
  → FrameGrouping v₁ 
  → FrameGrouping v₂ 
  → m (FrameGrouping v₃)
frameGroupingInterWithM f vs₁ vs₂ = case (vs₁,vs₂) of
  (B_FG kvs₁,B_FG kvs₂) → B_FG ^$ dinterByM f kvs₁ kvs₂
  (N_FG kvs₁,N_FG kvs₂) → N_FG ^$ dinterByM f kvs₁ kvs₂
  (Z_FG kvs₁,Z_FG kvs₂) → Z_FG ^$ dinterByM f kvs₁ kvs₂
  (D_FG kvs₁,D_FG kvs₂) → D_FG ^$ dinterByM f kvs₁ kvs₂
  (S_FG kvs₁,S_FG kvs₂) → S_FG ^$ dinterByM f kvs₁ kvs₂
  _ → abort

data FrameData =
    Vec_FD ℕ64 (𝕊 ⇰ FrameCol)
  | Grp_FD 𝕊 (FrameGrouping FrameData)
  deriving (Eq,Ord,Show)

data Frame = Frame
  { frameColP ∷ 𝑃 𝕊
  , frameColV ∷ 𝕍 𝕊
  , frameColT ∷ 𝕊 ⇰ FrameType
  , frameGrpT ∷ 𝕊 ⇰ FrameType
  , frameData ∷ (𝕊 ⇰ FrameVal) ⇰ ℕ64 ∧ (𝕊 ⇰ FrameCol)
  } deriving (Eq,Ord,Show)

frameProduct ∷ Frame → Frame → 𝑂 Frame
frameProduct fr₁ fr₂ = do
  let Frame colp₁ colv₁ colt₁ grpt₁ data₁ = fr₁
      Frame colp₂ colv₂ colt₂ grpt₂ data₂ = fr₂
      colp₁' ∷ 𝑃 𝕊
      colp₁' = pow $ mapOn (iter colp₁) $ flip (⧺) "_L"
      colp₂' ∷ 𝑃 𝕊
      colp₂' = pow $ mapOn (iter colp₂) $ flip (⧺) "_R"
      colv₁' ∷ 𝕍 𝕊
      colv₁' = mapOn colv₁ $ flip (⧺) "_L"
      colv₂' ∷ 𝕍 𝕊
      colv₂' = mapOn colv₂ $ flip (⧺) "_R"
      colt₁' ∷ 𝕊 ⇰ FrameType
      colt₁' = assoc $ mapOn (iter colt₁) $ mapFst $ flip (⧺) "_L"
      colt₂' ∷ 𝕊 ⇰ FrameType
      colt₂' = assoc $ mapOn (iter colt₂) $ mapFst $ flip (⧺) "_R"
      colp'  ∷ 𝑃 𝕊
      colp'  = colp₁' ∪ colp₂'
      colv'  ∷ 𝕍 𝕊
      colv'  = colv₁' ⧺ colv₂'
      colt'  ∷ 𝕊 ⇰ FrameType
      colt'  = colt₁' ⩌ colt₂'
  grpt' ∷ 𝕊 ⇰ FrameType
        ← dinterByM (\ τ₁ τ₂ → do guard $ τ₁ ≡ τ₂ ; return τ₁) grpt₁ grpt₂
  let data' = dinterByOn data₁ data₂ $ \ (n₁ :* svss₁) (n₂ :* svss₂) → 
        let svss₁'₁ ∷ 𝕊 ⇰ FrameCol
            svss₁'₁ = assoc $ mapOn (iter svss₁) $ mapFst $ flip (⧺) "_L"
            svss₂'₁ ∷ 𝕊 ⇰ FrameCol

            svss₂'₁ = assoc $ mapOn (iter svss₂) $ mapFst $ flip (⧺) "_R"
            svss₁'₂ ∷ 𝐼C (𝕊 ⇰ FrameVal)
            svss₁'₂ = mapOn (uptoC n₁) $ \ n → mapOn svss₁'₁ $ viewΩ someL ∘ frameColIndex n
            svss₂'₂ ∷ 𝐼C (𝕊 ⇰ FrameVal)
            svss₂'₂ = mapOn (uptoC n₂) $ \ n → mapOn svss₂'₁ $ viewΩ someL ∘ frameColIndex n
            svss'₁ ∷ 𝕍 (𝕊 ⇰ FrameVal)
            svss'₁ = vecC $ prodWith𝐼C (⩌) svss₁'₂ svss₂'₂
            svss'₂ ∷ 𝕊 ⇰ FrameCol

            rows = csize svss'₁

            svss'₂ = kmapOn colt' $ \ s τ → 
              viewΩ someL $ frameColPack τ $ mapOn (iterC svss'₁) $ lupΩ s
        in rows :* svss'₂
  return $ Frame colp' colv' colt' grpt' data'

frameGroup ∷ 𝕊 → 𝕊 → Frame → 𝑂 Frame
frameGroup col s₀ (Frame colp colv colt grpt data') = do
  guard $ col ∈ colp
  guard $ not $ s₀ ⋿ grpt
  return $
    let colp' ∷ 𝑃 𝕊
        colp' = colp ∖ single col
        colv' ∷ 𝕍 𝕊
        colv' = vec $ filter (≢ col) colv
        colt' ∷ 𝕊 ⇰ FrameType
        colt' = dtoss (single col) colt 
        grpt' ∷ 𝕊 ⇰ FrameType
        grpt' = dict [s₀ ↦ colt ⋕! col,grpt]
        data'₁ ∷ (𝕊 ⇰ FrameVal) ⇰ FrameVal ⇰ ℕ64 ∧ (𝕊 ⇰ FrameCol)
        data'₁ = mapOn data' $ \ (n :* svss) →
          let svs ∷ FrameCol
              svs = svss ⋕! col
              svss'₁ ∷ 𝕊 ⇰ FrameCol
              svss'₁ = dtoss (single col) svss
              svss'₂ ∷ FrameVal ⇰ 𝐼C (𝕊 ⇰ FrameVal)
              svss'₂ = concat $ mapOn (upto n) $ \ nᵢ → 
                let vᵢ   = viewΩ someL $ frameColIndex nᵢ svs
                    svsᵢ = mapOn svss'₁ $ viewΩ someL ∘ frameColIndex nᵢ
                in vᵢ ↦ single svsᵢ
              svss'₃ ∷ FrameVal ⇰ ℕ64 ∧ (𝕊 ⇰ FrameCol)
              svss'₃ = mapOn svss'₂ $ \ svssᵢ →
                let rows = csize svssᵢ
                    svsᵢ = kmapOn colt' $ \ s τ → 
                      viewΩ someL $ frameColPack τ $ mapOn svssᵢ $ lupΩ s
                in rows :* svsᵢ
          in svss'₃
        data'₂ ∷ (𝕊 ⇰ FrameVal) ⇰ ℕ64 ∧ (𝕊 ⇰ FrameCol)
        data'₂ = assoc $ do
          svs :* vnsvss ← iter data'₁
          v :* nsvss ← iter vnsvss
          return $ dict [s₀ ↦ v,svs] :* nsvss
    in
    Frame colp' colv' colt' grpt' data'₂

frameUngroup ∷ 𝕊 → 𝕊 → Frame → 𝑂 Frame
frameUngroup grp s₀ (Frame colp colv colt grpt data') = do
  guard $ grp ⋿ grpt
  guard $ not $ s₀ ∈ colp
  return $
    let colp' ∷ 𝑃 𝕊
        colp' = single s₀ ∪ colp
        colv' ∷ 𝕍 𝕊
        colv' = single s₀ ⧺ colv
        colt' ∷ 𝕊 ⇰ FrameType
        colt' = dict [s₀ ↦ grpt ⋕! grp,colt]
        grpt' ∷ 𝕊 ⇰ FrameType
        grpt' = dtoss (single grp) grpt
        data'₁ ∷ (𝕊 ⇰ FrameVal) ⇰ ℕ64 ∧ (𝕊 ⇰ 𝐼C FrameVal)
        data'₁ = concat $ mapOn (iter data') $ \ (svs :* (n :* svss)) → 
          let svs' ∷ 𝕊 ⇰ FrameVal
              svs' = dtoss (single grp) svs 
              v ∷ FrameVal
              v = svs ⋕! grp
              svss' ∷ 𝕊 ⇰ 𝐼C FrameVal
              svss' = dict
                [ (↦) s₀ $ mapOn (uptoC n) $ const v
                , map frameColUnpack svss
                ]
          in
          svs' ↦ n :* svss'
        data'₂ ∷ (𝕊 ⇰ FrameVal) ⇰ ℕ64 ∧ (𝕊 ⇰ FrameCol)
        data'₂ = mapOn data'₁ $ \ (n :* svss) → 
          let svss' ∷ 𝕊 ⇰ FrameCol
              svss' = kmapOn svss $ \ s vs →
                let τ ∷ FrameType
                    τ = colt' ⋕! s
                    vs' ∷ FrameCol
                    vs' = viewΩ someL $ frameColPack τ vs
                in vs'
          in
          n :* svss'
    in
    Frame colp' colv' colt' grpt' data'₂

frameValParse ∷ 𝕊 → FrameType → IO FrameVal
frameValParse s = \case
  B_FT → do
    if | s ≡ "true"  → return $ B_FV True
       | s ≡ "false" → return $ B_FV False
       | otherwise   → failIO $ "fail parse (bool): " ⧺ s
  N_FT → do
    case HS.readMaybe $ lazyList s of
      HS.Just n  → return $ N_FV n
      HS.Nothing → failIO $ "fail parse (nat): " ⧺ s
  Z_FT → do
    case HS.readMaybe $ lazyList s of
      HS.Just i  → return $ Z_FV i
      HS.Nothing → failIO $ "fail parse (int): " ⧺ s
  D_FT → do
    case HS.readMaybe $ lazyList s of
      HS.Just d  → return $ D_FV d
      HS.Nothing → failIO $ "fail parse (dbl) " ⧺ s
  S_FT → return $ S_FV s

frameParse ∷ 𝕊 → IO Frame
frameParse s = do
  sss ∷ 𝕍 (𝕍 𝕊) ← 
    elimChoice (failIO ∘ string) (return ∘ map (map (Text.decodeUtf8 ∘ BSL.toStrict) ∘ 𝕍) ∘ 𝕍) $ 
      frhs $ CSV.decode @(Vector.Vector BSL.ByteString) CSV.NoHeader $ 
        BSL.fromStrict $ Text.encodeUtf8 s
  cols ∷ 𝐿 𝕊 ← ifNoneM (failIO "bad1") $ list ^$ sss ⋕? 0
  typs ∷ 𝐿 𝕊 ← ifNoneM (failIO "bad2") $ list ^$ sss ⋕? 1
  let sss' ∷ 𝕍 (𝕍 𝕊)
      sss' = vecF (csize sss - 2) $ \ i → sss ⋕! (i + 2)
      rows ∷ ℕ64
      rows = csize sss'
  typs' ∷ 𝐿 FrameType ← ifNoneM (failIO "bad3") $ mapMOn typs $ flip lup $ dict @((⇰) _)
    [ frameTypeCode B_FT ↦ B_FT
    , frameTypeCode N_FT ↦ N_FT
    , frameTypeCode Z_FT ↦ Z_FT
    , frameTypeCode D_FT ↦ D_FT
    , frameTypeCode S_FT ↦ S_FT
    ]
  coltyps ∷ 𝐿 (𝕊 ∧ FrameType) ← ifNoneM (failIO "bad4") $ zipSameLength cols typs'
  let coltyps' ∷ 𝕊 ⇰ FrameType
      coltyps' = assoc coltyps
  svss ∷ 𝕍 (𝕊 ⇰ FrameVal) ← mapMOn sss' $ \ ss → do
    stss ← ifNoneM (failIO "unexpected row") $ zipSameLength coltyps $ list ss
    assoc ^$ mapMOn stss $ \ ((key :* t) :* sᵢ) → do
      v ← frameValParse sᵢ t
      return $ key :* v
  let svss' ∷ 𝕊 ⇰ FrameCol
      svss' = kmapOn coltyps' $ \ sᵢ τ → 
        viewΩ someL $ frameColPack τ $ mapOn (iterC svss) $ lupΩ sᵢ
  return $ Frame (pow cols) (vec cols) (assoc coltyps) null $ null ↦ (rows :* svss')

instance Pretty Frame where
  pretty (Frame _colp colv colt grps data') = 
    let data'' = mapOn data' $ \ (rows :* svss) → 
          let svss' ∷ 𝕊 ⇰ 𝕍 𝕊
              svss' = map (vecC ∘ map ppshow ∘ frameColUnpack) svss
              colWidths ∷ 𝕍 (𝕊 ∧ ℕ64)
              colWidths = mapOn colv $ \ col → 
                (:*) col $ joins
                  [ csize col
                  , csize $ frameTypeCode $ colt ⋕! col
                  , joins $ map csize $ svss' ⋕! col
                  ]
          in 
          concat
            [ ppForceBreak
            , ppVertical
                [ ppHorizontal $ inbetween (ppComment "|") $ mapOn colWidths $ \ (col :* width) → 
                    ppCon $ alignLeft (nat width) col
                , ppComment $ string $ 
                    replicate (sum [sum $ map snd colWidths,(count colWidths ⊔ 1 - 1) × 3]) '-'
                , ppHorizontal $ inbetween (ppComment "|") $ mapOn colWidths $ \ (col :* width) →
                    ppComment $ alignLeft (nat width) $ frameTypeCode $ colt ⋕! col
                , ppComment $ string $ 
                    replicate (sum [sum $ map snd colWidths,(count colWidths ⊔ 1 - 1) × 3]) '-'
                , ppVertical $ mapOn (upto rows) $ \ n →
                    ppHorizontal $ inbetween (ppComment "|") $ mapOn colWidths $ \ (col :* width) →
                      ppLit $ alignLeft (nat width) $ (svss' ⋕! col) ⋕! n
                , ppComment $ "⇈ ROWS: " ⧺ show𝕊 rows
                ]
            ]
    in 
    if
    | isEmpty grps → pretty $ data'' ⋕! null
    | otherwise    → pretty data''

