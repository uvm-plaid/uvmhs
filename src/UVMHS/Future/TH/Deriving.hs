module UVMHS.Future.TH.Deriving where

import UVMHS.Core
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Pretty
import UVMHS.Future.TH

import qualified Language.Haskell.TH as TH

---------------------
-- DERIVING MONOID --
---------------------

createMonoidInstance ∷ TH.Name → TH.DecsQ
createMonoidInstance name = do
  𝒾 ← adtProdInfo "createMonoidInstance" name
  [d| instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Null $τ |])
        ⇒
        Null $(adtProdInfoFullTypeQ 𝒾)
        where
          null = $(adtProdInfoConsQ 𝒾 $ \ mk τs →
            foldOnFrom τs mk $ \ τ eQ → [| $eQ (null @($τ)) |])
      instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Append $τ |])
        ⇒
        Append $(adtProdInfoFullTypeQ 𝒾)
        where
          x ⧺ y = $(adtProdInfoLetQ 𝒾 [| x |] $ \ xs → adtProdInfoLetQ 𝒾 [| y |] $ \ ys →
            adtProdInfoConsQ 𝒾 $ \ mk _τs →
              foldOnFrom (zip xs ys) mk $ uncurry $ \ xᵢ yᵢ eQ → [| $eQ ($xᵢ ⧺ $yᵢ) |])
      instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Monoid $τ |])
        ⇒
        Monoid $(adtProdInfoFullTypeQ 𝒾)
   |]

--------------------
-- DERIVING FUZZY --
--------------------

createFuzzyInstance ∷ [𝕊] → TH.Name → TH.DecsQ
createFuzzyInstance recNamesS name = do
  when (count @ℕ64 (pow𝑃 recNamesS) ≢ count recNamesS) $ \ () →
    fail $ err_MSG_DUPLICATE_REC_NAMES ()
  𝒾 ← adtInfo "createFuzzyInstance" name
  let nameMap ∷ 𝕊 ⇰ TH.Name
      nameMap = adtInfoTypeArgsNameMap 𝒾
  recNames ∷ 𝑃 TH.Name
            ← pow ^$ mapMOn recNamesS $ \ nameᵢ → do
    case nameMap ⋕? nameᵢ of
      None → fail $ err_MSG_INVALID_REC_NAME nameMap
      Some nameᵣ → return nameᵣ
  let recNames' ∷ 𝑃 TH.Name
      recNames' = recNames ∪ single name
  [d| instance 
        $(thTupsTQ $ mapOn (adtInfoAllConArgsQ 𝒾) $ \ τ → [t| Fuzzy $τ |])
        ⇒
        Fuzzy $(adtInfoFullTypeQ 𝒾) 
        where
          fuzzy = $(do
            d ← thGensymS "d"
            es ← TH.listE $ adtInfoConssQ 𝒾 $ \ mk τs → do
              (con :* (anyRec :* nonRec :* stmts)) 
                ∷ TH.ExpQ ∧ (𝔹 ∧ ℕ64 ∧ 𝐼 TH.StmtQ)
                ← evalRWST () mk $ 
                    retStateOut $ 
                      eachOn (withIndex τs) $ \ (n :* τ) → UVMHS.Core.do
                x' ← lift $ thGensymN n
                modify $ \ eQ → [| $eQ $(TH.varE x') |]
                isRec ∷ 𝔹
                      ← lift $ thAnyNameOccursInType recNames' ^$ τ
                tellL (fstL ⊚ fstL) isRec
                when (not isRec) $ \ () →
                  tellL (sndL ⊚ fstL) one
                tellL sndL $ single $ TH.bindS (TH.varP x') $ if not isRec then [| fuzzy @($τ) |] else [| fuzzyRec @($τ) |]
              when (anyRec ⩓ nonRec ≡ zero) $ \ () →
                fail $ "not ok to have only recursive fields in constructor: " ⧺ show anyRec ⧺ " " ⧺ show nonRec
              let weight = if anyRec then [| $(TH.varE d) |] else [| one |]
              [| \ () → $(weight) :* $(TH.doE $ lazyList $ concat [stmts,single $ TH.noBindS [| return $con |]]) |]
            [| do $(TH.varP d) ← fuzzyDepth
                  wrchoose $(return es)
             |])
   |]
  where
    err_MSG_DUPLICATE_REC_NAMES ∷ () → [ℂ]
    err_MSG_DUPLICATE_REC_NAMES () = concat $ inbetween " "
      [ "createFuzzyInstance:"
      , "[["
      , TH.pprint name
      , ","
      , lazyList $ ppRenderNoFmt $ pretty recNamesS
      , "]]"
      , "the list of provided names"
      , "should not include any duplicates"
      , "(purely as a sanity check)."
      ]
    err_MSG_INVALID_REC_NAME ∷ 𝕊 ⇰ TH.Name → [ℂ]
    err_MSG_INVALID_REC_NAME nameMap = concat $ inbetween " " $
      [ "createFuzzyInstance:"
      , "[["
      , TH.pprint name
      , ","
      , lazyList $ ppRenderNoFmt $ pretty recNamesS
      , "]]"
      , "each provided name must must match"
      , "the name of a type variable argument"
      , "for the declared data type."
      , "valid options are:"
      , show $ dkeys nameMap
      ]


-------------------
-- DERIVING LENS --
-------------------

-- createLensDefs ∷ TH.Name → TH.DecsQ
-- createLensDefs name = do
--   𝒾 ← adtRecordInfo name
--   [d| instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Null $τ |])
--         ⇒
--         Null $(adtProdInfoFullTypeQ 𝒾)
--         where
--           null = $(adtProdInfoConsQ 𝒾 $ \ mk τs →
--             foldOnFrom τs mk $ \ τ eQ → [| $eQ (null @($τ)) |])
--       instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Append $τ |])
--         ⇒
--         Append $(adtProdInfoFullTypeQ 𝒾)
--         where
--           x ⧺ y = $(adtProdInfoLetQ 𝒾 [| x |] $ \ xs → adtProdInfoLetQ 𝒾 [| y |] $ \ ys →
--             adtProdInfoConsQ 𝒾 $ \ mk _τs →
--               foldOnFrom (zip xs ys) mk $ uncurry $ \ xᵢ yᵢ eQ → [| $eQ ($xᵢ ⧺ $yᵢ) |])
--       instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Monoid $τ |])
--         ⇒
--         Monoid $(adtProdInfoFullTypeQ 𝒾)
--    |]
