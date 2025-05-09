module UVMHS.Lib.Pretty.Levels where

import UVMHS.Core

-------------------
-- STATIC LEVELS --
-------------------

pDEF ∷ ℕ64
pLET,pSEP,pASC,pARR,pOR,pAND,pCMP,pCONS ∷ ℕ64
pBWAND,pBWOR,pBSHFT ∷ ℕ64
pPLUS,pTIMES,pNEG,pPOW,pFAC,pAPP,pREF,pIDX ∷ ℕ64
pTOP ∷ ℕ64

pDEF    = 001  --  x = e

pLET    = 005  --  let fun
pSEP    = 006  --  , ;
pASC    = 007  --  e : τ
pARR    = 010  --  →
pOR     = 020  --  ∨
pAND    = 030  --  ∧
pCMP    = 040  --  ==
pCONS   = 041  --  ∷

pBWOR   = 045  -- ⊻ ⩔
pBWAND  = 046  -- ⩓
pBSHFT  = 047  -- ⋙ ⋘

pPLUS   = 050  --  + - ⩔ ⊻
pTIMES  = 060  --  * / ⩓
pNEG    = 065  --  - (negation)
pPOW    = 070  --  ^ ⋙ ⋘
pFAC    = 090  --  !

pAPP    = 200  --  ␣

pREF    = 250  --  *x &x
pIDX    = 300  --  x.y x@y


pTOP    = 999


