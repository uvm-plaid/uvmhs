module UVMHS.Lib.Parser.Blockify where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Regex (IndentCommand(..))
import UVMHS.Lib.Window

------------------
-- ANCHOR STACK --
------------------

data BlockifyBracket t = BlockifyBracket
  { blockifyBracketSeps ∷ 𝑃 t
  , blockifyBracketsCloses ∷ 𝑃 t
  } deriving (Eq,Ord,Show)

data BlockifyAnchor t = BlockifyAnchor
  { blockifyAnchorCol ∷ AddBT ℕ64
  , blockifyAnchorBrackets ∷ 𝐿 (BlockifyBracket t)
  } deriving (Eq,Ord,Show)
makeLenses ''BlockifyAnchor

blockifyAnchor₀ ∷ BlockifyAnchor t
blockifyAnchor₀ = BlockifyAnchor (AddBT 0) null

--------------------
-- BLOCKIFY MONAD --
--------------------

data BlockifyArgs t = BlockifyArgs
  { blockifyArgsSource ∷ 𝕊
  , blockifyArgsAnchorTL ∷ 𝔹
  , blockifyArgsMkBlockifyToken ∷ IndentCommand → t
  , blockifyArgsNewlineToken ∷ t
  , blockifyArgsIsBlock ∷ t → 𝔹
 -- | true for open, sep and close brackets
  , blockifyArgsIsBracket ∷ t → 𝔹
  -- | map each open bracket to its matching sep/close info
  , blockifyArgsGetCloseBracket ∷ t ⇰ BlockifyBracket t 
  , blockifyArgsInput ∷ 𝑆 (PreParserToken t)
  }

data BlockifyEnv t = BlockifyEnv
  { blockifyEnvSource ∷ 𝕊
  , blockifyEnvAnchorTL ∷ 𝔹
  , blockifyEnvMkBlockifyToken ∷ IndentCommand → t
  , blockifyEnvNewlineToken ∷ t
  , blockifyEnvIsBlock ∷ t → 𝔹
  , blockifyEnvIsBracket ∷ t → 𝔹
  -- | map each open bracket to its matching sep/close info
  , blockifyEnvGetCloseBracket ∷ t ⇰ BlockifyBracket t 
  }
makeLenses ''BlockifyEnv

blockifyEnv₀ ∷ BlockifyArgs t → BlockifyEnv t
blockifyEnv₀ ρ = 
  BlockifyEnv (blockifyArgsSource ρ)
              (blockifyArgsAnchorTL ρ)
              (blockifyArgsMkBlockifyToken ρ)
              (blockifyArgsNewlineToken ρ)
              (blockifyArgsIsBlock ρ)
              (blockifyArgsIsBracket ρ) $
              blockifyArgsGetCloseBracket ρ

type BlockifyOut t = 𝐼C (PreParserToken t)

data BlockifyState t = BlockifyState
  { blockifyStateInput ∷ 𝑆 (PreParserToken t)
  , blockifyStateSkipPrefix ∷ 𝐼C (PreParserToken t)
  , blockifyStatePrefixEnd ∷ AddBT Loc
  , blockifyStateSkipPrefixContainsNewline ∷ 𝔹
  , blockifyStateCurrentAnchor ∷ BlockifyAnchor t
  , blockifyStateParentAnchors ∷ 𝐿 (BlockifyAnchor t)
  , blockifyStateJustSawBlock ∷ 𝔹
  , blockifyStateIsAfterFirstToken ∷ 𝔹
  }
makeLenses ''BlockifyState

blockifyState₀ ∷ BlockifyArgs t → BlockifyState t
blockifyState₀ ρ = 
  BlockifyState 
    (blockifyArgsInput ρ) 
    null BotBT False blockifyAnchor₀ null
    False False

newtype BlockifyM t a = BlockifyM 
  { unBlockifyM ∷ RWST (BlockifyEnv t) (BlockifyOut t) (BlockifyState t) ((∨) Doc) a }
  deriving 
  ( Return,Bind,Functor,Monad
  , MonadError Doc
  , MonadReader (BlockifyEnv t)
  , MonadWriter (BlockifyOut t)
  , MonadState (BlockifyState t)
  )

runBlockifyM ∷ BlockifyEnv t → BlockifyState t → BlockifyM t a → Doc ∨ (BlockifyState t ∧ BlockifyOut t ∧ a)
runBlockifyM γ σ = runRWST γ σ ∘ unBlockifyM

evalBlockifyM ∷ BlockifyEnv t → BlockifyState t → BlockifyM t a → Doc ∨ a
evalBlockifyM γ σ = map snd ∘ runBlockifyM γ σ

oevalBlockifyM ∷ BlockifyEnv t → BlockifyState t → BlockifyM t a → Doc ∨ BlockifyOut t
oevalBlockifyM γ σ = evalBlockifyM γ σ ∘ retOut

oevalBlockifyM₀ ∷ BlockifyArgs t → BlockifyM t a → Doc ∨ BlockifyOut t
oevalBlockifyM₀ ρ = oevalBlockifyM (blockifyEnv₀ ρ) $ blockifyState₀ ρ

-------------
-- HELPERS --
-------------

blockifySyntheticToken ∷ IndentCommand → BlockifyM t (PreParserToken t)
blockifySyntheticToken ic = do
  prefixEnd ← getL blockifyStatePrefixEndL
  let pcS = case ic of
        OpenIC → ppBG white $ ppFG grayLight $ ppString "{"
        CloseIC → ppBG white $ ppFG grayLight $ ppString "}"
        NewlineIC → ppBG white $ ppFG grayLight $ ppString ";"
      eL = eWindowL pcS
      eR = eWindowR pcS
      prefixEndBump = map bumpCol₂ prefixEnd
      pc = ParserContext (LocRange prefixEndBump prefixEndBump) eL eR eR
  mkBlockifyToken ← askL blockifyEnvMkBlockifyTokenL
  return $ PreParserToken (mkBlockifyToken ic) False pc

blockifyPushAnchor ∷ AddBT ℕ64 → BlockifyM t ()
blockifyPushAnchor col = do
  𝑎 ← getputL blockifyStateCurrentAnchorL $ BlockifyAnchor col null
  modifyL blockifyStateParentAnchorsL $ (:&) 𝑎

blockifyPopAnchor ∷ 𝑂 (PreParserToken t) → BlockifyM t ()
blockifyPopAnchor tO = do
  pc ← case tO of
    None → do
      pEnd ← getL blockifyStatePrefixEndL
      return $ eofContext pEnd
    Some t → return $ preParserTokenContext t
  𝑎s ← getL blockifyStateParentAnchorsL
  so ← askL blockifyEnvSourceL
  -- let er = displaySourceError so $ AddNull $ ParserError
  case 𝑎s of
    Nil → throw $ ppVertical
      [ ppErr "BLOCKIFY INTERNAL ERROR"
      , ppString "could not pop anchor stack"
      ]
    𝑎 :& 𝑎s' → do
      putL blockifyStateCurrentAnchorL 𝑎
      putL blockifyStateParentAnchorsL 𝑎s'

blockifyPushAnchorBracket ∷ BlockifyBracket t → BlockifyM t ()
blockifyPushAnchorBracket bt =
  modifyL (blockifyAnchorBracketsL ⊚ blockifyStateCurrentAnchorL) $ (:&) bt

blockifyPopAnchorBracket ∷ BlockifyM t ()
blockifyPopAnchorBracket = do
  BlockifyAnchor col bs ← getL blockifyStateCurrentAnchorL
  case bs of
    Nil → throw $ ppVertical
      [ ppErr "BLOCKIFY INTERNAL ERROR"
      , ppString "could not pop anchor bracket stack"
      ]
    _b :& bs' → do
      putL blockifyStateCurrentAnchorL $ BlockifyAnchor col bs'

blockifyFlushSkipPrefix ∷ BlockifyM t ()
blockifyFlushSkipPrefix = do
  sp ← getputL blockifyStateSkipPrefixL null 
  let spEnd = joins $ mapOn sp $ \ t → locRangeEnd $ parserContextLocRange $ preParserTokenContext t
  modifyL blockifyStatePrefixEndL $ (⊔) spEnd
  putL blockifyStateSkipPrefixContainsNewlineL False
  tell sp

blockifyEmitToken ∷ (Pretty t,Ord t) ⇒ PreParserToken t → BlockifyM t ()
blockifyEmitToken t = do
  -----------------------
  -- FIRST TOKEN LOGIC --
  -----------------------
  putL blockifyStateIsAfterFirstTokenL True
  -----------------
  -- BLOCK LOGIC --
  -----------------
  isBlock ← askL blockifyEnvIsBlockL
  if isBlock $ preParserTokenValue t
  then putL blockifyStateJustSawBlockL True
  else putL blockifyStateJustSawBlockL False
  -------------------
  -- BRACKET LOGIC --
  -------------------
  isBracket ← askL blockifyEnvIsBracketL
  getCloseBracket ← askL blockifyEnvGetCloseBracketL
  let tVal = preParserTokenValue t
  when (isBracket tVal) $ \ () → do
    case getCloseBracket ⋕? tVal of
      Some bt → do
        --------------------------
        -- IT IS A BRACKET OPEN --
        --------------------------
        --
        --              ⇒        ▽
        --     token  ( ⇒ token  (
        --          ⇧ ↑ ⇒      ⇧ ↑
        --
        -- - push its sep/close info on the stack
        blockifyPushAnchorBracket bt
      None → repeat $ \ again → do
        𝑎ᵢ ← getL blockifyStateCurrentAnchorL
        ----------------------------------
        -- IT IS A BRACKET SEP OR CLOSE --
        ----------------------------------
        case blockifyAnchorBrackets 𝑎ᵢ of
          bt :& bts → do
            ----------------------------------------------
            -- IT IS A BRACKET TOKEN FOR CURRENT ANCHOR --
            ----------------------------------------------
            -- - we are currently inside a bracket
            -- - try to match token with sep or close
            -- - if sep, do nothing
            -- - if close, close this bracket (pop the bracket stack)
            -- - fail if no match
            let BlockifyBracket seps closes = bt
            if tVal ∈ seps then do
              -------------------------
              -- IT IS A BRACKET SEP --
              -------------------------
              --
              --     ▽         ⇒ ▽
              --     (token  , ⇒ (token  ,
              --           ⇧ ↑ ⇒       ⇧ ↑
              --
              -- - nothing to do
              skip
            else if tVal ∈ closes then do
              ---------------------------
              -- IT IS A BRACKET CLOSE --
              ---------------------------
              --
              --           ▽         ⇒ ▽
              --     (token(token  ) ⇒ (token(token  )
              --                 ⇧ ↑ ⇒             ⇧ ↑
              --
              -- - pop the bracket stack
              putL (blockifyAnchorBracketsL ⊚ blockifyStateCurrentAnchorL) bts
            else do
              -------------------------------
              -- IT IS A BAD BRACKET TOKEN --
              -------------------------------
              --
              --     ▽        
              --     (token  ]
              --           ⇧ ↑
              --
              -- OR
              --
              --     ▽        
              --     (token  ;
              --           ⇧ ↑
              --
              -- - fail
              throw $ ppVertical
                    [ ppErr "BLOCKIFY ERROR"
                    , ppString "improper nesting/use of bracket close/sep"
                    ]
          Nil → do
            ---------------------------------------------
            -- IT IS A BRACKET TOKEN FOR PARENT ANCHOR --
            ---------------------------------------------
            --
            --           ⌄                 ⌄    ▽
            --     block(block{ token  ) ⇒ block(block{ token}  )
            --                       ⇧ ↑                      ⇧ ↑
            --
            -- - we are currently inside a block anchor with no bracket stack
            -- - close out the block
            -- - pop the anchor
            -- - repeat
            blockifyEmitSyntheticToken CloseIC
            blockifyPopAnchor $ Some t
            again ()
  --------------------
  -- EMIT THE TOKEN --
  --------------------
  blockifyFlushSkipPrefix
  tell $ single t

blockifyEmitSyntheticToken ∷ IndentCommand → BlockifyM t ()
blockifyEmitSyntheticToken ic = do
  tell *$ single ^$ blockifySyntheticToken ic

blockifyEmitSkipToken ∷ (Eq t) ⇒ PreParserToken t → BlockifyM t ()
blockifyEmitSkipToken t = do
  newlineToken ← askL blockifyEnvNewlineTokenL
  modifyL blockifyStateSkipPrefixL $ pospend $ single t
  modifyL blockifyStatePrefixEndL $ (⊔) $ locRangeEnd $ parserContextLocRange $ preParserTokenContext t
  modifyL blockifyStateSkipPrefixContainsNewlineL $ (⩔) $ preParserTokenValue t ≡ newlineToken

blockifyAnchorOnToken ∷ PreParserToken t → BlockifyM t ()
blockifyAnchorOnToken t = do
  let tCol ∷ AddBT ℕ64
      tCol = map locCol $ locRangeBegin $ parserContextLocRange $ preParserTokenContext t
  blockifyPushAnchor tCol

blockifyPopInput ∷ BlockifyM t (𝑂 (PreParserToken t))
blockifyPopInput = do
  ts ← getL blockifyStateInputL
  case un𝑆 ts () of
    None → return None
    Some (t :* ts') → do
      putL blockifyStateInputL ts'
      return $ Some t

----------------------------
-- MAIN MONADIC PROCEDURE --
----------------------------

blockifyM ∷ ∀ t. (Ord t,Pretty t) ⇒ BlockifyM t ()
blockifyM = do
  tO ← blockifyPopInput
  anchorTL ← askL blockifyEnvAnchorTLL
  case tO of
    None → do
      -- ============================== --
      -- we are out of input to process --
      -- ============================== --
      ------------------------------------------------------------------------
      -- IF WE JUST CREATED A BLOCK AND IT IS NOT YET ANCHORED CLOSE IT OUT --
      ------------------------------------------------------------------------
      --
      --     block <EOF> ⇒ block{} <EOF>
      --          ⇧↑     ⇒        ⇧↑
      --
      justSawBlock ← getL blockifyStateJustSawBlockL
      when justSawBlock $ \ () → do
        -- - we just saw a block token and haven't created an anchor for it yet.
        -- - open and close it out
        blockifyEmitSyntheticToken OpenIC 
        blockifyEmitSyntheticToken CloseIC 
        putL blockifyStateJustSawBlockL False
      ---------------------------------------------------------------
      -- WHILE THERE ARE STILL ANCHORS ON THE STACK CLOSE THEM OUT --
      ---------------------------------------------------------------
      --
      --     block{          ⇒ block{
      --       block{        ⇒   block{
      --         token <EOF> ⇒     token}} <EOF>
      --              ⇧↑     ⇒            ⇧↑
      --
      repeat $ \ again → do
        𝑎ᵢ ← getL blockifyStateCurrentAnchorL
        when (𝑎ᵢ ≢ blockifyAnchor₀) $ \ () → do
          -- - the current anchor is not the initial anchor
          -- - fail if there are outstanding open brackets
          when (not $ isEmpty $ blockifyAnchorBrackets 𝑎ᵢ) $ \ () →
            throw $ ppVertical
              [ ppErr "BLOCKIFY ERROR"
              , ppString "input ended before a closing bracket"
              ]
          -- - otherwise let's "close out" this anchor with a close token and continue
          blockifyEmitSyntheticToken CloseIC
          -- - safe to assume parent anchors are non-empty 
          --   (otherwise a ≡ a₀ would succeed)
          blockifyPopAnchor None
          again ()
        -- - the current anchor is the initial anchor
        -- - nothing left to do
        skip
      ---------------------------
      -- FLUSH THE SKIP PREFIX --
      ---------------------------
      --
      --     block{} <EOF> ⇒ block{} <EOF>
      --            ⇧↑     ⇒         ⇈
      --
      blockifyFlushSkipPrefix
    Some t → do
      -- =============================== --
      -- we have a next token to process --
      -- =============================== --
      if preParserTokenSkip t then do
        ------------------------
        -- IT IS A SKIP TOKEN --
        ------------------------
        --
        --     token  ␣token ⇒ token   token
        --          ⇧ ↑      ⇒      ⇧  ↑
        --
        blockifyEmitSkipToken t
      else do
        repeat $ \ again → do
          justSawBlock ← getL blockifyStateJustSawBlockL
          𝑎 ← getL blockifyStateCurrentAnchorL
          let 𝑎Col = blockifyAnchorCol 𝑎
              tCol = map locCol $ locRangeBegin $ parserContextLocRange $ preParserTokenContext t
          if tCol > 𝑎Col then do
            ---------------------
            -- RIGHT OF ANCHOR --
            ---------------------
            --
            --      ⌄
            --      token
            --           ⇧
            --          token 
            --          ↑
            --
            when justSawBlock $ \ () → do
              -----------------------------------
              -- PRIOR TOKEN WAS A BLOCK TOKEN --
              -----------------------------------
              --
              --      ⌄               ⌄ 
              --      block     ⇒ block{
              --           ⇧    ⇒       ⇧
              --          token ⇒     token
              --          ↑     ⇒     ↑
              --
              blockifyEmitSyntheticToken OpenIC
              blockifyAnchorOnToken t
              putL blockifyStateJustSawBlockL False
          else if tCol ≡ 𝑎Col then do
            -------------------------
            -- IN LINE WITH ANCHOR --
            -------------------------
            --
            --      ⌄        
            --      token    
            --           ⇧   
            --      token
            --      ↑    
            --
            when justSawBlock $ \ () → do
              -----------------------------------
              -- PRIOR TOKEN WAS A BLOCK TOKEN --
              -----------------------------------
              --
              --      ⌄        ⌄
              --      block  ⇒ block{}
              --           ⇧ ⇒        ⇧
              --      token  ⇒ token
              --      ↑      ⇒ ↑
              --
              blockifyEmitSyntheticToken OpenIC
              blockifyEmitSyntheticToken CloseIC
              putL blockifyStateJustSawBlockL False
            --
            --      ⌄        ⌄
            --      token  ⇒ token;
            --           ⇧ ⇒       ⇧
            --      token  ⇒ token
            --      ↑      ⇒ ↑
            --
            isAfterFirstToken ← getL blockifyStateIsAfterFirstTokenL
            when (isAfterFirstToken ⩓ (not anchorTL ⇛ 𝑎 ≢ blockifyAnchor₀)) $ \ () → do
              -- CORNER CASES:
              -- - isAfterFirstToken: 
              --   When in anchored mode, we start with an anchor at column
              --   zero. This has the effect of the algorithm thinking the
              --   first token is a "fresh newline", which would emit a
              --   newline synthetic token, and not just "the first line",
              --   which shouldn't have a newline synthetic token before it.
              -- - anchorTL:
              --   when in unanchored mode (anchorTL ≡ False), do not emit
              --   newlines when the current anchor is the root/initial
              --   anchor.
              blockifyEmitSyntheticToken NewlineIC
          else {- if tCol < 𝑎Col then -} do
            ---------------------------------------
            -- IT IS ON NEXT LINE LEFT OF ANCHOR --
            ---------------------------------------
            --
            --        ⌄        
            --        token    
            --             ⇧   
            --      token
            --      ↑    
            --
            when justSawBlock $ \ () → do
              -----------------------------------
              -- PRIOR TOKEN WAS A BLOCK TOKEN --
              -----------------------------------
              --
              --          ⌄        
              --        { block  ⇒   { block{}
              --               ⇧ ⇒            ⇧
              --      token      ⇒ token
              --      ↑          ⇒ ↑
              --
              blockifyEmitSyntheticToken OpenIC
              blockifyEmitSyntheticToken CloseIC
              putL blockifyStateJustSawBlockL False
            --
            --          ⌄        
            --        { token  ⇒   { token}
            --               ⇧ ⇒           ⇧
            --      token      ⇒ token
            --      ↑          ⇒ ↑
            --
            blockifyEmitSyntheticToken CloseIC
            blockifyPopAnchor $ Some t
            again ()
        --
        --     token   token ⇒ token   token
        --          ⇧  ↑     ⇒              ⇈
        --
        blockifyEmitToken t
      blockifyM

-------------------------
-- TOP LEVEL OPERATION --
-------------------------

blockify ∷ (Ord t,Pretty t) ⇒ BlockifyArgs t → Doc ∨ BlockifyOut t
blockify ρ = oevalBlockifyM₀ ρ blockifyM
