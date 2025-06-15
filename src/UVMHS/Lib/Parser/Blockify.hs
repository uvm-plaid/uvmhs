module UVMHS.Lib.Parser.Blockify where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.Regex (IndentCommand(..))
import UVMHS.Lib.Window

------------------
-- ANCHOR STACK --
------------------

data BlockifyBracket t = BlockifyBracket
  { blockifyBracketSeps ∷ 𝑃 t
  , blockifyBracketCloses ∷ 𝑃 t
  , blockifyBracketSepsAndCloses ∷ 𝑃 t
  } deriving (Eq,Ord,Show)

blockifyBracketDepthOne ∷ (Ord t) ⇒ BlockifyBracket t → t ⇰ ℕ64
blockifyBracketDepthOne bb =
  dict $ mapOn (iter $ blockifyBracketSepsAndCloses bb) $ \ t → t ↦ 1

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

data BlockifyBracketArg t = BlockifyBracketArg
  { blockifyBracketArgSeps ∷ 𝑃 t
  , blockifyBracketArgCloses ∷ 𝑃 t
  } deriving (Eq,Ord,Show)

blockifyBracketArgToBlockifyBracket ∷ (Ord t) ⇒ BlockifyBracketArg t → BlockifyBracket t
blockifyBracketArgToBlockifyBracket (BlockifyBracketArg seps closes) =
  BlockifyBracket seps closes $ seps ∪ closes

data BlockifyArgs t = BlockifyArgs
  { blockifyArgsSource ∷ 𝕊
  , blockifyArgsAnchorTL ∷ 𝔹
  , blockifyArgsMkBlockifyToken ∷ IndentCommand → t
  , blockifyArgsNewlineToken ∷ t
  , blockifyArgsIsBlock ∷ t → 𝔹
 -- | true for open, sep and close brackets
  , blockifyArgsBracketOpens ∷ 𝑃 t
  , blockifyArgsBracketSeps ∷ 𝑃 t
  , blockifyArgsBracketCloses ∷ 𝑃 t
  -- | map each open bracket to its matching sep/close info
  , blockifyArgsGetCloseBracket ∷ t ⇰ BlockifyBracketArg t 
  , blockifyArgsInput ∷ 𝑆 (ParserToken t)
  }

data BlockifyEnv t = BlockifyEnv
  { blockifyEnvSource ∷ 𝕊
  , blockifyEnvAnchorTL ∷ 𝔹
  , blockifyEnvMkBlockifyToken ∷ IndentCommand → t
  , blockifyEnvNewlineToken ∷ t
  , blockifyEnvIsBlock ∷ t → 𝔹
  , blockifyEnvBracketOpens ∷ 𝑃 t
  , blockifyEnvBracketSeps ∷ 𝑃 t
  , blockifyEnvBracketCloses ∷ 𝑃 t
  , blockifyEnvIsBracket ∷ t → 𝔹
  -- | map each open bracket to its matching sep/close info
  , blockifyEnvGetCloseBracket ∷ t ⇰ BlockifyBracket t 
  }
makeLenses ''BlockifyEnv

blockifyEnv₀ ∷ (Ord t) ⇒ BlockifyArgs t → BlockifyEnv t
blockifyEnv₀ ρ = 
  let bracketOpens = blockifyArgsBracketOpens ρ
      bracketSeps = blockifyArgsBracketSeps ρ
      bracketCloses = blockifyArgsBracketCloses ρ
  in
  BlockifyEnv (blockifyArgsSource ρ)
              (blockifyArgsAnchorTL ρ)
              (blockifyArgsMkBlockifyToken ρ)
              (blockifyArgsNewlineToken ρ)
              (blockifyArgsIsBlock ρ)
              bracketOpens bracketSeps bracketCloses 
              (flip (∈♭) $ unions [bracketOpens,bracketSeps,bracketCloses]) $
              map blockifyBracketArgToBlockifyBracket $ blockifyArgsGetCloseBracket ρ

type BlockifyOut t = 𝐼C (PreParserToken t)

data BlockifyState t = BlockifyState
  { blockifyStateInput ∷ 𝑆 (ParserToken t)
  , blockifyStateSkipPrefix ∷ 𝐼C (ParserToken t)
  , blockifyStatePrefix ∷ WindowR Doc Doc
  , blockifyStatePrefixEnd ∷ AddBT Loc
  , blockifyStateSkipPrefixContainsNewline ∷ 𝔹
  , blockifyStateCurrentAnchor ∷ BlockifyAnchor t
  , blockifyStateParentAnchors ∷ 𝐿 (BlockifyAnchor t)
  , blockifyStateJustSawBlock ∷ 𝔹
  , blockifyStateIsAfterFirstToken ∷ 𝔹
  , blockifyStateBracketTokenDepth ∷ t ⇰ ℕ64
  }
makeLenses ''BlockifyState

blockifyState₀ ∷ BlockifyArgs t → BlockifyState t
blockifyState₀ ρ = 
  BlockifyState 
    (blockifyArgsInput ρ) 
    null null BotBT False blockifyAnchor₀ null
    False False null

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

oevalBlockifyM₀ ∷ (Ord t) ⇒ BlockifyArgs t → BlockifyM t a → Doc ∨ BlockifyOut t
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

blockifyErr ∷ 𝑂 (ParserToken t) → 𝕊 → BlockifyM t ()
blockifyErr tO msg = do
  pc :* ps ← case tO of
    None → do
      pEnd ← getL blockifyStatePrefixEndL
      return $ eofContext pEnd :* null
    Some t → return $ parserTokenContext t :* parserTokenSuffix t
  so ← askL blockifyEnvSourceL
  pr ← getL blockifyStatePrefixL
  spr ← getL blockifyStateSkipPrefixL
  let pr' = concat [pr,concat $ map (parserContextDisplayR ∘ parserTokenContext) spr]
      pei = ParserErrorInfo pr' null msg null
      pe = ParserError (locRangeEnd $ parserContextLocRange pc) (parserContextError pc) ps $ single pei
  throw $ displaySourceError so $ AddNull pe

blockifyPopAnchor ∷ 𝑂 (ParserToken t) → 𝕊 → BlockifyM t ()
blockifyPopAnchor tO msg = do
  𝑎 ← getL blockifyStateCurrentAnchorL
  when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () → 
    blockifyErr tO msg
  𝑎s ← getL blockifyStateParentAnchorsL
  case 𝑎s of
    Nil → blockifyErr tO msg
    𝑎' :& 𝑎s' → do
      putL blockifyStateCurrentAnchorL 𝑎'
      putL blockifyStateParentAnchorsL 𝑎s'

blockifyPushAnchorBracket ∷ (Ord t) ⇒ BlockifyBracket t → BlockifyM t ()
blockifyPushAnchorBracket bb = do
  modifyL (blockifyAnchorBracketsL ⊚ blockifyStateCurrentAnchorL) $ (:&) bb
  modifyL blockifyStateBracketTokenDepthL $ (+) $ blockifyBracketDepthOne bb

blockifyRecordPrefix ∷ 𝐼C (PreParserToken t) → BlockifyM t ()
blockifyRecordPrefix ts =
  modifyL blockifyStatePrefixL $ pospend $ concat $ map (parserContextDisplayR ∘ preParserTokenContext) ts

blockifyEmit ∷ 𝐼C (PreParserToken t) → BlockifyM t ()
blockifyEmit ts = do
  blockifyRecordPrefix ts
  tell ts

blockifyFlushSkipPrefix ∷ BlockifyM t ()
blockifyFlushSkipPrefix = do
  sp ← getputL blockifyStateSkipPrefixL null 
  let spEnd = joins $ mapOn sp $ \ t → locRangeEnd $ parserContextLocRange $ parserTokenContext t
  modifyL blockifyStatePrefixEndL $ (⊔) spEnd
  putL blockifyStateSkipPrefixContainsNewlineL False
  blockifyEmit $ map parserTokenToPreParserToken sp

blockifyEmitToken ∷ (Pretty t,Ord t) ⇒ ParserToken t → BlockifyM t ()
blockifyEmitToken t = do
  -----------------------
  -- FIRST TOKEN LOGIC --
  -----------------------
  putL blockifyStateIsAfterFirstTokenL True
  -----------------
  -- BLOCK LOGIC --
  -----------------
  isBlock ← askL blockifyEnvIsBlockL
  if isBlock $ parserTokenValue t
  then putL blockifyStateJustSawBlockL True
  else putL blockifyStateJustSawBlockL False
  -------------------
  -- BRACKET LOGIC --
  -------------------
  isBracket ← askL blockifyEnvIsBracketL
  getCloseBracket ← askL blockifyEnvGetCloseBracketL
  let tVal = parserTokenValue t
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
            if tVal ∈ blockifyBracketSeps bt then do
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
            else if tVal ∈ blockifyBracketCloses bt then do
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
              bracketSeps ← askL blockifyEnvBracketSepsL
              blockifyErr (Some t) $ concat $ inbetween " " 
                [ "bracket CLOSE before this bracket"
                , if tVal ∈ bracketSeps then "SEP" else "CLOSE"
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
            tokenDepth ← getL blockifyStateBracketTokenDepthL
            bracketSeps ← askL blockifyEnvBracketSepsL
            when (tokenDepth ⋕? tVal ∈♭ pow [None,Some 0]) $ \ () →
              blockifyErr (Some t) $ concat $ inbetween " "
                [ "bracket OPEN before this bracket"
                , if tVal ∈ bracketSeps then "SEP" else "CLOSE"
                ]
            blockifyEmitSyntheticToken CloseIC
            blockifyPopAnchor (Some t) $ concat $ inbetween " " 
              [ "bracket OPEN before this bracket"
              , if tVal ∈ bracketSeps then "SEP" else "CLOSE"
              ]
            again ()
  --------------------
  -- EMIT THE TOKEN --
  --------------------
  blockifyFlushSkipPrefix
  blockifyEmit $ single $ parserTokenToPreParserToken t

blockifyEmitSyntheticToken ∷ IndentCommand → BlockifyM t ()
blockifyEmitSyntheticToken ic = do
  blockifyEmit *$ single ^$ blockifySyntheticToken ic

blockifyEmitSkipToken ∷ (Eq t) ⇒ ParserToken t → BlockifyM t ()
blockifyEmitSkipToken t = do
  newlineToken ← askL blockifyEnvNewlineTokenL
  modifyL blockifyStateSkipPrefixL $ pospend $ single t
  modifyL blockifyStatePrefixEndL $ (⊔) $ locRangeEnd $ parserContextLocRange $ parserTokenContext t
  modifyL blockifyStateSkipPrefixContainsNewlineL $ (⩔) $ parserTokenValue t ≡ newlineToken

blockifyAnchorOnToken ∷ ParserToken t → BlockifyM t ()
blockifyAnchorOnToken t = do
  let tCol ∷ AddBT ℕ64
      tCol = map locCol $ locRangeBegin $ parserContextLocRange $ parserTokenContext t
  blockifyPushAnchor tCol

blockifyPopInput ∷ BlockifyM t (𝑂 (ParserToken t))
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
            blockifyErr None "bracket CLOSE before END OF INPUT"
          -- - otherwise let's "close out" this anchor with a close token and continue
          blockifyEmitSyntheticToken CloseIC
          -- - safe to assume parent anchors are non-empty 
          --   (otherwise a ≡ a₀ would succeed)
          blockifyPopAnchor None "[INTERNAL ERROR]"
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
      if parserTokenSkip t then do
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
              tCol = map locCol $ locRangeBegin $ parserContextLocRange $ parserTokenContext t
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
              when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () →
                blockifyErr (Some t) $ concat $ inbetween " " 
                  [ "bracket CLOSE before block NEWLINE"
                  ]
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
            when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () →
              blockifyErr (Some t) $ concat $ inbetween " " 
                [ "bracket CLOSE before block CLOSE"
                ]
            blockifyPopAnchor (Some t) "[INTERNAL ERROR]"
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
