module UVMHS.Lib.Parser.Blockify where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Window

--------------------
-- BLOCKIFY TOKEN --
--------------------

data BlockifyToken = Open_BT | Close_BT | Sep_BT
  deriving (Eq,Ord,Show)

------------------
-- ANCHOR STACK --
------------------

data OpenBracketInfo t = OpenBracketInfo
  { openBracketInfoSeps ∷ 𝑃 t
  , openBracketInfoCloses ∷ 𝑃 t
  , openBracketInfoSepsAndCloses ∷ 𝑃 t
  } deriving (Eq,Ord,Show)

openBracketInfoDepthOne ∷ (Ord t) ⇒ OpenBracketInfo t → t ⇰ ℤ64
openBracketInfoDepthOne obi =
  dict $ mapOn (iter $ openBracketInfoSepsAndCloses obi) $ \ t → t ↦ 1

data BlockifyAnchor t = BlockifyAnchor
  { blockifyAnchorCol ∷ AddBT ℕ64
  , blockifyAnchorBrackets ∷ 𝐿 (OpenBracketInfo t)
  } deriving (Eq,Ord,Show)
makeLenses ''BlockifyAnchor

blockifyAnchor₀ ∷ BlockifyAnchor t
blockifyAnchor₀ = BlockifyAnchor (AddBT 0) null

--------------------
-- BLOCKIFY MONAD --
--------------------

data BlockifyArgs t = BlockifyArgs
  { blockifyArgsAnchorTL ∷ 𝔹
  , blockifyArgsMkBlockifyToken ∷ BlockifyToken → t
  , blockifyArgsNewlineToken ∷ t
  , blockifyArgsIsBlock ∷ t → 𝔹
  , blockifyArgsBrackets ∷ t ⇰ 𝕊 ∧ (t ⇰ 𝕊) ∧ (t ⇰ 𝕊)
  }

data BlockifyEnv t = BlockifyEnv
  { blockifyEnvSource ∷ 𝕊
  , blockifyEnvAnchorTL ∷ 𝔹
  , blockifyEnvMkBlockifyToken ∷ BlockifyToken → t
  , blockifyEnvNewlineToken ∷ t
  , blockifyEnvIsBlock ∷ t → 𝔹
  , blockifyEnvIsBracket ∷ t → 𝔹
  , blockifyEnvGetOpenBracketInfo ∷ t ⇰ OpenBracketInfo t 
  , blockifyEnvCloseBrackets ∷ 𝑃 t
  , blockifyEnvGetOpenBracket ∷ t ⇰ 𝑃 t
  , blockifyEnvGetDisplayToken ∷ t ⇰ 𝕊
  }
makeLenses ''BlockifyEnv

blockifyEnv₀ ∷ ∀ t. (Ord t) ⇒ BlockifyArgs t → 𝕊 → BlockifyEnv t
blockifyEnv₀ ρ so = 
  let isBracket = flip (∈♭) $ unions $ mapOn (iter $ blockifyArgsBrackets ρ) $ uncurry $ \ open (_tS :* seps :* closes) →
        unions [single open,dkeys seps,dkeys closes]
      getOpenBracketInfo = dict $ mapOn (iter $ blockifyArgsBrackets ρ) $ uncurry $ \ open (_tS :* seps :* closes) →
        let seps' = dkeys seps
            closes' = dkeys closes
            obi = OpenBracketInfo seps' closes' $ seps' ∪ closes'
        in open ↦ obi
      closeBrackets = unions $ map openBracketInfoCloses $ dvals getOpenBracketInfo
      getOpenBracket = joins $ do
        open :* obi ← iter getOpenBracketInfo
        tok ← iter $ openBracketInfoSepsAndCloses obi
        return $ tok ↦ single open
      getDisplayToken = dict $ mapOn (iter $ blockifyArgsBrackets ρ) $ uncurry $ \ open (ts :* seps :* closes) →
        dict
          [ open ↦ ts
          , seps
          , closes
          ]
  in
  BlockifyEnv 
    { blockifyEnvSource = so
    , blockifyEnvAnchorTL = blockifyArgsAnchorTL ρ
    , blockifyEnvMkBlockifyToken = blockifyArgsMkBlockifyToken ρ
    , blockifyEnvNewlineToken = blockifyArgsNewlineToken ρ
    , blockifyEnvIsBlock = blockifyArgsIsBlock ρ
    , blockifyEnvIsBracket = isBracket 
    , blockifyEnvGetOpenBracketInfo = getOpenBracketInfo 
    , blockifyEnvCloseBrackets = closeBrackets 
    , blockifyEnvGetOpenBracket = getOpenBracket
    , blockifyEnvGetDisplayToken = getDisplayToken
    }

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
  , blockifyStateBracketTokenDepth ∷ t ⇰ ℤ64
  }
makeLenses ''BlockifyState

blockifyState₀ ∷ 𝕍 (ParserToken t) → BlockifyState t
blockifyState₀ ts = BlockifyState (stream ts) null null BotBT False blockifyAnchor₀ null False False null

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

oevalBlockifyM₀ ∷ (Ord t) ⇒ BlockifyArgs t → 𝕊 → 𝕍 (ParserToken t) → BlockifyM t a → Doc ∨ BlockifyOut t
oevalBlockifyM₀ ρ so ts = oevalBlockifyM (blockifyEnv₀ ρ so) $ blockifyState₀ ts

-------------
-- HELPERS --
-------------

blockifySyntheticToken ∷ BlockifyToken → BlockifyM t (PreParserToken t)
blockifySyntheticToken ic = do
  prefixEnd ← getL blockifyStatePrefixEndL
  let pcS = case ic of
        Open_BT → ppBG white $ ppFG grayLight $ ppString "{"
        Close_BT → ppBG white $ ppFG grayLight $ ppString "}"
        Sep_BT → ppBG white $ ppFG grayLight $ ppString ";"
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

blockifyError ∷ 𝑂 (ParserToken t) → 𝕊 → BlockifyM t ()
blockifyError tO msg = do
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

blockifyPopAnchor ∷ 𝑂 (ParserToken t) → BlockifyM t ()
blockifyPopAnchor tO = do
  𝑎 ← getL blockifyStateCurrentAnchorL
  when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () → 
    blockifyError tO "[INTERNAL ERROR]"
  𝑎s ← getL blockifyStateParentAnchorsL
  case 𝑎s of
    Nil → blockifyError tO "[INTERNAL ERROR]"
    𝑎' :& 𝑎s' → do
      putL blockifyStateCurrentAnchorL 𝑎'
      putL blockifyStateParentAnchorsL 𝑎s'

blockifyPushAnchorBracket ∷ (Ord t) ⇒ OpenBracketInfo t → BlockifyM t ()
blockifyPushAnchorBracket obi = do
  modifyL (blockifyAnchorBracketsL ⊚ blockifyStateCurrentAnchorL) $ (:&) obi
  modifyL blockifyStateBracketTokenDepthL $ (+) $ openBracketInfoDepthOne obi

blockifyPopAnchorBracket ∷ (Ord t) ⇒ OpenBracketInfo t → 𝐿 (OpenBracketInfo t) → BlockifyM t ()
blockifyPopAnchorBracket obi obis = do
  putL (blockifyAnchorBracketsL ⊚ blockifyStateCurrentAnchorL) obis
  modifyL blockifyStateBracketTokenDepthL $ (+) $ neg $ openBracketInfoDepthOne obi


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
  getOpenBracketInfo ← askL blockifyEnvGetOpenBracketInfoL
  getOpenBracket ← askL blockifyEnvGetOpenBracketL
  getDisplayToken ← askL blockifyEnvGetDisplayTokenL
  let tVal = parserTokenValue t
  tokenDepth₀ ← getL blockifyStateBracketTokenDepthL
  when (isBracket tVal) $ \ () → do
    case getOpenBracketInfo ⋕? tVal of
      Some bt | tokenDepth₀ ⋕? tVal ∈♭ pow [None,Some 0] → do
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
      _ → repeat $ \ again → do
        𝑎 ← getL blockifyStateCurrentAnchorL
        ----------------------------------
        -- IT IS A BRACKET SEP OR CLOSE --
        ----------------------------------
        case blockifyAnchorBrackets 𝑎 of
          bt :& bts → do
            ----------------------------------------------
            -- IT IS A BRACKET TOKEN FOR CURRENT ANCHOR --
            ----------------------------------------------
            -- - we are currently inside a bracket
            -- - try to match token with sep or close
            -- - if sep, do nothing
            -- - if close, close this bracket (pop the bracket stack)
            -- - fail if no match
            if tVal ∈ openBracketInfoSeps bt then do
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
            else if tVal ∈ openBracketInfoCloses bt then do
              ---------------------------
              -- IT IS A BRACKET CLOSE --
              ---------------------------
              --
              --           ▽         ⇒ ▽
              --     (token(token  ) ⇒ (token(token  )
              --                 ⇧ ↑ ⇒             ⇧ ↑
              --
              -- - pop the bracket stack
              blockifyPopAnchorBracket bt bts
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
              closeBrackets ← askL blockifyEnvCloseBracketsL
              let expectedCloses = openBracketInfoCloses bt
              blockifyError (Some t) $ concat $ inbetween " " 
                [ "matching bracket CLOSE"
                , concat $ inbetween " OR " $ mapOn (iter expectedCloses) $ \ tᵢ → concat ["‹",getDisplayToken ⋕! tᵢ,"›" ]
                , "before this bracket"
                , if tVal ∈ closeBrackets then "CLOSE" else "SEP"
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
            -- TODO: change `tokenDepth` to just be the nearest
            --       AnchorBracketInfo object up the anchor stack
            tokenDepth ← getL blockifyStateBracketTokenDepthL
            closeBrackets ← askL blockifyEnvCloseBracketsL
            when (tokenDepth ⋕? tVal ∈♭ pow [None,Some 0]) $ \ () →
              blockifyError (Some t) $ concat $ inbetween " "
                [ "matching bracket OPEN"
                , concat $ inbetween " OR " $ do
                    tᵢ ← iter $ getOpenBracket ⋕! tVal
                    return $ concat ["‹",getDisplayToken ⋕! tᵢ,"›"]
                , "before this bracket"
                , if tVal ∈ closeBrackets then "CLOSE" else "SEP"
                ]
            blockifyEmitSyntheticToken Close_BT
            blockifyPopAnchor $ Some t
            again ()
  --------------------
  -- EMIT THE TOKEN --
  --------------------
  blockifyFlushSkipPrefix
  blockifyEmit $ single $ parserTokenToPreParserToken t

blockifyEmitSyntheticToken ∷ BlockifyToken → BlockifyM t ()
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
  getDisplayToken ← askL blockifyEnvGetDisplayTokenL
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
        blockifyEmitSyntheticToken Open_BT 
        blockifyEmitSyntheticToken Close_BT 
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
        𝑎 ← getL blockifyStateCurrentAnchorL
        when (𝑎 ≢ blockifyAnchor₀) $ \ () → do
          -- - the current anchor is not the initial anchor
          -- - fail if there are outstanding open brackets
          when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () → do
            let expectedCloses = openBracketInfoCloses $ viewΩ someL $ firstElem $ blockifyAnchorBrackets 𝑎
            blockifyError None $ concat $ inbetween " "
              [ "bracket CLOSE"
              , concat $ inbetween " OR " $ mapOn (iter expectedCloses) $ \ tᵢ → concat ["‹",getDisplayToken ⋕! tᵢ,"›" ]
              , "before END OF INPUT"
              ]

          -- - otherwise let's "close out" this anchor with a close token and continue
          blockifyEmitSyntheticToken Close_BT
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
              blockifyEmitSyntheticToken Open_BT
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
              blockifyEmitSyntheticToken Open_BT
              blockifyEmitSyntheticToken Close_BT
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
              --   SEP synthetic token, and not just "the first line",
              --   which shouldn't have a SEP synthetic token before it.
              -- - anchorTL:
              --   when in unanchored mode (anchorTL ≡ False), do not emit
              --   SEP synthetic tokens when the current anchor is the
              --   root/initial anchor.
              blockifyEmitSyntheticToken Sep_BT
              when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () → do
                let expectedCloses = openBracketInfoCloses $ viewΩ someL $ firstElem $ blockifyAnchorBrackets 𝑎
                blockifyError (Some t) $ concat $ inbetween " " 
                  [ "bracket CLOSE"
                  , concat $ inbetween " OR " $ mapOn (iter expectedCloses) $ \ tᵢ → concat ["‹",getDisplayToken ⋕! tᵢ,"›" ]
                  , "before block SEP triggered by this TOKEN"
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
              blockifyEmitSyntheticToken Open_BT
              blockifyEmitSyntheticToken Close_BT
              putL blockifyStateJustSawBlockL False
            --
            --          ⌄        
            --        { token  ⇒   { token}
            --               ⇧ ⇒           ⇧
            --      token      ⇒ token
            --      ↑          ⇒ ↑
            --
            blockifyEmitSyntheticToken Close_BT
            when (not $ isEmpty $ blockifyAnchorBrackets 𝑎) $ \ () → do
              let expectedCloses = openBracketInfoCloses $ viewΩ someL $ firstElem $ blockifyAnchorBrackets 𝑎
              blockifyError (Some t) $ concat $ inbetween " " 
                [ "bracket CLOSE"
                , concat $ inbetween " OR " $ mapOn (iter expectedCloses) $ \ tᵢ → concat ["‹",getDisplayToken ⋕! tᵢ,"›" ]
                , "before block CLOSE triggered by this TOKEN"
                ]
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

blockify ∷ (Ord t,Pretty t) ⇒ BlockifyArgs t → 𝕊 → 𝕍 (ParserToken t) → Doc ∨ 𝕍 (ParserToken t)
blockify ρ so ts = finalizeTokens ∘ vecC ^$ oevalBlockifyM₀ ρ so ts blockifyM
