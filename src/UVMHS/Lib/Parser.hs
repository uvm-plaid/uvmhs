module UVMHS.Lib.Parser
  -- * UTILITY TYPES
  ( Loc(..),LocRange(..)
  , ParserContext(..)
  , SrcCxt(..),srcCxt₀
  , ParserErrorInfo(..)
  , ParserError(..)
  , PreParserToken(..)
  , ParserToken(..),preTokens,finalizeTokens,tokens,renderParserTokens
  -- * GENERIC TYPES
  -- ** REGEX
  , Regex(..)
  , nullRegex
  , resRegex,epsRegex,retRegex,outRegex,lepsRegex,fepsRegex,oepsRegex,uepsRegex
  , atomRegex,tokRegex,ntokRegex,classRegex
  , sumRegex,seqRegex,starRegex
  , DFA(..),compileRegex
  -- ** LEXER
  , GenLexer(..)
  , glex,glexIO,glexIOMain
  -- ** PARSER
  , GenParser(..)
  , gpRender,gpErr,gpFinal,gpTok,gpTokAny,gpTokRet,gpTokShaped,gpTokSat,gpAny,gpWord,gpWordRet
  , gpDie,gpGuard,gpFailEff
  , gpNewExpressionContext,gpNewErrContext,gpNewContext,gpWithContextRendered,gpNewWithContextRendered,gpGetContextRendered
  , runGenParser₀
  , gparse,gparseIO,gparseIOMain
  -- * MIXFIX
  , GenMixesF(..),GenMixfixF(..)
  , gfmixOnlyTerms
  , gfmixPrefix,gfmixPostfix,gfmixInfix,gfmixInfixL,gfmixInfixR,gfmixTerminal
  , gfmixfix,gfmixfixWithContext
  , GenMixes(..),GenMixfix(..)
  , gmixOnlyTerms
  , gmixPrefix,gmixPostfix,gmixInfix,gmixInfixL,gmixInfixR,gmixTerminal
  , gmixfix,gmixfixWithContext
  -- * DEFAULT LANGUAGE ABSTRACTION
  -- ** REGEX
  , StdCharClass(..)
  , lWord,lSpaceOrNL,lSpace,lNL,lName,lNatPre,lNat,lNatCoded,lIntPre,lInt,lDbl,lString
  , lComment,lCommentMLOpen,lCommentMLBodyOpen,lCommentMLBodyClose,lCommentMLBody
  -- ** Lexer
  , TokenClass(..)
  , Token(..)
  , Syntax(..)
  , syntaxBrks,syntaxPuns,syntaxKeys,syntaxPrms,syntaxOprs,syntaxBlks
  , LexerArgs(..)
  , Lexer(..)
  , mkLexer
  , mkBlockifyToken
  , lex,lexIO,lexIOMain
  -- ** Parser
  , Parser(..)
  , pRender,pErr,pFinal,pTok,pTokAny,pTokRet,pTokShaped,pTokSat,pAny
  , pDie,pGuard,pFailEff
  , pTokName,pTokSyntax,pTokSyntaxAny,pTokNatN,pTokNatN64,pTokInt,pTokInt64,pTokNat,pTokNat64
  , pTokDouble,pTokString,pTokChar,pTokBlock,pTokOpen,pTokClose,pTokSep
  , pNewExpressionContext,pNewErrContext,pNewContext,pWithContextRendered,pNewWithContextRendered,pGetContextRendered
  , parse,parseIO,parseIOMain
  , lexParse,lexParseIO,lexParseIOMain
  -- ** Mixfix
  , Mixes(..),Mixfix(..)
  , mixOnlyTerms
  , mixPrefix,mixPostfix,mixInfix,mixInfixL,mixInfixR,mixTerminal
  , mixfix
  ) where

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.Mixfix
import UVMHS.Lib.Parser.GenParser
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex
import UVMHS.Lib.Parser.GenLexer
import UVMHS.Lib.Parser.StdParser
