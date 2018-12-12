module UVMHS.Lib.Parser.Examples where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserInput

testParsingSmall ∷ IO ()
testParsingSmall = parseIOMain parser input
  where
    parser = pWord "xyzxyz"
    input = tokens "xyzxycxyz"

testParsingMultiline ∷ IO ()
testParsingMultiline = parseIOMain parser input
  where
    parser = exec $ inbetween (void $ pWord "\n") $ list $ repeatI 7 $ \ n → pNew "line" $ void $ pWord ("xyz" ⧺ show𝕊 (succ n))
    input = tokens "xyz1\nxyz2\nxyz3\nxyc4\nxyz5\nxyz6\nxyz7\n"

testParsingBranching ∷ IO ()
testParsingBranching = parseIOMain parser input
  where
    parser ∷ Parser ℂ 𝕊
    parser = tries
      [ pNew "XXX*" $ tries
          [ pRender (FG pink) $ pWord "xxxy"
          , pRender (FG pink) $ pWord "xxxz"
          ]
      , pNew "XXXZ" $ do
          x ← pErr "XX" $ pRender (FG blue) $ pWord "xx"
          y ← pErr "XZ" $ pRender (FG green) $ pWord "xz"
          return $ x ⧺ y
      , pNew "XXZZ" $ pWord "xxzz"
      , pNew "XXXAorB" $ pRender (FG teal) $ do
          x ← pWord "xxx"
          y ← single ^$ tries
            [ pLit 'a'
            , pLit 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ 𝑆 (ParserToken ℂ)
    input = tokens "xxxx"
    
-- testParsingAmbiguity ∷ IO ()
-- testParsingAmbiguity = parseIOMain parser input
--   where
--     parser = concat ^$ pOneOrMore $ tries 
--       [ ppFG yellow ∘ ppText ∘ single ^$ pLit 'y'
--       , ppFG green ∘ ppText ∘ single ^$ pLit 'x'
--       , ppFG blue ∘ ppText ^$ pWord "xx" 
--       ]
--     input = tokens "xxx"

testParsingGreedy ∷ IO ()
testParsingGreedy = parseIOMain parser input
  where
    parser = concat ^$ pOneOrMore $ tries 
      [ ppFG yellow ∘ ppText ∘ single ^$ pRender (FG yellow) $ pLit 'y'
      , ppFG green ∘ ppText ∘ single ^$ pRender (FG green) $ pLit 'x'
      , ppFG blue ∘ ppText ^$ pRender (FG yellow) $ pWord "xx" 
      ]
    input = tokens "xxx"

testParsingGreedyAmbiguity ∷ IO ()
testParsingGreedyAmbiguity = parseIOMain parser input
  where
    parser = concat ^$ pOneOrMore $ tries 
      [ ppFG yellow ∘ ppText ∘ single ^$ pRender (FG yellow) $ pLit 'y'
      , tries
          [ ppFG blue ∘ ppText ^$ pRender (FG blue) $ pWord "x" 
          , ppFG pink ∘ ppText ^$ pRender (FG pink) $ pWord "xx" 
          ]
      , ppFG green ∘ ppText ∘ single ^$ pRender (FG green) $ pLit 'x'
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = parseIOMain parser input
  where
    parser = concat ^$ pOneOrMore $ tries [pRender (FG green) $ pWord "xx",pRender (FG blue) $ pWord "yy"]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = parseIOMain (string ^$ pMany $ pLit 'x') $ tokens "xxx\nx"

testParsingErrorEof ∷ IO ()
testParsingErrorEof = parseIOMain (exec $ repeat 3 $ void $ pLit 'x') $ tokens "xx"

testTokenizeSimple ∷ IO ()
testTokenizeSimple = tokenizeIOMain (single (pLit 'x')) $ tokens "xxx"

testTokenize ∷ IO ()
testTokenize = tokenizeIOMain (list [pWord "x",pWord "xy",pWord "y"]) $ tokens "xxyxyxyxyxxyy"

testTokenizeFailure1 ∷ IO ()
testTokenizeFailure1 = tokenizeIOMain
  (list 
     [ pRender (FG green) $ pWord "x"
     , pRender (FG yellow) $ pWord "x"
     , pRender (FG blue) $ pWord "xx"
     , pRender (FG teal) $ pWord "xy"
     , pRender (FG pink) $ pWord "xz"
     ]) 
  $ tokens "xxxxy"

testTokenizeFailure2 ∷ IO ()
testTokenizeFailure2 = tokenizeIOMain
  (list 
     [ pRender (FG green) $ pWord "x"
     , pRender (FG yellow) $ pWord "x"
     , pRender (FG blue) $ pWord "xx"
     , pRender (FG teal) $ pWord "xy"
     , pRender (FG pink) $ pWord "xz"
     ]) 
  $ tokens "xxxyxxxzxc"
