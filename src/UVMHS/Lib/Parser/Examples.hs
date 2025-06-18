module UVMHS.Lib.Parser.Examples where

import UVMHS.Core
import UVMHS.Lib.Parser.GenParser
import UVMHS.Lib.Parser.GenLexer
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.RawParser
import UVMHS.Lib.Parser.Regex
import UVMHS.Lib.Parser.StdParser
import UVMHS.Lib.Pretty

testParsingSmall ∷ IO ()
testParsingSmall = gparseIOMain parser "<small example>" input
  where
    parser = gpWord $ 𝕤 "xyzxyz"
    input = tokens "xyzxycxyz"

testParsingMultiline ∷ IO ()
testParsingMultiline = gparseIOMain parser "<multiline example>" input
  where
    parser = exec $ inbetween (gpWord $ 𝕤 "\n") $ list $ replicateI (𝕟 7) $ \ n → gpNewContext "line" $ gpWord ("xyz" ⧺ show𝕊 n)
    input = tokens "xyz0\nxyz1\nxyz2\nxyc3\nxyz4\nxyz5\nxyz6\n"

testParsingBranching ∷ IO ()
testParsingBranching = gparseIOMain parser "<branching example>" input
  where
    parser ∷ GenParser ℂ 𝕊
    parser = tries
      [ gpNewContext "XXX*" $ tries
          [ gpRender (formats [FG pink]) $ gpWordRet "xxxy"
          , gpRender (formats [FG pink]) $ gpWordRet "xxxz"
          ]
      , gpNewContext "XXXZ" $ do
          x ← gpErr "XX" $ gpRender (formats [FG blue]) $ gpWordRet "xx"
          y ← gpErr "XZ" $ gpRender (formats [FG green]) $ gpWordRet "xz"
          return $ x ⧺ y
      , gpNewContext "XXZZ" $ gpWordRet "xxzz"
      , gpNewContext "XXXAorB" $ gpRender (formats [FG teal]) $ do
          x ← gpWordRet "xxx"
          y ← single ^$ tries
            [ gpTokRet 'a'
            , gpTokRet 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens "xxxx"

-- testParsingAmbiguity ∷ IO ()
-- testParsingAmbiguity = gparseIOMain parser input
--   where
--     parser = concat ^$ oneOrMore $ tries
--       [ ppFG yellow ∘ ppString ∘ single ^$ gpTok 'y'
--       , ppFG green ∘ ppString ∘ single ^$ gpTok 'x'
--       , ppFG blue ∘ ppString ^$ gpWord "xx"
--       ]
--     input = tokens "xxx"

testParsingGreedy ∷ IO ()
testParsingGreedy = gparseIOMain parser "<greedy example>" input
  where
    parser = concat ^$ oneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ gpRender (formats [FG yellow]) $ gpRawParser $ rpToken 'y'
      , ppFG green ∘ ppString ∘ single ^$ gpRender (formats [FG green]) $ gpRawParser $ rpToken 'x'
      , ppFG blue ∘ ppString ^$ gpRender (formats [FG yellow]) $ gpWordRet "xx"
      ]
    input = tokens "xxx"

testParsingGreedyAmbiguity ∷ IO ()
testParsingGreedyAmbiguity = gparseIOMain parser "<greedy ambiguity example>" input
  where
    parser = concat ^$ oneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ gpRender (formats [FG yellow]) $ gpRawParser $ rpToken 'y'
      , tries
          [ ppFG blue ∘ ppString ^$ gpRender (formats [FG blue]) $ gpWordRet "x"
          , ppFG pink ∘ ppString ^$ gpRender (formats [FG pink]) $ gpWordRet "xx"
          ]
      , ppFG green ∘ ppString ∘ single ^$ gpRender (formats [FG green]) $ gpRawParser $ rpToken 'x'
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = gparseIOMain parser "<success example>" input
  where
    parser = concat ^$ oneOrMore $ tries
      [ gpRender (formats [FG green]) $ gpWord $ 𝕤 "xx"
      , gpRender (formats [FG blue]) $ gpWord $ 𝕤 "yy"
      ]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = gparseIOMain (string ^$ many $ gpRawParser $ rpToken 'x') "<error newline example>" $ tokens "xxx\nx"

testParsingErrorEof ∷ IO ()
testParsingErrorEof = gparseIOMain (exec $ replicate (𝕟 3) $ gpTok 'x') "<error eof example>" $ tokens "xx"

testTokenizeSimple ∷ IO ()
testTokenizeSimple =
  let rgx = lWord "x" ▷ oepsRegex ()
      dfa = compileRegex rgx
  in glexIOMain (GenLexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize simple example>" $ tokens "xxx"

testTokenize ∷ IO ()
testTokenize =
  let rgx = concat [lWord "x",lWord "xy",lWord "y"] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in glexIOMain (GenLexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize example>" $ tokens "xxyxyxyxyxxyy"

testTokenizeFailure1 ∷ IO ()
testTokenizeFailure1 =
  let rgx = concat
        [ lWord "x" ▷ fepsRegex (formats [FG green]) ▷ lepsRegex (𝕟64 2)
        , lWord "x" ▷ fepsRegex (formats [FG yellow]) ▷ lepsRegex (𝕟64 1)
        , lWord "xx" ▷ fepsRegex (formats [FG blue])
        , lWord "xy" ▷ fepsRegex (formats [FG teal])
        , lWord "xz" ▷ fepsRegex (formats [FG pink])
        ] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in glexIOMain (GenLexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize failure1 example>" $ tokens "xxxxy"

testTokenizeFailure2 ∷ IO ()
testTokenizeFailure2 =
  let rgx = concat
        [ lWord "x" ▷ fepsRegex (formats [FG green]) ▷ lepsRegex (𝕟64 2)
        , lWord "x" ▷ fepsRegex (formats [FG yellow]) ▷ lepsRegex (𝕟64 1)
        , lWord "xx" ▷ fepsRegex (formats [FG blue])
        , lWord "xy" ▷ fepsRegex (formats [FG teal])
        , lWord "xz" ▷ fepsRegex (formats [FG pink])
        ] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in glexIOMain (GenLexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize failiure2 example>" $ tokens "xxxyxxxzxc"
