module UVMHS.Core.IO where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Effects
import UVMHS.Core.Monads ()

import System.Exit
import System.IO.Unsafe

import qualified Data.Text.IO as Text
import qualified Prelude as HS
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified System.IO as HS
import qualified System.IO.Unsafe as UNSAFE

writeOut ∷ 𝕊 → IO ()
writeOut = Text.putStr

out ∷ 𝕊 → IO ()
out s = exec [writeOut s,writeOut "\n"]

outs ∷ (ToIter 𝕊 t) ⇒ t → IO ()
outs ss = eachWith ss out

shout ∷ (Show a) ⇒ a → IO ()
shout = out ∘ show𝕊

writeErr ∷ 𝕊 → IO ()
writeErr = Text.hPutStr HS.stderr

err ∷ 𝕊 → IO ()
err s = exec [writeErr s,writeErr "\n"]

abortIO ∷ IO a
abortIO = exitWith $ ExitFailure $ tohs $ intΩ32 1

failIO ∷ 𝕊 → IO a
failIO = HS.fail ∘ chars

stdin ∷ IO 𝕊
stdin = Text.getContents

args ∷ IO (𝐼 𝕊)
args = map string ∘ iter ^$ Environment.getArgs

read ∷ 𝕊 → IO 𝕊
read = io ∘ Text.readFile ∘ chars

write ∷ 𝕊 → 𝕊 → IO ()
write fn = io ∘ Text.writeFile (chars fn)

trace ∷ 𝕊 → a → a
trace s x = unsafePerformIO $ do
  out s
  return x

traceM ∷ (Monad m) ⇒ 𝕊 → m ()
traceM msg = trace msg skip

optionIO ∷ 𝑂 a → IO a
optionIO None = abortIO
optionIO (Some x) = return x

shell ∷ 𝕊 → IO (𝔹 ∧ 𝕊 ∧ 𝕊)
shell c = do
  (e,o,r) ← Process.readCreateProcessWithExitCode (Process.shell $ chars c) []
  return (e ≡ Exit.ExitSuccess :꘍ string o :꘍ string r)

shellOK ∷ 𝕊 → IO 𝕊
shellOK c = do
  (e :꘍ o :꘍ r) ← shell c
  case e of
    True → return o
    False → do
      out r
      failIO r

shelll ∷ 𝕊 → IO (𝔹 ∧ 𝕊 ∧ 𝕊)
shelll c = do
  out $ "(sh) > " ⧺ c
  shell c

shelllOK ∷ 𝕊 → IO 𝕊
shelllOK c = do
  out $ "(sh) > " ⧺ c
  shellOK c

ioUNSAFE ∷ IO a → a
ioUNSAFE = UNSAFE.unsafePerformIO
