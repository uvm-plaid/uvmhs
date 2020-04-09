module UVMHS.Core.IO where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Monads ()
import UVMHS.Core.Time

import System.Exit
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified GHC.Stats  as Stat
import qualified Prelude as HS
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath.Posix as FP
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO_UNSAFE
import qualified System.Mem as Mem
import qualified System.Process as Proc

initUVMHS ∷ IO ()
initUVMHS = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stdin IO.utf8

writeOut ∷ 𝕊 → IO ()
writeOut = BS.putStr ∘ Text.encodeUtf8

out ∷ 𝕊 → IO ()
out s = exec [writeOut s,writeOut "\n"]

outs ∷ (ToIter 𝕊 t) ⇒ t → IO ()
outs ss = eachOn ss out

shout ∷ (Show a) ⇒ a → IO ()
shout = out ∘ show𝕊

flushOut ∷ IO ()
flushOut = IO.hFlush IO.stdout

writeErr ∷ 𝕊 → IO ()
writeErr = BS.hPutStr IO.stderr ∘ Text.encodeUtf8

err ∷ 𝕊 → IO ()
err s = exec [writeErr s,writeErr "\n"]

flushErr ∷ IO ()
flushErr = IO.hFlush IO.stderr

abortIO ∷ IO a
abortIO = exitWith $ ExitFailure $ tohs $ 𝕫64 1

failIO ∷ 𝕊 → IO a
failIO = HS.fail ∘ chars

stdin ∷ IO 𝕊
stdin = Text.decodeUtf8 ^$ BS.getContents

readFile ∷ 𝕊 → IO 𝕊
readFile = Text.decodeUtf8 ^∘ BS.readFile ∘ chars

writeFile ∷ 𝕊 → 𝕊 → IO ()
writeFile file = BS.writeFile (chars file) ∘ Text.encodeUtf8

trace ∷ 𝕊 → a → a
trace s = unsafePerformIO $ do
  out s
  flushOut
  return id

traceM ∷ (Monad m) ⇒ 𝕊 → m ()
traceM msg = trace msg skip

optionIO ∷ 𝑂 a → IO a
optionIO None = abortIO
optionIO (Some x) = return x

shell ∷ 𝕊 → IO (𝔹 ∧ 𝕊 ∧ 𝕊)
shell c = do
  (e,o,r) ← Proc.readCreateProcessWithExitCode (Proc.shell $ chars c) []
  return $ e ≡ Exit.ExitSuccess :* string o :* string r

shellOK ∷ 𝕊 → IO 𝕊
shellOK c = do
  (e :* o :* r) ← shell c
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
ioUNSAFE = IO_UNSAFE.unsafePerformIO

gc ∷ IO ()
gc = Mem.performGC

time ∷ (() → a) → IO (a ∧ TimeD)
time f = do
  gc
  t₁ ← now
  let x = f ()
  gc
  t₂ ← now
  return $ x :* (t₂ ⨺ t₁)

rtime ∷ 𝕊 → (() → a) → IO a
rtime s f = do
  do out $ "TIMING: " ⧺ s ; flushOut
  x :* t ← time f
  do out $ "RESULT: " ⧺ show𝕊 t ; flushOut
  return x

timeIO ∷ IO a → IO (a ∧ TimeD)
timeIO xM = do
  gc
  t₁ ← now
  x ← xM
  gc
  t₂ ← now
  return $ x :* (t₂ ⨺ t₁)

rtimeIO ∷ 𝕊 → IO a → IO a
rtimeIO s xM = do
  do out $ "TIMING: " ⧺ s ; flushOut
  x :* t ← timeIO xM
  do out $ "RESULT: " ⧺ show𝕊 t ; flushOut
  return x

profile ∷ (() → a) → IO (TimeD ∧ 𝔻)
profile f = do
  gc
  s₁ ← Stat.getRTSStats
  let (n₁,u₁) = (Stat.major_gcs s₁,Stat.cumulative_live_bytes s₁)
  t₁ ← now
  let _ = f ()
  t₂ ← now
  s₂ ← Stat.getRTSStats
  let (n₂,u₂) = (Stat.major_gcs s₂,Stat.cumulative_live_bytes s₂)
  return $ (t₂ ⨺ t₁) :* (dbl (HS.fromIntegral u₂ - HS.fromIntegral u₁ ∷ ℕ) / dbl (HS.fromIntegral n₂ - HS.fromIntegral n₁ ∷ ℕ))

askArgs ∷ IO (𝐿 𝕊)
askArgs = map (list ∘ map string) Env.getArgs

localArgs ∷ 𝐿 𝕊 → IO a → IO a
localArgs args = Env.withArgs $ lazyList $ map chars $ iter args

files ∷ IO (𝐿 𝕊)
files = list ∘ map string ^$ Dir.listDirectory $ chars "."

indir ∷ 𝕊 → IO a → IO a
indir = Dir.withCurrentDirectory ∘ chars

touchDirs ∷ 𝕊 → IO ()
touchDirs = Dir.createDirectoryIfMissing True ∘ chars

copyFile ∷ 𝕊 → 𝕊 → IO ()
copyFile fr to = Dir.copyFile (chars fr) $ chars to

writeAppend ∷ 𝕊 → 𝕊 → IO ()
writeAppend fn = BS.appendFile (chars fn) ∘ Text.encodeUtf8

pathFn ∷ 𝕊 → 𝕊
pathFn = string ∘ FP.takeFileName ∘ chars

pathBn ∷ 𝕊 → 𝕊
pathBn = string ∘ FP.takeBaseName ∘ chars

pathDir ∷ 𝕊 → 𝕊
pathDir = string ∘ FP.takeDirectory ∘ chars

pathExt ∷ 𝕊 → 𝕊
pathExt = string ∘ FP.takeExtension ∘ chars

rmDirs ∷ 𝕊 → IO ()
rmDirs = Dir.removeDirectoryRecursive ∘ chars

pathExists ∷ 𝕊 → IO 𝔹
pathExists = Dir.doesPathExist ∘ chars
