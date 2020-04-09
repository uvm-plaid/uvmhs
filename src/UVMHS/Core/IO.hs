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

------------------
-- Standard Out --
------------------

owrite ∷ 𝕊 → IO ()
owrite = BS.putStr ∘ Text.encodeUtf8

out ∷ 𝕊 → IO ()
out s = exec [owrite s,owrite "\n"]

outs ∷ (ToIter 𝕊 t) ⇒ t → IO ()
outs ss = eachOn ss out

oflush ∷ IO ()
oflush = IO.hFlush IO.stdout

shout ∷ (Show a) ⇒ a → IO ()
shout = out ∘ show𝕊

trace ∷ 𝕊 → ()
trace s = unsafePerformIO $ do
  out s
  oflush
  return ()

traceM ∷ (Monad m) ⇒ 𝕊 → m ()
traceM msg =
  let _ = trace msg 
  in skip

------------------
-- Standard Err --
------------------

ewrite ∷ 𝕊 → IO ()
ewrite = BS.hPutStr IO.stderr ∘ Text.encodeUtf8

err ∷ 𝕊 → IO ()
err s = exec [ewrite s,ewrite "\n"]

eflush ∷ IO ()
eflush = IO.hFlush IO.stderr

-----------------
-- Standard In --
-----------------

iread ∷ IO 𝕊
iread = Text.decodeUtf8 ^$ BS.getContents

iargs ∷ IO (𝐿 𝕊)
iargs = map (list ∘ map string) Env.getArgs

ilocalArgs ∷ 𝐿 𝕊 → IO a → IO a
ilocalArgs args = Env.withArgs $ lazyList $ map chars $ iter args

------------
-- Errors --
------------

abortIO ∷ IO a
abortIO = exitWith $ ExitFailure $ tohs $ 𝕫64 1

failIO ∷ 𝕊 → IO a
failIO = HS.fail ∘ chars

-----------
-- Files --
-----------

fread ∷ 𝕊 → IO 𝕊
fread = Text.decodeUtf8 ^∘ BS.readFile ∘ chars

fwrite ∷ 𝕊 → 𝕊 → IO ()
fwrite file = BS.writeFile (chars file) ∘ Text.encodeUtf8

fappend ∷ 𝕊 → 𝕊 → IO ()
fappend fn = BS.appendFile (chars fn) ∘ Text.encodeUtf8

fcopy ∷ 𝕊 → 𝕊 → IO ()
fcopy fr to = Dir.copyFile (chars fr) $ chars to

-----------------
-- Directories --
-----------------

dfilesAll ∷ IO (𝐿 𝕊)
dfilesAll = sort ∘ list ∘ map string ^$ Dir.listDirectory $ chars "."

dfiles ∷ IO (𝐿 𝕊)
dfiles = do
  files ← dfilesAll
  return $ list $ filterOn files $ \ f → case fst ^$ uncons f of
    None → False
    Some c → c ≢ '.'

din ∷ 𝕊 → IO a → IO a
din = Dir.withCurrentDirectory ∘ chars

dtouch ∷ 𝕊 → IO ()
dtouch = Dir.createDirectoryIfMissing True ∘ chars

drremove ∷ 𝕊 → IO ()
drremove = Dir.removeDirectoryRecursive ∘ chars

-----------
-- Paths --
-----------

pexists ∷ 𝕊 → IO 𝔹
pexists = Dir.doesPathExist ∘ chars

pfilename ∷ 𝕊 → 𝕊
pfilename = string ∘ FP.takeFileName ∘ chars

pbasename ∷ 𝕊 → 𝕊
pbasename = string ∘ FP.takeBaseName ∘ chars

pdirectory ∷ 𝕊 → 𝕊
pdirectory = string ∘ FP.takeDirectory ∘ chars

pextension ∷ 𝕊 → 𝕊
pextension = string ∘ FP.takeExtension ∘ chars

-----------
-- Shell --
-----------

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

--------
-- GC --
--------

gc ∷ IO ()
gc = Mem.performGC

---------------
-- Profiling --
---------------

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
  do out $ "TIMING: " ⧺ s ; oflush
  x :* t ← time f
  do out $ "RESULT: " ⧺ show𝕊 t ; oflush
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
  do out $ "TIMING: " ⧺ s ; oflush
  x :* t ← timeIO xM
  do out $ "RESULT: " ⧺ show𝕊 t ; oflush
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

---------------
-- Unsafe IO --
---------------

ioUNSAFE ∷ IO a → a
ioUNSAFE = IO_UNSAFE.unsafePerformIO
