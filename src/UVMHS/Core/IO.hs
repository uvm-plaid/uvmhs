module UVMHS.Core.IO
  ( module UVMHS.Core.IO
  , module System.IO.Error
  , module System.Exit
  ) where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Monads ()
import UVMHS.Core.FilePath

import System.Exit     (ExitCode)
import System.IO.Error (IOError)

import qualified Control.Exception     as HS
import qualified Data.ByteString       as BS
import qualified Data.IORef            as IORef
import qualified Data.Text.Encoding    as Text
import qualified GHC.IO.Handle         as IO
import qualified GHC.Stats             as Stat
import qualified Prelude               as HS
import qualified System.Directory      as Dir
import qualified System.Directory      as HS
import qualified System.Environment    as Env
import qualified System.Exit           as Exit
import qualified System.Exit           as HS
import qualified System.IO             as IO
import qualified System.IO.Error       as HS
import qualified System.IO.Unsafe      as IO
import qualified System.Mem            as Mem
import qualified System.Process        as Proc

infix 1 ↢

----------------
-- REFERENCES --
----------------

type 𝑅 = IORef.IORef

ref ∷ a → IO (𝑅 a)
ref = IORef.newIORef

deref ∷ 𝑅 a → IO a
deref = IORef.readIORef

(↢) ∷ 𝑅 a → a → IO ()
(↢) = IORef.writeIORef
---------------
-- Unsafe IO --
---------------

io_UNSAFE ∷ IO a → a
io_UNSAFE = IO.unsafePerformIO

----------
-- INIT --
----------

initUVMHS ∷ IO ()
initUVMHS = do
  IO.hSetEncoding IO.stdin  IO.utf8
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

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
trace s = io_UNSAFE $ do
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

redirectErrToOut ∷ IO ()
redirectErrToOut = IO.hDuplicateTo IO.stdout IO.stderr

-----------------
-- Standard In --
-----------------

iread ∷ IO 𝕊
iread = Text.decodeUtf8 ^$ BS.getContents

iargs ∷ IO (𝐿 𝕊)
iargs = map (list ∘ map string) Env.getArgs

ilocalArgs ∷ 𝐿 𝕊 → IO a → IO a
ilocalArgs args = Env.withArgs $ lazyList $ map tohsChars $ iter args

------------
-- Errors --
------------

abortIOCode ∷ ℤ64 → IO a
abortIOCode i = HS.exitWith $ HS.ExitFailure $ tohs i

abortIO ∷ IO a
abortIO = abortIOCode $ 𝕫64 1

exitIO ∷ IO a
exitIO = HS.exitWith HS.ExitSuccess

failIO ∷ 𝕊 → IO a
failIO = HS.fail ∘ tohsChars

throwIO ∷ IOError → IO a
throwIO = HS.ioError

catchIO ∷ IO a → (IOError → IO a) → IO a
catchIO = HS.catchIOError

cleanExit ∷ IO a → IO a
cleanExit xM = HS.catch xM (\ (c ∷ ExitCode) → shout c ≫ exitIO)

-----------
-- Files --
-----------

fread ∷ ℙ → IO 𝕊
fread = Text.decodeUtf8 ^∘ BS.readFile ∘ tohsChars ∘ unℙ

fwrite ∷ ℙ → 𝕊 → IO ()
fwrite file = BS.writeFile (tohsChars $ unℙ file) ∘ Text.encodeUtf8

fappend ∷ ℙ → 𝕊 → IO ()
fappend fn = BS.appendFile (tohsChars $ unℙ fn) ∘ Text.encodeUtf8

fcopy ∷ ℙ → ℙ → IO ()
fcopy fr to = Dir.copyFile (tohsChars $ unℙ fr) $ tohsChars $ unℙ to

-----------------
-- Directories --
-----------------

dfilesAll ∷ IO (𝐿 ℙ)
dfilesAll = sort ∘ list ∘ map (ℙ ∘ string) ^$ Dir.listDirectory $ tohsChars "."

dfiles ∷ IO (𝐿 ℙ)
dfiles = do
  files ← dfilesAll
  return $ list $ filterOn files $ \ f → case firstElem $ unℙ f of
    None → False
    Some c → c ≢ '.'

din ∷ ℙ → IO a → IO a
din = Dir.withCurrentDirectory ∘ tohsChars ∘ unℙ

dtouch ∷ ℙ → IO ()
dtouch = Dir.createDirectoryIfMissing True ∘ tohsChars ∘ unℙ

drremove ∷ ℙ → IO ()
drremove = Dir.removeDirectoryRecursive ∘ tohsChars ∘ unℙ

dcurrent ∷ IO ℙ
dcurrent = ℙ ∘ string ^$ HS.getCurrentDirectory

-----------
-- Paths --
-----------

pexists ∷ ℙ → IO 𝔹
pexists = Dir.doesPathExist ∘ tohsChars ∘ unℙ

-----------
-- Shell --
-----------

shell ∷ 𝕊 → IO (𝔹 ∧ 𝕊 ∧ 𝕊)
shell c = do
  (e,o,r) ← Proc.readCreateProcessWithExitCode (Proc.shell $ tohsChars c) []
  return $ (e ≡ Exit.ExitSuccess) :* string o :* string r

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

-- takes a quantity in nanoseconds and gives a more human readable version
humanReadableTime ∷ 𝔻 → 𝕊 ∧ 𝔻
humanReadableTime t = if 
  | t ≥ 1000000000 → "s"  :* (t / 1000000000)
  | t ≥ 1000000    → "ms" :* (t / 1000000)
  | t ≥ 1000       → "μs" :* (t / 1000)
  | otherwise      → "ns" :* t

-- takes a quantity in bytes and gives a more human readable version
humanReadableBytes ∷ 𝔻 → 𝕊 ∧ 𝔻
humanReadableBytes b = if 
  | b ≥ 1000000000 → "GB" :* (b / 1000000000)
  | b ≥ 1000000    → "MB" :* (b / 1000000)
  | b ≥ 1000       → "KB" :* (b / 1000)
  | otherwise       → "B"  :* b

-- requires +RTS -T
nowCPU ∷ IO ℤ64
nowCPU = do
  gc
  s ← Stat.getRTSStats
  return $ Stat.cpu_ns s

-- returns time in nanoseconds
-- requires +RTS -T
timeIO ∷ (() → IO a) → IO (a ∧ ℕ64)
timeIO f = do
  t₁ ← nowCPU
  x ← f ()
  t₂ ← nowCPU
  return $ x :* natΩ64 (t₂ - t₁)

-- requires +RTS -T
timeIOLog ∷ 𝕊 → (() → IO a) → IO a
timeIOLog s f = do
  out $ "TIMING: " ⧺ s 
  oflush
  x :* t ← timeIO f
  let u :* t' = humanReadableTime $ dbl t
  out $ "CPU TIME ELAPSED: " ⧺ show𝕊 t' ⧺ " " ⧺ u
  oflush
  return x

-- requires +RTS -T
profileIO ∷ (() → IO a) → IO (a ∧ ℕ64 ∧ 𝔻)
profileIO xM = do
  gc
  s₁ ← Stat.getRTSStats
  x ← xM ()
  gc
  s₂ ← Stat.getRTSStats
  let -- total number of major GCs
      n₁ = Stat.major_gcs s₁
      -- sum of live bytes across all major GCs
      u₁ = Stat.cumulative_live_bytes s₁
      -- total CPU time at previous GC in nanoseconds
      t₁ = Stat.cpu_ns s₁
      --
      n₂ = Stat.major_gcs s₂
      u₂ = Stat.cumulative_live_bytes s₂
      t₂ = Stat.cpu_ns s₂
      --
      -- elapsed CPU time in seconds
      t = natΩ64 $ t₂ - t₁
      -- average live data across GCs in bytes
      m  = dbl (u₂ - u₁) / dbl (n₂ - n₁)
  return $ x :* t :* m

-- requires +RTS -T
profileIOLog ∷ 𝕊 → (() → IO a) → IO a
profileIOLog s xM = do
  out $ "TIMING AND MEMORY: " ⧺ s 
  oflush
  x :* t :* m ← profileIO xM
  let ut :* t' = humanReadableTime $ dbl t
      um :* m' = humanReadableBytes m
  out $ "CPU TIME ELAPSED: " ⧺ show𝕊 t' ⧺ " " ⧺ ut
  out $ "AVERAGE MEMORY USED: " ⧺ show𝕊 m' ⧺ " " ⧺ um
  oflush
  return x
