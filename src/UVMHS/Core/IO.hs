module UVMHS.Core.IO
  ( module UVMHS.Core.IO 
  , module System.IO.Error
  , module System.Exit
  ) where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Monads ()
import UVMHS.Core.Time
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
exitIO = HS.exitWith $ HS.ExitSuccess

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

profile ∷ IO a → IO (a ∧ TimeD ∧ 𝔻)
profile xM = do
  gc
  s₁ ← Stat.getRTSStats
  let (n₁,u₁) = (Stat.major_gcs s₁,Stat.cumulative_live_bytes s₁)
  t₁ ← now
  x ← xM
  t₂ ← now
  gc
  s₂ ← Stat.getRTSStats
  let (n₂,u₂) = (Stat.major_gcs s₂,Stat.cumulative_live_bytes s₂)
      t'      = t₂ ⨺ t₁
      m       = dbl (HS.fromIntegral u₂ - HS.fromIntegral u₁ ∷ ℕ) 
                / dbl (HS.fromIntegral n₂ - HS.fromIntegral n₁ ∷ ℕ)
  return $ x :* t' :* m
