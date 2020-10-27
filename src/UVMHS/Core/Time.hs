module UVMHS.Core.Time where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import qualified Data.Time.Clock as Time
import qualified Prelude as HS

infixl 5 ⨺,⨹

type Time = Time.UTCTime
type TimeD = Time.NominalDiffTime

secondsTimeD ∷ TimeD → 𝔻
secondsTimeD = HS.realToFrac

instance Zero TimeD where {zero = HS.fromIntegral 0}
instance Plus TimeD where {(+) = (HS.+)}
instance One TimeD where {one = HS.fromIntegral 1}
instance Times TimeD where {(×) = (HS.*)}

instance Additive TimeD
instance Multiplicative TimeD

(⨺) ∷ Time → Time → TimeD
(⨺) = Time.diffUTCTime

(⨹) ∷ Time → TimeD → Time
(⨹) = flip Time.addUTCTime

now ∷ IO Time
now = Time.getCurrentTime
