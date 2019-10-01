module UVMHS.Core.Data.Unit where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Null () where null = ()
instance Append () where () ⧺ () = ()
instance Monoid ()

instance Zero () where zero = ()
instance Plus () where (+) = const $ const ()
instance Additive ()
