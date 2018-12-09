module UVMHS.Core.Data.Unit where

import UVMHS.Core.Classes

instance Null () where null = ()
instance Append () where () ⧺ () = ()
instance Monoid ()

