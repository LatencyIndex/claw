-- | Prelude for interactive use.
-- Unfortunately in Haskell module exports are in a flat namespace.
-- I.e. it is not possible in module A to re-export B.C.f so that it could be accessed as A.C.f
-- So those parts of the prelude that we want to remain nested are imported in init.ghci
module Claw.Prelude (
    module Claw.FS.Path,
    module Claw.FS.Read,
    module Claw.FS.Write,
    module Claw.Time,
    (&),
    (<&>),
    (<=<),
    both,
    isPrefixOf,
    isSuffixOf,
    sort,
    sortOn,
) where

import Control.Monad ((<=<))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, isSuffixOf, sort, sortOn)
import Data.Tuple.Extra (both)

import Claw.FS.Path
import Claw.FS.Read
import Claw.FS.Write
import Claw.Time
