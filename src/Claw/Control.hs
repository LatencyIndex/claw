module Claw.Control (
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
