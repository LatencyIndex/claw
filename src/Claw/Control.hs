module Claw.Control (
    (&),
    (<&>),
    (<=<),
    isPrefixOf,
    isSuffixOf,
    mapBoth,
    sort,
    sortOn,
) where

import Control.Monad ((<=<))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf, isSuffixOf, sort, sortOn)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)
