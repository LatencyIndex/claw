module Claw.Utils.List (
    mkTabular,
    padL,
    padR,
) where

padL :: a -> Int -> [a] -> [a]
padL padding width xs = replicate nb_missing padding ++ xs
  where
    nb_missing = max 0 (width - length xs)

padR :: a -> Int -> [a] -> [a]
padR padding width xs = xs ++ replicate nb_missing padding
  where
    nb_missing = max 0 (width - length xs)

-- | Pad every element to match the longest one.
mkTabular :: a -> [[a]] -> [[a]]
mkTabular x xs = padR x maxLen <$> xs
  where
    maxLen = maximum (length <$> xs)
