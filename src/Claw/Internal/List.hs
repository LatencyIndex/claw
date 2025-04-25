module Claw.Internal.List (
    align,
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
mkTabular x = align (padR x)

-- | Align a column by applying a padding function to all elements.
-- The padding function must pad its argument to a specified length, if shorter.
-- E.g. it can left-pad, right-pad, or pad both sides for central alignment.
align :: (Int -> [a] -> [a]) -> [[a]] -> [[a]]
align padFn xs = padFn maxLen <$> xs
  where
    maxLen = maximum (length <$> xs)
