{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Claw.Utils.PrettyPrint (
    Pretty,
    pprint,
    pshow,
    showBytes,
) where

import Claw.Utils.List (mkTabular)
import Data.List (transpose)
import Numeric (showFFloatAlt)

class Pretty a where
    pshow :: a -> String

instance {-# OVERLAPPABLE #-} (Show a) => Pretty a where
    pshow = show

pprint :: (Pretty a) => a -> IO ()
pprint = putStrLn . pshow

-- Render as column-aligned table
instance Pretty [[String]] where
    pshow rows = unlines (unwords <$> aligned_rows')
      where
        rows' = mkTabular "" rows -- Fill missing cells with blanks.
        cols' = transpose rows'
        aligned_cols' = mkTabular ' ' <$> cols' -- Pad cells in a column to the same width.
        aligned_rows' = transpose aligned_cols'

showBytes :: Int -> String
showBytes n = showFFloatAlt (Just 2) ratio prefix
  where
    binarySi =
        [ (2 ^ (10 :: Int), " kB"),
          (2 ^ (20 :: Int), " MB"),
          (2 ^ (30 :: Int), " GB"),
          (2 ^ (40 :: Int), " TB"),
          (2 ^ (50 :: Int), " PB")
        ]
    canFit (qty, _siPrefix) = qty <= abs n
    (unit, prefix) = last $ (1, "") : takeWhile canFit binarySi
    ratio = fromIntegral n / fromIntegral unit :: Double
