{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Claw.Internal.PrettyPrint (
    Pretty,
    pprint,
) where

import Claw.Internal.List (mkTabular)
import Data.List (transpose)

class Pretty a where
    pprint :: a -> IO ()

instance {-# OVERLAPPABLE #-} (Show a) => Pretty a where
    pprint = print
instance Pretty String where
    pprint = putStrLn

-- Render as column-aligned table
instance Pretty [[String]] where
    pprint rows = putStrLn $ unlines $ unwords <$> aligned_rows'
      where
        rows' = mkTabular "" rows -- Fill missing cells with blanks.
        cols' = transpose rows'
        aligned_cols' = mkTabular ' ' <$> cols' -- Pad cells in a column to the same width.
        aligned_rows' = transpose aligned_cols'
