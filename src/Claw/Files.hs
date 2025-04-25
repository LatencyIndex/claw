{-# LANGUAGE FlexibleInstances #-}

module Claw.Files (
    INode (..),
    EpochTime,
) where

import Claw.FilePath
import Claw.Internal.List (align, padL, padR)
import Claw.Internal.PrettyPrint (Pretty, pprint)
import Data.List (transpose)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToZonedTime)
import Numeric (showFFloat)
import System.Posix.Types (EpochTime)

-- | Show UTC time in given timezone.
showLocalTime :: TimeZone -> EpochTime -> String
showLocalTime timezone posixTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
  where
    utcTime = posixSecondsToUTCTime $ realToFrac posixTime :: UTCTime
    localTime = utcToZonedTime timezone utcTime

-- | Express the size of data in the most appropriate unit of bytes, as a (quantity, unit) pair.
inXBytes :: Int -> (Float, String)
inXBytes n = (ratio, prefix)
  where
    binarySi =
        [ (2 ^ (10 :: Int), "kB"),
          (2 ^ (20 :: Int), "MB"),
          (2 ^ (30 :: Int), "GB"),
          (2 ^ (40 :: Int), "TB"),
          (2 ^ (50 :: Int), "PB")
        ]
    canFit (qty, _siPrefix) = qty <= abs n
    (unit, prefix) = last $ (1, "B") : takeWhile canFit binarySi
    ratio = fromIntegral n / fromIntegral unit :: Float

-- | A filesystem entry, like a directory, file, link, block device, etc.
data INode = INode
    { -- | Only the name itself, without the path.
      name :: String,
      size :: Int,
      modified :: EpochTime,
      is_regular_file :: Bool,
      is_dir :: Bool,
      is_symlink :: Bool
    }
    deriving (Eq, Show)

instance Ord INode where
    compare = comparing (not . is_dir) <> comparing (getExt . name) <> comparing name

instance Pretty INode where
    pprint x = pprint [x]
instance Pretty [INode] where
    pprint xs = do
        timezone <- getCurrentTimeZone
        let
            modCell :: INode -> String
            modCell = showLocalTime timezone . modified
            sizeCell :: INode -> (String, String)
            sizeCell x
                | is_regular_file x = (showFFloat (Just 2) qty "", unit)
                | otherwise = ("", "")
              where
                (qty, unit) = inXBytes (size x)
            dirCell :: INode -> String
            dirCell x
                | is_regular_file x = "file"
                | is_dir x = "dir"
                | otherwise = "?"
            linkCell :: INode -> String
            linkCell x = if is_symlink x then "symlink" else ""
            extCell :: INode -> String
            extCell x = if is_regular_file x then getExt (name x) else ""
            alignR :: [String] -> [String]
            alignR = align (padL ' ')
            alignL :: [String] -> [String]
            alignL = align (padR ' ')

            modCol = alignL $ modCell <$> xs
            sizeCol = sizeCell <$> xs
            sizeQtyCol = alignR $ fst <$> sizeCol
            sizeUnitCol = alignL $ snd <$> sizeCol
            dirCol = alignL $ dirCell <$> xs
            linkCol = alignL $ linkCell <$> xs
            extCol = alignL $ extCell <$> xs
            nameCol = alignL $ name <$> xs

            cols = [modCol, sizeQtyCol, sizeUnitCol, dirCol, linkCol, extCol, nameCol]
            rows = transpose cols
        putStr $ unlines $ unwords <$> rows
