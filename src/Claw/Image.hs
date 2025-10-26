-- | Image conversion utilities. Requires imagemagick.
module Claw.Image (
    convert,
    countFrames,
    getSize,
    toWebpInPlace,
) where

import Claw.FS.Path (getExt, replaceExt)
import Claw.FS.Read (findUnique, getFileSize)
import Claw.Internal.Conversion (convertInPlace, convertSafely)
import Claw.Internal.Numeric (mulRII)
import Control.Exception (PatternMatchFail (..), throw)
import Data.Tuple.Extra (both)
import System.Process (callProcess, readProcess)

-- | Convert image to given format (derived from file extension), resolution, and quality.
-- Preserves aspect ratio if either x or y are 0. If both are 0, no resizing happens.
-- Quality is on 1-100 scale, where 100 is best. 0 keeps estimated image quality.
--     Chroma channels are not subsampled at quality >= 90
--     Details: http://www.imagemagick.org/script/command-line-options.php#quality
-- Format is determined by extension. Empty string keeps current format.
-- Returns true if conversion was successful.
convert :: (Int, Int) -> Int -> FilePath -> FilePath -> IO (Maybe FilePath)
convert (w, h) quality = convertSafely (convertUnsafe (w, h) quality)

-- Same as convert, but without checking if src and dst are the same or already exist.
-- Warning: Overwrites dst_file if it already exists, even if it is the same as src_file.
-- Warning: May throw exception, e.g. for invalid paths.
convertUnsafe :: (Int, Int) -> Int -> FilePath -> FilePath -> IO ()
convertUnsafe (w, h) quality src dst =
    let
        doResize = w > 0 || h > 0
        doQuality = 0 < quality
        toResizeArg :: Int -> String
        toResizeArg x
            | x > 0 = show x
            | otherwise = ""
        resizeArgs = if doResize then ["-resize", toResizeArg w ++ "x" ++ toResizeArg h] else []
        qualityArgs = if doQuality then ["-quality", show quality] else []
     in
        callProcess "magick" ([src] ++ resizeArgs ++ qualityArgs ++ [dst])

-- | Warning: May throw exception, e.g. for invalid paths.
countFrames :: FilePath -> IO Int
-- 'identify' is part of imagemagick. Can also be invoked as 'magick identify'.
countFrames file = length . lines <$> readProcess "identify" [file] ""

-- | Return the image size as (width, height). Throws on failure.
-- Warning: May throw exception, e.g. for invalid paths.
getSize :: FilePath -> IO (Int, Int)
getSize file = do
    output <- readProcess "identify" ["-ping", "-format", "%w %h", file] ""
    case read <$> words output of
        [w, h] -> return (w, h)
        _ -> throw $ PatternMatchFail $ "Claw.Image.getSize: Failed to parse image dimensions of file " ++ file

-- | Reduce the smallest dimension to <= x, keeping the aspect ratio.
-- Useful when resizing large, very non-square images, where clamping the larger dimension
-- could result in an unreasonably small smaller dimension.
_clampSmaller :: Int -> (Int, Int) -> (Int, Int)
_clampSmaller x (w, h) =
    let scale = min 1 $ fromIntegral x / fromIntegral (min w h) :: Double
     in both (mulRII scale) (w, h)

-- Given an original image and its converted version, returns which can be discarded.
-- Assumes both paths point to existing files (and not directories).
obsoleteImage :: FilePath -> FilePath -> IO FilePath
obsoleteImage original converted = do
    orig_size <- getFileSize original
    conv_size <- getFileSize converted
    let
        -- Minimal savings required to prefer the converted image.
        threshold = case getExt original of
            ".png" -> 0.75 -- png is lossless so harder to justify replacing it.
            _ -> 0.9 :: Float
        small_enough = fromInteger conv_size <= threshold * fromInteger orig_size :: Bool
        -- If converted is less than 1kB, something probably went wrong.
        big_enough = 1024 < conv_size
    if small_enough && big_enough
        then
            return original
        else
            return converted

-- | Convert to 90 quality .webp, returning the path to the converted image on success.
toWebp :: FilePath -> IO (Maybe FilePath)
toWebp src = do
    m_dst <- findUnique (replaceExt ".webp" src) -- Find free destination filename.
    case m_dst of
        Nothing -> return Nothing -- No free filename found - give up.
        Just dst -> convert (0, 0) 90 src dst

-- | Convert to .webp, deleting the original image on success.
toWebpInPlace :: FilePath -> IO ()
toWebpInPlace = convertInPlace toWebp obsoleteImage
