-- | Image conversion utilities. Requires imagemagick.
module Claw.Image (
    convert,
    countFrames,
    getSize,
    toWebpDestructive,
) where

import Claw.FS.Path (replaceExt)
import Claw.FS.Read (findUnique)
import Claw.Internal.Conversion (convertDestructive, convertSafely, findObsoleteBySize)
import Claw.Internal.Numeric (mulRII)
import Claw.Internal.Process (callProcessSafe, readProcessSafe)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both)
import Text.Read (readMaybe)

-- | Convert image to given format (derived from file extension), resolution, and quality.
-- Preserves aspect ratio if either x or y are 0. If both are 0, no resizing happens.
-- Quality is on 1-100 scale, where 100 is best. 0 keeps estimated image quality.
--     Chroma channels are not subsampled at quality >= 90
--     Details: http://www.imagemagick.org/script/command-line-options.php#quality
-- Format is determined by extension. Empty string keeps current format.
-- If the conversion was successful (i.e. a new file was created), returns the path to the new file.
convert :: (Int, Int) -> Int -> FilePath -> FilePath -> IO (Maybe FilePath)
convert (w, h) quality = convertSafely (convertUnsafe (w, h) quality)

-- Same as convert, but without checking if src and dst are the same or already exist.
-- Warning: Overwrites dst_file if it already exists, even if it is the same as src_file.
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
        callProcessSafe "magick" ([src] ++ resizeArgs ++ qualityArgs ++ [dst])

-- | Yields Nothing on failure.
countFrames :: FilePath -> IO (Maybe Int)
countFrames file = fmap (length . lines) <$> readProcessSafe "magick" ["identify", file] ""

-- | Return the image size as (width, height). Yields Nothing on failure.
getSize :: FilePath -> IO (Maybe (Int, Int))
getSize file = (>>= parseSize) <$> readProcessSafe "magick" ["identify", "-ping", "-format", "%w %h", file] ""
  where
    parseSize :: String -> Maybe (Int, Int)
    parseSize s = case mapMaybe readMaybe (words s) of
        [w, h] -> Just (w, h)
        _ -> Nothing

-- | Reduce the smallest dimension to <= x, keeping the aspect ratio.
-- Useful when resizing large, very non-square images, where clamping the larger dimension
-- could result in an unreasonably small smaller dimension.
_clampSmaller :: Int -> (Int, Int) -> (Int, Int)
_clampSmaller x (w, h) =
    let scale = min 1 $ fromIntegral x / fromIntegral (min w h) :: Double
     in both (mulRII scale) (w, h)

webpFindObsolete :: FilePath -> FilePath -> IO (Maybe FilePath)
webpFindObsolete = findObsoleteBySize minSizeAbs maxSizeRel where
    minSizeAbs = 2048 -- Images smaller than 2kB are probably broken.
    maxSizeRel = 0.9 -- webp supports lossless encoding, so no need to special-case .png

-- | Convert to .webp, returning the path to the converted image on success.
-- Attempts to preserve estimated image quality.
toWebp :: FilePath -> IO (Maybe FilePath)
toWebp src = do
    m_dst <- findUnique (replaceExt ".webp" src) -- Find free destination filename.
    case m_dst of
        Nothing -> return Nothing -- No free filename found - give up.
        Just dst -> convert (0, 0) 0 src dst

-- | Convert to .webp, trashing the original image on success.
toWebpDestructive :: FilePath -> IO ()
toWebpDestructive = convertDestructive toWebp webpFindObsolete
