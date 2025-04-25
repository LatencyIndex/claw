module Claw.Image (
    -- | Image conversion utilities. Requires imagemagick.
    convert,
    getSize,
    toJpg,
    smallerJpg,
    smallerJpgs,
) where

import Claw.Control (mapBoth)
import Claw.FilePath (getBaseName, getExt, hasExt, (<.>), (</>))
import Claw.FileSystem (lsdir)
import Claw.Internal.Numeric (mulRII)
import Control.Exception (PatternMatchFail (..), throw)
import System.Process (callProcess, readProcess)

-- | Convert image to given format, resolution, and quality.
-- Preserves aspect ratio if either x or y are 0. If both are 0, no resizing happens.
-- Quality is on 1-100 scale, where 100 is best. Use 0 to keep estimated image quality.
--     Chroma channels are not subsampled at quality >= 90
--     Details: http://www.imagemagick.org/script/command-line-options.php#quality
-- Format is determined by extension. Empty string keeps current format.
-- Returns the destination filepath.
convert :: (Int, Int) -> Int -> String -> FilePath -> FilePath -> IO FilePath
convert (w, h) quality ext dst_dir src_file =
    let newExt = if null ext then getExt src_file else ext
        dst_file = dst_dir </> getBaseName src_file <.> newExt
        doResize = w > 0 || h > 0
        doQuality = 0 < quality
        toResizeArg :: Int -> String
        toResizeArg x
            | x > 0 = show x
            | otherwise = ""
        resizeArgs = if doResize then ["-resize", toResizeArg w ++ "x" ++ toResizeArg h] else []
        qualityArgs = if doQuality then ["-quality", show quality] else []
     in do
            callProcess "convert" (resizeArgs ++ qualityArgs ++ [src_file, dst_file])
            return dst_file

-- | Return the image size as (width, height). Throws on failure.
getSize :: FilePath -> IO (Int, Int)
getSize file = do
    output <- readProcess "identify" ["-ping", "-format", "%w %h", file] ""
    case read <$> words output of
        [w, h] -> return (w, h)
        _ -> throw $ PatternMatchFail $ "Claw.Image.getSize: Failed to parse image dimensions of file " ++ file

-- | Reduce the smallest dimension to <= x, keeping the aspect ratio.
-- Useful when resizing large, very non-square images, where clamping the larger dimension
-- could result in an unreasonably small smaller dimension.
clampSmaller :: Int -> (Int, Int) -> (Int, Int)
clampSmaller x (w, h) =
    let scale = min 1 $ fromIntegral x / fromIntegral (min w h) :: Double
     in mapBoth (mulRII scale) (w, h)

-- | Convert to 90 quality jpg. Arguments are dst_dir and src_file.
toJpg :: FilePath -> FilePath -> IO FilePath
toJpg = convert (0, 0) 90 "jpg"

-- | Convert to 90 quality jpg, and resize so smallest axis <= maxDim, preserving aspect ratio.
smallerJpg :: Int -> FilePath -> FilePath -> IO FilePath
smallerJpg maxDim dstDir srcFile = do
    newDims <- clampSmaller maxDim <$> getSize srcFile
    convert newDims 90 "jpg" dstDir srcFile

-- | Convert all files with the specified extension in srcDir to reduced-size jpgs in dstDir.
smallerJpgs :: String -> Int -> FilePath -> FilePath -> IO ()
smallerJpgs ext maxDim srcDir dstDir = do
    srcFiles <- filter (hasExt ext) <$> lsdir srcDir
    mapM_ (smallerJpg maxDim dstDir) srcFiles
