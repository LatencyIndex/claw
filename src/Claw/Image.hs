module Claw.Image (
    -- | Image conversion utilities. Requires @imagemagick@.
    resize,
    getSize,
) where

import Claw.FilePath
import Control.Exception (PatternMatchFail (..), throw)
import System.Process (callProcess, readProcess)

{- | Resize image to given resolution and quality.
Preserves aspect ratio if either x or y are 0. If both are 0, no resizing happens.
Quality is on 1-100 scale, where 100 is best. Use 0 to keep estimated image quality.
    Details: http://www.imagemagick.org/script/command-line-options.php#quality
Returns the destination filepath.
-}
resize :: (Int, Int) -> Int -> FilePath -> FilePath -> IO FilePath
resize (x, y) quality dst_dir src_file =
    let dst_file = dst_dir </> getFileName src_file
        mkDim :: Int -> String
        mkDim n
            | n > 0 = show n
            | otherwise = ""
        doResize = x > 0 || y > 0
        resizeArgs = if doResize then ["-resize", mkDim x ++ "x" ++ mkDim y] else []
        doQuality = 0 < quality
        qualityArgs = if doQuality then ["-quality", show quality] else []
     in do
            callProcess "convert" (resizeArgs ++ qualityArgs ++ [src_file, dst_file])
            return dst_file

-- | Return the image size. Throws on failure.
getSize :: FilePath -> IO (Int, Int)
getSize file = do
    output <- readProcess "identify" ["-ping", "-format", "%w %h", file] ""
    case read <$> words output of
        [x, y] -> return (x, y)
        _ -> throw $ PatternMatchFail $ "Claw.Image.getSize: Failed to parse image dimensions of file " ++ file
