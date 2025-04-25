module Claw.Video (
    -- | Video conversion utilities. Requires @ffmpeg@ and @ffprobe@.
    getSubtitles,
    probe,
) where

import Claw.FilePath
import System.Process (callProcess)

{- | Extract n-th subtitle track to destination directory.
Output subtitle type is determined by extension.
If it does not match the input type, conversion errors or data loss may occur.
If src_file is @src_dir/file.mkv@, destination filename is @dst_dir/file.ext@
Returns the destination filepath.
-}
getSubtitles :: Int -> String -> FilePath -> FilePath -> IO FilePath
getSubtitles n ext dst_dir src_file =
    let dst_file = dst_dir </> getBaseName src_file <.> ext
     in do
            callProcess "ffmpeg" ["-i", src_file, "-map", "0:s:" ++ show n, dst_file]
            return dst_file

-- | Print information about the media file.
probe :: FilePath -> IO ()
probe file = callProcess "ffprobe" [file]
