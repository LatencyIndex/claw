module Claw.Video (
    -- | Video conversion utilities. Requires @ffmpeg@ and @ffprobe@.
    getSubtitles,
    probe,
    cut,
) where

import Claw.FilePath
import Claw.Time (Duration, showDuration)
import System.Process (callProcess)

-- | Extract n-th subtitle track to destination directory.
-- Output subtitle type is determined by extension.
-- If it does not match the input type, conversion errors or data loss may occur.
-- If src_file is @src_dir/file.mkv@, destination filename is @dst_dir/file.ext@
-- Returns the destination filepath.
getSubtitles :: Int -> String -> FilePath -> FilePath -> IO FilePath
getSubtitles n ext dst_dir src_file =
    let dst_file = dst_dir </> getBaseName src_file <.> ext
     in do
            callProcess "ffmpeg" ["-i", src_file, "-map", "0:s:" ++ show n, dst_file]
            return dst_file

-- | Print information about the media file.
probe :: FilePath -> IO ()
probe file = callProcess "ffprobe" [file]

-- | Create a new video from the given time interval.
cut :: Duration -> Duration -> FilePath -> FilePath -> IO ()
cut t0 t1 src_file dst_basename =
    let
        dst_file = dst_basename <.> getExt src_file
        dt = t1 - t0
     in
        callProcess "ffmpeg" ["-i", src_file, "-ss", showDuration t0, "-t", showDuration dt, "-vcodec", "copy", "-acodec", "copy", dst_file]
