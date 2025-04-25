module Claw.Video (
    -- | Video editing and conversion utilities. Requires @ffmpeg@ and @ffprobe@.
    getSubtitles,
    probe,
    cut,
    vconcat,
    reencode,
    reencodeMany,
) where

import Claw.FilePath
import Claw.Time (Duration, showDuration)
import System.Directory (removeFile)
import System.IO (openTempFile, hPutStr, hClose)
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

vconcat :: [FilePath] -> FilePath -> IO ()
vconcat files dst_basename = do
    -- ffmpeg docs on concatenating media: https://trac.ffmpeg.org/wiki/Concatenate
    (tempFile, tempHandle) <- openTempFile "." "tmp_ffmpeg_filelist_.txt"
    hPutStr tempHandle listFileContents
    hClose tempHandle
    callProcess "ffmpeg" ["-f", "concat", "-safe", "0", "-i", tempFile, "-c", "copy", dst_file]
    removeFile tempFile
    where
    dst_file = dst_basename <.> getExt (head files) :: String
    pathToLine path = "file '" ++ path ++ "'"
    listFileContents = unlines (pathToLine <$> files) :: String

-- | Re-encode video as h265-encoded mp4. Quality is between 0 (best) and 51 (worst). 24 is a good compromise.
reencode :: Int -> FilePath -> FilePath -> IO ()
reencode quality src_file dst_basename =
    let dst_file = dst_basename <.> ".mp4"
    in callProcess "ffmpeg" ["-i", src_file, "-vcodec", "libx265", "-crf", show quality, dst_file]

reencodeMany :: Int -> [FilePath] -> FilePath -> IO ()
reencodeMany quality srcFiles dstDir = mapM_ reencode' srcFiles where
    toDstPath srcPath = dstDir </> getBaseName srcPath
    reencode' srcFile = reencode quality srcFile (toDstPath srcFile)
