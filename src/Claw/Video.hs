-- | Video editing and conversion utilities. Requires @ffmpeg@ and @ffprobe@.
module Claw.Video (
    getSubtitles,
    probe,
    cut,
    vconcat,
    reencode,
    reencodeMany,
    toMp4Destructive,
) where

import Claw.FS.Path
import Claw.FS.Read (findUnique)
import Claw.Internal.Conversion (convertDestructive, convertSafely, findObsoleteBySize)
import Claw.Internal.Process (callProcessSafe)
import Claw.Time (Duration, showDuration)
import Control.Monad (when)
import System.Directory (removeFile)
import System.IO (hClose, hPutStr, openTempFile)
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
        callProcess "ffmpeg" [
            "-fflags", "+genpts", -- Generate presentation timestamps
            "-i", src_file,
            "-ss", showDuration t0, -- Start time
            "-t", showDuration dt, -- Clip duration
            "-vcodec", "copy",
            "-acodec", "copy",
            dst_file
        ]

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
     in convertUnsafe quality src_file dst_file

reencodeMany :: Int -> [FilePath] -> FilePath -> IO ()
reencodeMany quality srcFiles dstDir = mapM_ reencode' srcFiles
  where
    toDstPath srcPath = dstDir </> getBaseName srcPath
    reencode' srcFile = reencode quality srcFile (toDstPath srcFile)

convertUnsafe :: Int -> FilePath -> FilePath -> IO ()
convertUnsafe quality src dst =
    when (getExt dst == ".mp4") $
        callProcessSafe "ffmpeg" ["-i", src, "-vcodec", "libx265", "-crf", show quality, dst]

convert :: Int -> FilePath -> FilePath -> IO (Maybe FilePath)
convert quality = convertSafely (convertUnsafe quality)

mp4FindObsolete :: FilePath -> FilePath -> IO (Maybe FilePath)
mp4FindObsolete = findObsoleteBySize minSizeAbs maxSizeRel
  where
    minSizeAbs = 8096 -- Videos smaller than 8kB are probably broken.
    maxSizeRel = 0.9

toMp4 :: Int -> FilePath -> IO (Maybe FilePath)
toMp4 quality src = do
    m_dst <- findUnique (replaceExt ".mp4" src) -- Find free destination filename.
    case m_dst of
        Nothing -> return Nothing -- No free filename found - give up.
        Just dst -> convert quality src dst

-- | Convert to .mp4, trashing the original video on success.
toMp4Destructive :: Int -> FilePath -> IO ()
toMp4Destructive quality = convertDestructive (toMp4 quality) mp4FindObsolete
